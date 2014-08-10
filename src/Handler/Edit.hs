{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( edit
    , commit) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, strip, replace)
import Handler.Utils
import Routes
import Store
import Store.Paths
import Templates
import Templates.Utils (form', input', toHtml, empty)
import Text.Blaze.Html5 (Html, (!), ul, li, a, textarea, input)
import Text.Blaze.Html5.Attributes (href, style, name, type_, value, required, class_)
import Text.Blaze.Internal (textValue)
import Types
import Web.Seacat (Handler, MkUrl, redirect, param')
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Data.Text                   as Te
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- |Display an edit form for a page.
edit :: WikiPage -> Handler Sitemap
edit wp = protect wp $ do
  wikiPage <- getStoredFile  $ wikipage wp
  current  <- latestRevision $ wikipage wp

  htmlUrlResponse $ renderEditPage wp current Nothing $ fromMaybe "" wikiPage

-- |Save an edit, possibly resolving a merge in the process.
commit :: WikiPage -> Handler Sitemap
commit wp = protect wp $ do
  -- Grab the required fields. For all we care, an empty value is the
  -- same as no value at all - so just default them.
  markup   <- replace "\r\n" "\n" . strip <$> param' "markup" ""
  who      <- strip <$> param' "who"    ""
  desc     <- strip <$> param' "desc"   ""
  revid    <- strip <$> param' "revid"  ""

  -- Error handlers
  let badForm reason md = htmlUrlResponse $ renderEditPage wp (toRevision revid) (Just reason) md
  let conflict = badForm "Someone else has edited this page. Please resolve the merge before continuing."

  if any ((==0) . Te.length) [markup, who, desc, revid]
  -- Nooo, a bad form!
  then badForm "One or more mandatory fields are missing" markup
  -- All required fields are present
  else do
    -- Check the file hasn't been modified since we started.
    current <- latestRevision $ wikipage wp
    case revid of
      "new" -> if isJust current
              -- Show the edit page again, indicating that the page
              -- was created in the intervening time.
              then do
                wikiPage <- fromJust <$> getStoredFile (wikipage wp)
                conflict $ "<<<\n" <> wikiPage <> "\n===\n" <> markup <> "\n>>>"

              -- Commit everything
              else do
                  create (wikipage wp) who desc markup
                  redirect $ View wp Nothing

      _ -> case toRevision revid of
            Just r -> if Just r /= current
                     -- Show the edit page again, with the merge
                     -- conflict in the edit box.
                     then do
                       mergeinfo <- (\(Left x) -> x) <$> save (wikipage wp) r who desc markup
                       conflict $ mergText mergeinfo

                     -- Commit everything
                     else do
                       void $ save (wikipage wp) r who desc markup
                       redirect $ View wp Nothing

            -- The revision ID is bad.
            _ -> badForm "The revid field is invalid. This should never happen unles you touch it manually." markup

-----

-- |Display a page with an area for displaying a preview, and a box
-- for editing the markup.
renderEditPage :: WikiPage -> Maybe Revision
               -> Maybe Text
               -- ^Error message
               -> Text -> MkUrl Sitemap -> Html
renderEditPage wp r msg md = renderHtmlPage (const (wp, False) <$> r) ("Edit " <> pageNiceName wp) form
    where form   = form' target inputs "Save Changes" msg Nothing (Just before) (Just after)
          target = Edit wp
          inputs = [ (textarea ! name "markup" ! required "required" $ toHtml md, Nothing)
                   , (input' "Your Handle:" "who" "text", Nothing)
                   , (input' "Edit Summary:" "desc" "text", Nothing)
                   ]

          before = do
            -- Fancy tabs to change between edit and preview.
            ul ! class_ "tabs" $ do
              li $ a ! A.id "editLnk" ! href "javascript:changeToEditTab()" ! style "font-weight: bold" $ toHtml "Edit"
              li $ a ! A.id "previewLnk" ! href "javascript:changeToPreviewTab()" $ toHtml "Preview"

            -- Inside here lives a rendered preview, we'll have some javascript pull it in
            H.div ! A.id "preview" ! style "display:none" $ empty

          -- Keep track of the revision, to allow for merging
          after = li ! style "display: none" $
                    let revid val = input ! name "revid" ! type_ "text" ! value (textValue val) ! required "required"
                    in case r of
                         Just rev -> revid $ revisionTextId rev
                         Nothing  -> revid "new"
