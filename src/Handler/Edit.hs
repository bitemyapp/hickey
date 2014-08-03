{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( edit
    , commit
    , preview) where

import Control.Applicative ((<$>))
import Control.Monad (mapM_, when, void)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, strip)
import Data.Text.Encoding (decodeUtf8)
import Templates
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Types
import Routes
import Store
import Web.Seacat (FileInfo(..), Handler, MkUrl, htmlResponse, redirect, param')
import Web.Seacat.RequestHandler (htmlUrlResponse)
import Handler.Error

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Te
import qualified Templates.Utils as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad.IO.Class (liftIO)

-- |Display an edit form for a page.
edit :: WikiPage -> Handler Sitemap
edit wp = do
  wikiPage <- getStoredFile  $ pageFileName wp
  current  <- latestRevision $ pageFileName wp

  htmlUrlResponse $ renderEditPage wp current Nothing $ fromMaybe "" wikiPage

-- |Save an edit, possibly resolving a merge in the process.
commit :: WikiPage -> Handler Sitemap
commit wp = do
  -- Grab the required fields. For all we care, an empty value is the
  -- same as no value at all - so just default them.
  markup   <- strip <$> param' "markup" ""
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
    current <- latestRevision $ pageFileName wp
    case revid of
      "new" -> if isJust current
              -- Show the edit page again, indicating that the page
              -- was created in the intervening time.
              then do
                wikiPage <- fromJust <$> getStoredFile (pageFileName wp)
                conflict $ "<<<\n" <> wikiPage <> "\n===\n" <> markup <> "\n>>>"

              -- Commit everything
              else do
                  create (pageFileName wp) who desc markup
                  redirect $ View wp Nothing

      _ -> case toRevision revid of
            Just r -> if Just r /= current
                     -- Show the edit page again, with the merge
                     -- conflict in the edit box.
                     then do
                       mergeinfo <- (\(Left x) -> x) <$> save (pageFileName wp) r who desc markup
                       conflict $ mergText mergeinfo

                     -- Commit everything
                     else do
                       void $ save (pageFileName wp) r who desc markup
                       redirect $ View wp Nothing

            -- The revision ID is bad.
            _ -> badForm "The revid field is invalid. This should never happen unles you touch it manually." markup

-- |Render a preview of some posted markup.
preview :: Handler Sitemap
preview = param' "markup" "" >>= htmlResponse . renderBareMarkup

-----

-- |Display a page with an area for displaying a preview, and a box
-- for editing the markup.
renderEditPage :: WikiPage -> Maybe Revision
               -> Maybe Text
               -- ^Error message
               -> Text -> MkUrl Sitemap -> Html
renderEditPage wp r msg md = renderHtmlPage "Edit" $ \mkurl -> do
  -- Start off with a nice cheerful error message.
  when (isJust msg) $
    H.div ! class_ (textValue "error") $ toHtml (fromJust msg)

  -- Inside here lives a rendered preview, we'll have some javascript pull it in
  H.div ! A.id (textValue "preview") $ T.empty

  -- And here is the edit form
  H.form ! method (textValue "post") ! action (textValue $ flip mkurl [] $ Edit wp) ! enctype (textValue "multipart/form-data") $
    fieldset $
      ol ! A.id (textValue "mainform") $ do
        li $
          textarea ! name (textValue "markup") ! required (textValue "required") $ toHtml md
        li $
          T.input' "Your Handle:" "who" "text"
        li $
          T.input' "Edit Summary:" "desc" "text"
        li $
          H.input ! type_ (textValue "submit") ! value (textValue "Save Changes")

        -- We also keep track of the revision, to allow for merging.
        li ! A.style (textValue "display: none") $
          let revid val = H.input ! name (textValue "revid") ! type_ (textValue "text") ! value (textValue val) ! required (textValue "required")
          in case r of
               Just rev -> revid $ revisionTextId rev
               Nothing  -> revid "new"
