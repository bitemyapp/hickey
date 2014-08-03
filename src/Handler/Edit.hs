{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( edit
    , commit
    , preview) where

import Control.Applicative ((<$>))
import Control.Monad (mapM_, void)
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
import Web.Seacat (FileInfo(..), Handler, MkUrl, htmlResponse, redirect, param, param', files)
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

  htmlUrlResponse $ renderEditPage wp current $ fromMaybe "" wikiPage

-- |Save an edit, possibly uploading some new attachments in the
-- process.
--
-- TODO: If there's something objectional with the edit, display the
-- edit form again, rather than just an error page.
commit :: WikiPage -> Handler Sitemap
commit wp = do
  -- Grab the required fields. For all we care, an empty value is the
  -- same as no value at all - so just default them.
  markup   <- strip <$> param' "markup" ""
  who      <- strip <$> param' "who"    ""
  desc     <- strip <$> param' "desc"   ""
  revid    <- strip <$> param' "revid"  ""

  if any ((==0) . Te.length) [markup, who, desc, revid]
  -- Nooo, a bad form!
  then htmlUrlResponse renderBadForm
  -- All required fields are present
  else do
    thefiles <- restrict <$> files
    if not (null thefiles) && not (all (isFileName . decodeUtf8 . fileName . snd) thefiles)
    -- Nooo, bad filenames!
    then htmlUrlResponse renderBadFiles
    else do
      -- Check the file hasn't been modified since we started.
      current <- latestRevision $ pageFileName wp
      case revid of
        "new" -> if isJust current
                -- Show the edit page again, indicating that the page
                -- was created in the intervening time.
                then do
                  wikiPage <- fromJust <$> getStoredFile (pageFileName wp)
                  conflict wp current $ "<<<\n" <> wikiPage <> "\n===\n" <> markup <> "\n>>>"

                -- Commit everything
                else do
                  createPage who desc markup thefiles
                  redirect $ View wp Nothing

        _ -> case toRevision revid of
              Just r -> if Just r /= current
                       -- Show the edit page again, with the merge
                       -- conflict in the edit box.
                       then do
                         mergeinfo <- (\(Left x) -> x) <$> save (pageFileName wp) r who desc markup
                         conflict wp current $ mergText mergeinfo

                       -- Commit everything
                       else do
                         updatePage who desc markup r thefiles
                         redirect $ View wp Nothing

              -- The revision ID is bad.
              _ -> htmlUrlResponse renderBadForm

  where createPage who desc markup thefiles = do
          uploadAll wp who desc thefiles
          create (pageFileName wp) who desc markup

        updatePage who desc markup rev thefiles = do
          uploadAll wp who desc thefiles
          void $ save (pageFileName wp) rev who desc markup

        uploadAll wp who desc = mapM_ $ \(_, file) -> overwrite wp (tofn file) who desc $ fileContent file

        conflict wp r merg = htmlUrlResponse $ renderEditPage wp r merg

        tofn file = fromJust . toFileName . decodeUtf8 $ fileName file

        restrict = filter $ not . B.null . fileContent . snd

-- |Render a preview of some posted markup.
preview :: Handler Sitemap
preview = param "markup" >>= htmlResponse . renderBareMarkup . fromMaybe ""

-----

-- |Display a page with an area for displaying a preview, and a box
-- for editing the markup.
renderEditPage :: WikiPage -> Maybe Revision -> Text -> MkUrl Sitemap -> Html
renderEditPage wp r md = renderHtmlPage "Edit" $ \mkurl -> do
  -- Inside here lives a rendered preview, we'll have some javascript pull it in
  H.div ! A.id (textValue "preview") $ T.empty

  -- And here is the edit form
  H.form ! method (textValue "post") ! action (textValue $ flip mkurl [] $ Edit wp) ! enctype (textValue "multipart/form-data") $
    fieldset $ do
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

      hr

      -- Now we have new attachments. Javascript can add on new
      -- entries.
      ol ! A.id (textValue "fileform") $
       li $
         input ! name (textValue "file0") ! type_ (textValue "file")

      -- Don't say you haven't been warned…
      p ! class_ (textValue "notice") $ T.toHtml "Filenames must be of the form [a-zA-Z0-9]+.[a-zA-Z0-9]+. If a file is uploaded with the same name as a currently existing file attached to this page, the existing file will be replaced."
      
