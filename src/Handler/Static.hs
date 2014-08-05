{-# LANGUAGE OverloadedStrings #-}

module Handler.Static
    ( file
    , fileAtRevision
    , files
    , upload
    , Handler.Static.static) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, strip)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Status (ok200, notFound404)
import Network.Mime (defaultMimeLookup)
import Network.Wai (responseLBS)
import Routes
import Store
import Store.Paths
import Templates (renderHtmlPage)
import Templates.Utils (link, input', form)
import Text.Blaze.Html5 (Html, ul, li)
import Types
import Web.Seacat (Handler, MkUrl, redirect, param', fileName, fileContent)
import Web.Seacat.RequestHandler (htmlUrlResponse, textResponse')

import qualified Data.ByteString.Lazy as B
import qualified Store.Paths          as P
import qualified Web.Seacat           as S

-- |Display a file as it exists currently.
file :: WikiPage -> FileName -> Handler Sitemap
file wp fn = fileAt (attachment wp fn) Nothing $ pageTextName wp <> " has no file " <> fileTextName fn

-- |Display a file at the given revision.
fileAtRevision :: WikiPage -> FileName -> Revision -> Handler Sitemap
fileAtRevision wp fn r = fileAt (attachment wp fn) (Just r) $ pageTextName wp <> " at " <> revisionShortId r <> " has no file " <> fileTextName fn

-- |Display the list of files as it is now.
files :: WikiPage -> Handler Sitemap
files wp = do
  allfiles <- listFiles $ attachmentdir wp
  htmlUrlResponse $ renderHtmlPage thetitle $ thehtml allfiles

  where thetitle = pageTextName wp <> " files"

        thehtml allfiles mkurl = do
          ul $ mapM_ (fileHtml mkurl) allfiles
          renderUpload wp Nothing mkurl

        fileHtml mkurl fle = let pfle = pack fle
                             in li $ link mkurl pfle $ File wp (fromJust $ toFileName pfle) Nothing

-- |Upload a file.
upload :: WikiPage -> Handler Sitemap
upload wp = do
  -- Get and verify the required fields
  allfiles <- S.files
  who      <- strip <$> param' "who"  ""
  desc     <- strip <$> param' "desc" ""

  -- Check stuff
  if who == "" || desc == ""
  then htmlUrlResponse $ renderHtmlPage "Upload" $ renderUpload wp $ Just "A required field was missing or invalid."
  else case restrict allfiles of
         ((_, fle):_) -> overwrite (attachment wp $ fname fle) who desc (fileContent fle) >> redirect (Files wp)
         _            -> htmlUrlResponse $ renderHtmlPage "Upload" $ renderUpload wp $ Just "You need to specify a file."

  where restrict = filter $ not . B.null . fileContent . snd
        fname fle = fromJust . toFileName . decodeUtf8 $ fileName fle

-- |Display a static file, if it exists.
static :: FileName -> Handler Sitemap
static fn = fileAt (P.static fn) Nothing $ "Cannot find " <> fileTextName fn

-----

-- |Display a file from the store, with the given content type.
fileAt :: FilePath
       -- ^Path relative to the root of the store.
       -> Maybe Revision
       -- ^Revision to take (Nothing for HEAD)
       -> Text
       -- ^Message to display on 404
       -> Handler Sitemap
fileAt fp r err = do
  contents <- getStoredFileAt' fp r

  case contents of
    Just cntnts -> return $ responseLBS ok200 [("Content-Type", defaultMimeLookup $ pack fp)] cntnts
    Nothing     -> textResponse' notFound404 err

-- |Render a file upload form, with a possible error message.
renderUpload :: WikiPage -> Maybe Text -> MkUrl Sitemap -> Html
renderUpload wp msg = form target inputs "Upload File" msg (Just help)
    where target = Files wp
          inputs = [ (input' "File" "file" "file", Nothing)
                   , (input' "Your Handle:" "who" "text", Nothing)
                   , (input' "Edit Summary:" "desc" "text", Nothing)
                   ]
          help = "Filenames must be of the form /[a-zA-Z0-9_-]+.[a-zA-Z0-9_-]+/. If you upload a file with the same name as another, the original file will be replaced."
