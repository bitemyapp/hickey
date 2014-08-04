{-# LANGUAGE OverloadedStrings #-}

module Handler.Static
    ( file
    , fileAtRevision
    , files
    , static) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Types.Status (ok200, notFound404)
import Network.Mime (defaultMimeLookup)
import Network.Wai (responseLBS)
import Routes
import Store
import System.FilePath.Posix (joinPath)
import Types
import Web.Seacat (Handler, conf')
import Web.Seacat.RequestHandler (textResponse')

-- |Display a file as it exists currently.
file :: WikiPage -> FileName -> Handler Sitemap
file wp fn = fileAt fpath Nothing $ pageTextName wp <> " has no file " <> fileTextName fn
    where fpath = joinPath [unpack $ pageTextName wp <> "-files", unpack $ fileTextName fn]

-- |Display a file at the given revision.
fileAtRevision :: WikiPage -> FileName -> Revision -> Handler Sitemap
fileAtRevision wp fn r = fileAt fpath (Just r) $ pageTextName wp <> " at " <> revisionShortId r <> " has no file " <> fileTextName fn
    where fpath = joinPath [unpack $ pageTextName wp <> "-files", unpack $ fileTextName fn]

-- |Display the list of files as it is now.
files :: WikiPage -> Handler Sitemap
files wp = undefined

-- |Display a static file, if it exists.
static :: FileName -> Handler Sitemap
static fn = fileAt fpath Nothing $ "Cannot find " <> fileTextName fn
    where fpath = joinPath ["static", unpack $ fileTextName fn]

-- |Display a file from the store, with the given content type.
fileAt :: FilePath
       -- ^Path relative to the root of the store.
       -> Maybe Revision
       -- ^Revision to take (Nothing for HEAD)
       -> Text
       -- ^Message to display on 404
       -> Handler Sitemap
fileAt fp r err = do
  contents <- case r of
               Just rev -> getStoredBinaryAt fp rev
               Nothing  -> getStoredBinary fp

  case contents of
    Just cntnts -> return $ responseLBS ok200 [("Content-Type", defaultMimeLookup $ pack fp)] cntnts
    Nothing     -> textResponse' notFound404 err
