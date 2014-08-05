-- |Functions for retrieving files from the store.
module Store.Retrieve
    ( -- *Retrieving HEAD
      getStoredFile
    , getStoredFileFS

      -- *Retrieving old revisions
    , getStoredFileAt
    , getStoredFileAtFS
    , getStoredFileAt'
    , getStoredFileAtFS'

    -- *Existence checking
    , doesFileExist
    , doesFileExistFS

    -- *Directory listing
    , listFiles
    , listFilesFS

    -- *Plugins
    , getPlugins
    , getPluginsFS
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (Resource(..), directory, retrieve)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Store.Paths
import Store.Types
import Store.Utils
import System.Directory (canonicalizePath)
import System.FilePath.Posix (joinPath)
import Types
import Web.Routes (PathInfo)
import Web.Seacat (RequestProcessor, conf')

-- |Get the contents of the HEAD of a file, if it exists.
getStoredFile :: (PathInfo r, Contents c) => FilePath -> RequestProcessor r (Maybe c)
getStoredFile fp = withFileStore $ \fs -> getStoredFileFS fs fp

-- |Alternative version of `getStoredFile` which takes a file store.
getStoredFileFS :: (Contents c, MonadIO m) => FileStore -> FilePath -> m (Maybe c)
getStoredFileFS fs fp = getStoredFileAtFS' fs fp Nothing

-- |Get the contents of a specific revision of a file. If the file
-- didn't exist at that revision, return Nothing.
getStoredFileAt :: (Contents c, PathInfo r) => FilePath -> Revision -> RequestProcessor r (Maybe c)
getStoredFileAt fp r = withFileStore $ \fs -> getStoredFileAtFS fs fp r

-- |Alternative version of `getStoredFileAt` which takes a file store.
getStoredFileAtFS :: (Contents c, MonadIO m) => FileStore -> FilePath -> Revision -> m (Maybe c)
getStoredFileAtFS fs fp = getStoredFileAtFS' fs fp . Just

-- |Get the contents of a file at the given revision, or the latest if
-- the revision is unspecified.
getStoredFileAt' :: (Contents c, PathInfo r) => FilePath -> Maybe Revision -> RequestProcessor r (Maybe c)
getStoredFileAt' fp r = withFileStore $ \fs -> getStoredFileAtFS' fs fp r

-- |Alternative version of `getStoredFileAt'` which takes a file store.
getStoredFileAtFS' :: (Contents c, MonadIO m) => FileStore -> FilePath -> Maybe Revision -> m (Maybe c)
getStoredFileAtFS' fs fp r = liftIO $ contents `onStoreExc` return Nothing
    where contents = Just <$> retrieve fs fp (unpack . revisionTextId <$> r)

-- |Check if a file exists at the moment
doesFileExist :: PathInfo r => FilePath -> RequestProcessor r Bool
doesFileExist fp = withFileStore $ flip doesFileExistFS fp

-- |Alternative version of `doesFileExist` which takes a file store.
doesFileExistFS :: MonadIO m => FileStore -> FilePath -> m Bool
doesFileExistFS fs fp = liftM (isJust :: Maybe ByteString -> Bool) $ getStoredFileFS fs fp

-- |List the files in a directory.
listFiles :: PathInfo r => FilePath -> RequestProcessor r [FilePath]
listFiles fp = withFileStore $ flip listFilesFS fp

-- |Alternative version of `listFiles` which takes a file store.
listFilesFS :: MonadIO m => FileStore -> FilePath -> m [FilePath]
listFilesFS fs fp = liftIO $ list `onStoreExc` return []
    where list = concatMap file <$> directory fs fp 
          file (FSFile f) = [f]
          file _ = []

-- |Get the list of plugins.
getPlugins :: PathInfo r => RequestProcessor r [Plugin]
getPlugins = do
  gpath <- conf' "git" "path"
  pluginpath <- liftIO $ canonicalizePath $ joinPath [gpath, plugindir]
  withFileStore $ \fs -> getPluginsFS fs pluginpath

-- |Alternative version of `getPlugins` which takes a file store and
-- plugin path.
getPluginsFS :: MonadIO m => FileStore -> FilePath -> m [Plugin]
getPluginsFS fs pluginpath = liftIO $ plugins `onStoreExc` return []
    where plugins = concatMap plug <$> directory fs plugindir
          plug (FSFile file) = [(file, joinPath [pluginpath, file])]
          plug _ = []
