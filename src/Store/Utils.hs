-- |Utility functions for dealing with data stored.
module Store.Utils where

import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.FileStore.Git (gitFileStore)
import Data.FileStore.Types (FileStoreError)
import Store.Types
import Web.Routes (PathInfo)
import Web.Seacat (RequestProcessor, conf')

-- *FileStore access

-- |Run a function that takes a reference to the filestore.
withFileStore :: PathInfo r => (FileStore -> IO a) -> RequestProcessor r a
withFileStore f = do
  git <- gitFileStore <$> conf' "git" "path"
  liftIO $ f git

-- |Get the file store
getFileStore :: PathInfo r => RequestProcessor r FileStore
getFileStore = withFileStore return

-- *Exception handling

-- |Catch a FileStore exception
catchStoreExc :: IO a -> (FileStoreError -> IO a) -> IO a
catchStoreExc = catch

-- |Return the second argument if a FileStore exception is caught.
onStoreExc :: IO a -> IO a -> IO a
onStoreExc dangerous = catchStoreExc dangerous . const
