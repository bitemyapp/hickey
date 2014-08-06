-- |Functions for writing to the store.
module Store.Write where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (mergeRevision, mergeConflicts, mergeText)
import Data.Text (Text, pack, unpack)
import Store.Types
import Store.Utils
import Types
import Web.Routes (PathInfo)
import Web.Seacat (RequestProcessor)

import qualified Data.FileStore as FS

-- *Creating new files

-- |Create a new file.
create :: PathInfo r
       => FilePath
       -> Text
       -- ^The author
       -> Text
       -- ^The change description
       -> Text
       -- ^The new contents
       -> RequestProcessor r ()
create fp who desc txt = withFileStore $ \fs -> createFS fs fp who desc txt

-- |Alternative version of `create` which takes a file store.
createFS :: MonadIO m => FileStore -> FilePath -> Text -> Text -> Text -> m ()
createFS fs fp who desc txt = liftIO $ FS.create fs fp author description txt
  where author      = toStoreAuthor who
        description = unpack desc

-- *Safe updates

-- |Update a file if it has not been touched since the given
-- revision. If it has been, merge conflict information is returned
-- instead.
save :: PathInfo r
     => FilePath
     -> Revision
     -> Text
     -- ^The author
     -> Text
     -- ^The change description
     -> Text
     -- ^The new contents
     -> RequestProcessor r (Either Merge ())
save fp r who desc txt = withFileStore $ \fs -> saveFS fs fp r who desc txt

-- |Alternative version of `save` which takes a file store.
saveFS :: MonadIO m => FileStore -> FilePath -> Revision -> Text -> Text -> Text -> m (Either Merge ())
saveFS fs fp r who desc txt = do
  res <- liftIO $ FS.modify fs fp rid author description txt

  return $
    case res of
      Left merge -> Left Merge { mergRevision  = fromStoreRevision $ mergeRevision merge
                              , mergConflicts = mergeConflicts merge
                              , mergText      = pack $ mergeText merge
                              }
      _          -> Right ()

  where rid         = toStoreRevisionId r
        author      = toStoreAuthor who
        description = unpack desc

-- *Unsafe updates

-- |Overwrite a file completely.
overwrite :: PathInfo r
          => FilePath
          -> Text
          -- ^The author
          -> Text
          -- ^The change description
          -> ByteString
          -- ^The new contents
          -> RequestProcessor r ()
overwrite fp who desc cntnts = withFileStore $ \fs -> overwriteFS fs fp who desc cntnts

-- |Alternative version of `overwrite` which takes a file store.
overwriteFS :: MonadIO m => FileStore -> FilePath -> Text -> Text -> ByteString -> m ()
overwriteFS fs fp who desc cntnts = liftIO $ FS.save fs fp author description cntnts
    where author      = toStoreAuthor who
          description = unpack desc
