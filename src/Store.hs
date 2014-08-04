{-# LANGUAGE OverloadedStrings #-}

module Store
    ( Commit
    , commitRevision
    , commitTime
    , commitAuthor
    , commitMessage

    , Differences
    , Diff(..)

    , Merge
    , mergRevision
    , mergConflicts
    , mergText

    , getFileStore

    , getPlugins
    , getPluginsFS

    , Contents
    , getStoredFile
    , getStoredFileFS
    , getStoredFileAt
    , getStoredFileAtFS
    , getStoredFileAt'
    , getStoredFileAtFS'

    , getHistory
    , getHistoryFS
    , getDiff
    , getDiffFS

    , latestRevision
    , latestRevisionFS
    , doesFileExist
    , doesFileExistFS

    , create
    , createFS
    , Store.save
    , saveFS
    , overwrite
    , overwriteFS
    ) where

import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (Diff(..), toByteString, fromByteString, modify, revId, revDateTime, revAuthor, revDescription, diff)
import Data.FileStore.Git (gitFileStore)
import Data.FileStore.Types (Author(..), Contents, FileStore(..), FileStoreError, MergeInfo(..), Resource(..), RevisionId, TimeRange(..), UTCTime)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack, unpack)
import Routes
import Store.Paths
import System.Directory (canonicalizePath)
import System.FilePath.Posix (joinPath)
import Types
import Web.Seacat (RequestProcessor, conf')

import qualified Data.FileStore as FS

data Commit = Commit { commitRevision :: Revision
                     , commitUnderlyingRevision :: RevisionId
                     , commitTime     :: UTCTime
                     , commitAuthor   :: Text
                     , commitMessage  :: Text
                     }

type Differences = [Diff [Text]]

data Merge = Merge { mergRevision  :: Revision
                   , mergUnderlyingRevision :: RevisionId
                   , mergConflicts :: Bool
                   , mergText      :: Text
                   }

instance Contents Text where
    fromByteString = pack . fromByteString
    toByteString   = toByteString . unpack

-- |Run a function that takes a reference to the filestore.
withFileStore :: (FileStore -> IO a) -> RequestProcessor Sitemap a
withFileStore f = do
  git <- gitFileStore <$> conf' "git" "path"
  liftIO $ f git

-- |Convert a Revision to a RevisionId.
revisionId :: Revision -> RevisionId
revisionId = unpack . revisionTextId

-- |Catch a FileStore exception
catchStoreExc :: IO a -> (FileStoreError -> IO a) -> IO a
catchStoreExc = catch

-- |Return the second argument if a FileStore exception is caught.
onStoreExc :: IO a -> IO a -> IO a
onStoreExc dangerous = catchStoreExc dangerous . const

-----

-- |Get the file store
getFileStore :: RequestProcessor Sitemap FileStore
getFileStore = withFileStore return

-- |Get the list of plugins.
getPlugins :: RequestProcessor Sitemap [Plugin]
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

-- |Get the contents of the HEAD of a file, if it exists.
getStoredFile :: Contents c => FilePath -> RequestProcessor Sitemap (Maybe c)
getStoredFile fp = withFileStore $ \fs -> getStoredFileFS fs fp

-- |Alternative version of `getStoredFile` which takes a file store.
getStoredFileFS :: (Contents c, MonadIO m) => FileStore -> FilePath -> m (Maybe c)
getStoredFileFS fs fp = getStoredFileAtFS' fs fp Nothing

-- |Get the contents of a specific revision of a file. If the file
-- didn't exist at that revision, return Nothing.
getStoredFileAt :: Contents c => FilePath -> Revision -> RequestProcessor Sitemap (Maybe c)
getStoredFileAt fp r = withFileStore $ \fs -> getStoredFileAtFS fs fp r

-- |Alternative version of `getStoredFileAt` which takes a file store.
getStoredFileAtFS :: (Contents c, MonadIO m) => FileStore -> FilePath -> Revision -> m (Maybe c)
getStoredFileAtFS fs fp = getStoredFileAtFS' fs fp . Just

-- |Get the contents of a file at the given revision, or the latest if
-- the revision is unspecified.
getStoredFileAt' :: Contents c => FilePath -> Maybe Revision -> RequestProcessor Sitemap (Maybe c)
getStoredFileAt' fp r = withFileStore $ \fs -> getStoredFileAtFS' fs fp r

-- |Alternative version of `getStoredFileAt'` which takes a file store.
getStoredFileAtFS' :: (Contents c, MonadIO m) => FileStore -> FilePath -> Maybe Revision -> m (Maybe c)
getStoredFileAtFS' fs fp r = liftIO $ contents `onStoreExc` return Nothing
    where contents = Just <$> retrieve fs fp (unpack . revisionTextId <$> r)

-- |Get the history of a file, if it exists, as a list of commits.
getHistory ::  FilePath -> RequestProcessor Sitemap (Maybe [Commit])
getHistory fp = withFileStore $ \fs -> getHistoryFS fs fp

-- |Alternative version of `getHistory` which takes a file store.
getHistoryFS :: MonadIO m => FileStore -> FilePath -> m (Maybe [Commit])
getHistoryFS fs fp = liftIO $ hist `onStoreExc` return Nothing
    where hist = (Just . map toCommit) <$> history fs [fp] (TimeRange Nothing Nothing) Nothing
          toCommit r = Commit { commitRevision           = fromJust . toRevision . pack $ revId r
                              , commitUnderlyingRevision = revId r
                              , commitTime               = revDateTime r
                              , commitAuthor             = pack . authorName $ revAuthor r
                              , commitMessage            = pack $ revDescription r
                              }

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff :: FilePath -> Revision -> Revision -> RequestProcessor Sitemap (Maybe Differences)
getDiff fp r1 r2 = withFileStore $ \fs -> getDiffFS fs fp r1 r2

-- |Alternative version of `getDiff` which takes a file store.
getDiffFS :: MonadIO m => FileStore -> FilePath -> Revision -> Revision -> m (Maybe Differences)
getDiffFS fs fp r1 r2 = liftIO $ dif `onStoreExc` return Nothing
    where dif = Just . map toText <$> diff fs fp (Just $ revisionId r1) (Just $ revisionId r2)
          toText (First  lines)   = First  $ map pack lines
          toText (Second lines)   = Second $ map pack lines
          toText (Both   lines _) = Both    (map pack lines) $ map pack lines

-- |Get the latest revision of a file, if it exists.
latestRevision :: FilePath -> RequestProcessor Sitemap (Maybe Revision)
latestRevision fp = withFileStore $ \fs -> latestRevisionFS fs fp

-- |Alternative version of `latestRevision` which takes a file store.
latestRevisionFS :: MonadIO m => FileStore -> FilePath -> m (Maybe Revision)
latestRevisionFS fs fp = liftIO $ rev `onStoreExc` return Nothing
    where rev = toRevision . pack <$> latest fs fp

-- |Check if a file exists at the moment
doesFileExist :: FilePath -> RequestProcessor Sitemap Bool
doesFileExist fp = withFileStore $ flip doesFileExistFS fp

-- |Alternative version of `doesFileExist` which takes a file store.
doesFileExistFS :: MonadIO m => FileStore -> FilePath -> m Bool
doesFileExistFS fs fp = liftM (isJust :: Maybe ByteString -> Bool) $ getStoredFileFS fs fp

-- |Create a new file.
create :: FilePath
       -> Text
       -- ^The author
       -> Text
       -- ^The change description
       -> Text
       -- ^The new contents
       -> RequestProcessor Sitemap ()
create fp who desc txt = withFileStore $ \fs -> createFS fs fp who desc txt

-- |Alternative version of `create` which takes a file store.
createFS :: MonadIO m => FileStore -> FilePath -> Text -> Text -> Text -> m ()
createFS fs fp who desc txt = liftIO $ FS.create fs fp author description txt
  where author      = Author { authorName = unpack who, authorEmail = "" }
        description = unpack desc

-- |Update a file if it has not been touched since the given
-- revision. If it has been, merge conflict information is returned
-- instead.
save :: FilePath
     -> Revision
     -> Text
     -- ^The author
     -> Text
     -- ^The change description
     -> Text
     -- ^The new contents
     -> RequestProcessor Sitemap (Either Merge ())
save fp r who desc txt = withFileStore $ \fs -> saveFS fs fp r who desc txt

-- |Alternative version of `save` which takes a file store.
saveFS :: MonadIO m => FileStore -> FilePath -> Revision -> Text -> Text -> Text -> m (Either Merge ())
saveFS fs fp r who desc txt = do
  res <- liftIO $ modify fs fp rid author description txt

  return $
    case res of
      Left merge -> Left Merge { mergRevision           = fromJust . toRevision . pack . revId $ mergeRevision merge
                              , mergUnderlyingRevision = revId $ mergeRevision merge
                              , mergConflicts          = mergeConflicts merge
                              , mergText               = pack $ mergeText merge
                              }
      _          -> Right ()

  where rid         = revisionId r
        author      = Author { authorName = unpack who, authorEmail = "" }
        description = unpack desc


-- |Overwrite a file completely.
overwrite :: FilePath
          -> Text
          -- ^The author
          -> Text
          -- ^The change description
          -> ByteString
          -- ^The new contents
          -> RequestProcessor Sitemap ()
overwrite fp who desc cntnts = withFileStore $ \fs -> overwriteFS fs fp who desc cntnts

-- |Alternative version of `overwrite` which takes a file store.
overwriteFS :: MonadIO m => FileStore -> FilePath -> Text -> Text -> ByteString -> m ()
overwriteFS fs fp who desc cntnts = liftIO $ FS.save fs fp author description cntnts
    where author      = Author { authorName = unpack who, authorEmail = "" }
          description = unpack desc
