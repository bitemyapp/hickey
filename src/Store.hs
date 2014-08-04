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

    , Contents
    , getStoredFile
    , getStoredFileAt
    , getStoredFileAt'

    , getHistory
    , getDiff

    , latestRevision

    , create
    , Store.save
    , overwrite
    ) where

import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (Diff(..), toByteString, fromByteString, modify, revId, revDateTime, revAuthor, revDescription, history, diff)
import Data.FileStore.Git (gitFileStore)
import Data.FileStore.Types (Author(..), Contents, FileStore(..), FileStoreError, MergeInfo(..), RevisionId, TimeRange(..), UTCTime)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Routes
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

-- |Get the contents of the HEAD of a file, if it exists.
getStoredFile :: Contents c => FilePath -> RequestProcessor Sitemap (Maybe c)
getStoredFile fp = getStoredFileAt' fp Nothing

-- |Get the contents of a specific revision of a file. If the file
-- didn't exist at that revision, return Nothing.
getStoredFileAt :: Contents c => FilePath -> Revision -> RequestProcessor Sitemap (Maybe c)
getStoredFileAt fp = getStoredFileAt' fp . Just

-- |Get the contents of a file at the given revision, or the latest if
-- the revision is unspecified.
getStoredFileAt' :: Contents c => FilePath -> Maybe Revision -> RequestProcessor Sitemap (Maybe c)
getStoredFileAt' fn r = withFileStore $ \fs -> contents fs `onStoreExc` return Nothing
    where contents fs = Just <$> retrieve fs fn (unpack . revisionTextId <$> r)

-- |Get the history of a file, if it exists, as a list of commits.
getHistory ::  FilePath -> RequestProcessor Sitemap (Maybe [Commit])
getHistory fp = withFileStore $ \fs -> hist fs `onStoreExc` return Nothing
    where hist fs = (Just . map toCommit) <$> history fs [fp] (TimeRange Nothing Nothing) Nothing
          toCommit r = Commit { commitRevision           = fromJust . toRevision . pack $ revId r
                              , commitUnderlyingRevision = revId r
                              , commitTime               = revDateTime r
                              , commitAuthor             = pack . authorName $ revAuthor r
                              , commitMessage            = pack $ revDescription r
                              }

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff :: FilePath -> Revision -> Revision -> RequestProcessor Sitemap (Maybe Differences)
getDiff fp r1 r2 = withFileStore $ \fs -> dif fs `onStoreExc` return Nothing
    where dif fs = Just . map toText <$> diff fs fp (Just $ revisionId r1) (Just $ revisionId r2)
          toText (First  lines)   = First  $ map pack lines
          toText (Second lines)   = Second $ map pack lines
          toText (Both   lines _) = Both    (map pack lines) $ map pack lines

-- |Get the latest revision of a file, if it exists.
latestRevision :: FilePath -> RequestProcessor Sitemap (Maybe Revision)
latestRevision fp = withFileStore $ \fs -> rev fs `onStoreExc` return Nothing
    where rev fs = toRevision . pack <$> latest fs fp

-- |Create a new file.
create :: FilePath
       -> Text
       -- ^The author
       -> Text
       -- ^The change description
       -> Text
       -- ^The new contents
       -> RequestProcessor Sitemap ()
create fp who desc txt = withFileStore $ \fs -> FS.create fs fp author description txt
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
save fp r who desc txt = withFileStore $ \fs -> do
  res <- modify fs fp rid author description txt

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
overwrite fp who desc cntnts = withFileStore $ \fs -> FS.save fs fp author description cntnts
    where author      = Author { authorName = unpack who, authorEmail = "" }
          description = unpack desc
