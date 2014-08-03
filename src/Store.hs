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

    , getStoredFile
    , getStoredFileAt
    , getStoredBinary
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
import Data.Monoid ((<>))
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

-- |Convert a FileName to a FilePath, relative to the root of the file
-- store.
filePath :: FileName -> FilePath
filePath = unpack . fileTextName

-- |Get the file path for a binary file.
fileBinaryPath :: WikiPage -> FileName -> FilePath
fileBinaryPath wp fn = unpack $ pageTextName wp <> "-files/" <> fileTextName fn

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

-- |Get the contents of the HEAD of a text file, if it exists.
getStoredFile :: FileName -> RequestProcessor Sitemap (Maybe Text)
getStoredFile fn = getStoredFileAt' (filePath fn) Nothing

-- |Get the contents of a specific revision of a text file. If the
-- file didn't exist at that revision, return Nothing.
getStoredFileAt :: FileName -> Revision -> RequestProcessor Sitemap (Maybe Text)
getStoredFileAt fn r = getStoredFileAt' (filePath fn) $ Just r

-- |Get the contents of a file at the given revision, or the latest if
-- the revision is unspecified.
getStoredFileAt' :: FilePath -> Maybe Revision -> RequestProcessor Sitemap (Maybe Text)
getStoredFileAt' fn r = withFileStore $ \fs -> contents fs `onStoreExc` return Nothing
    where contents fs = Just <$> retrieve fs fn Nothing

-- |Get the contents of the HEAD of a binary file, if it exists, as a
-- lazy bytestring.
getStoredBinary ::  FileName -> RequestProcessor Sitemap (Maybe ByteString)
getStoredBinary fn = withFileStore undefined

-- |Get the history of a file, if it exists, as a list of commits.
getHistory ::  FileName -> RequestProcessor Sitemap (Maybe [Commit])
getHistory fn = withFileStore $ \fs -> hist fs `onStoreExc` return Nothing
    where hist fs = (Just . map toCommit) <$> history fs [filePath fn] (TimeRange Nothing Nothing) Nothing
          toCommit r = Commit { commitRevision           = fromJust . toRevision . pack $ revId r
                              , commitUnderlyingRevision = revId r
                              , commitTime               = revDateTime r
                              , commitAuthor             = pack . authorName $ revAuthor r
                              , commitMessage            = pack $ revDescription r
                              }

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff :: FileName -> Revision -> Revision -> RequestProcessor Sitemap (Maybe Differences)
getDiff fn r1 r2 = withFileStore $ \fs -> dif fs `onStoreExc` return Nothing
    where dif fs = Just . map toText <$> diff fs (filePath fn) (Just $ revisionId r1) (Just $ revisionId r2)
          toText (First  lines)   = First  $ map pack lines
          toText (Second lines)   = Second $ map pack lines
          toText (Both   lines _) = Both    (map pack lines) $ map pack lines

-- |Get the latest revision of a text file, if it exists.
latestRevision :: FileName -> RequestProcessor Sitemap (Maybe Revision)
latestRevision fn = withFileStore $ \fs -> rev fs `onStoreExc` return Nothing
    where rev fs = toRevision . pack <$> latest fs (filePath fn)

-- |Create a new text file.
create :: FileName
       -> Text
       -- ^The author
       -> Text
       -- ^The change description
       -> Text
       -- ^The new contents
       -> RequestProcessor Sitemap ()
create fn who desc txt = withFileStore $ \fs -> FS.create fs fp author description txt
  where fp          = filePath fn
        author      = Author { authorName = unpack who, authorEmail = "" }
        description = unpack desc

-- |Update a file if it has not been touched since the given
-- revision. If it has been, merge conflict information is returned
-- instead.
save :: FileName
     -> Revision
     -> Text
     -- ^The author
     -> Text
     -- ^The change description
     -> Text
     -- ^The new contents
     -> RequestProcessor Sitemap (Either Merge ())
save fn r who desc txt = withFileStore $ \fs -> do
  res <- modify fs fp rid author description txt

  return $
    case res of
      Left merge -> Left Merge { mergRevision           = fromJust . toRevision . pack . revId $ mergeRevision merge
                              , mergUnderlyingRevision = revId $ mergeRevision merge
                              , mergConflicts          = mergeConflicts merge
                              , mergText               = pack $ mergeText merge
                              }
      _          -> Right ()

  where fp          = filePath fn
        rid         = revisionId r
        author      = Author { authorName = unpack who, authorEmail = "" }
        description = unpack desc

-- |Overwrite a binary file completely.
overwrite :: WikiPage
          -> FileName
          -> Text
          -- ^The author
          -> Text
          -- ^The change description
          -> ByteString
          -- ^The new contents
          -> RequestProcessor Sitemap ()
overwrite wp fn who desc cntnts = withFileStore $ \fs -> FS.save fs fp author description cntnts
    where fp          = fileBinaryPath wp fn
          author      = Author { authorName = unpack who, authorEmail = "" }
          description = unpack desc
