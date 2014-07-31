module Store
    ( Commit
    , commitRevision
    , commitTime
    , commitAuthor
    , commitMessage

    , Differences

    , getStoredFile
    , getStoredFileAt
    , getStoredBinary
    , getStoredBinaryAt
    , getHistory
    , getDiff
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Types
import Data.FileStore.Generic (Diff)
import Data.FileStore.Types (RevisionId, UTCTime)

data Commit = Commit { commitRevision :: Revision
                     , commitUnderlyingRevision :: RevisionId
                     , commitTime     :: UTCTime
                     , commitAuthor   :: Text
                     , commitMessage  :: Text
                     }

type Differences = [Diff [Text]]

-- |Get the contents of the HEAD of a text file, if it exists.
getStoredFile :: MonadIO m => FileName -> m (Maybe Text)
getStoredFile fn = undefined

-- |Get the contents of a specific revision of a text file. If the
-- file didn't exist at that revision, return Nothing.
getStoredFileAt :: MonadIO m => FileName -> Revision -> m (Maybe Text)
getStoredFileAt fn r = undefined

-- |Get the contents of the HEAD of a binary file, if it exists, as a
-- lazy bytestring.
getStoredBinary :: MonadIO m => FileName -> m (Maybe ByteString)
getStoredBinary fn = undefined

-- |Get the contents of a specific revision of a binary file, if it
-- exists, as a lazy bytestring.
getStoredBinaryAt :: MonadIO m => FileName -> Revision -> m (Maybe ByteString)
getStoredBinaryAt fn r = undefined

-- |Get the history of a file, if it exists, as a list of commits.
getHistory :: MonadIO m => FileName -> m (Maybe [Commit])
getHistory fn = undefined

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff :: MonadIO m => FileName -> Revision -> Revision -> m (Maybe Differences)
getDiff fn r1 r2 = undefined
