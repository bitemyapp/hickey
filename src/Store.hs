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

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore.Generic (Diff)
import Data.FileStore.Git (gitFileStore)
import Data.FileStore.Types
import Data.Text (Text, pack, unpack)
import Routes
import Types
import Web.Seacat (RequestProcessor, conf')

data Commit = Commit { commitRevision :: Revision
                     , commitUnderlyingRevision :: RevisionId
                     , commitTime     :: UTCTime
                     , commitAuthor   :: Text
                     , commitMessage  :: Text
                     }

type Differences = [Diff [Text]]

instance Contents Text where
    fromByteString = pack . fromByteString
    toByteString   = toByteString . unpack

-- |Run a function that takes a reference to the filestore.
withFileStore :: (FileStore -> IO a) -> RequestProcessor Sitemap a
withFileStore f = do
  git <- gitFileStore <$> conf' "git" "path"
  liftIO $ initialize git
  liftIO $ f git

-----

-- |Get the contents of the HEAD of a text file, if it exists.
getStoredFile :: FileName -> RequestProcessor Sitemap (Maybe Text)
getStoredFile fn = withFileStore undefined

-- |Get the contents of a specific revision of a text file. If the
-- file didn't exist at that revision, return Nothing.
getStoredFileAt ::  FileName -> Revision -> RequestProcessor Sitemap (Maybe Text)
getStoredFileAt fn r = withFileStore undefined

-- |Get the contents of the HEAD of a binary file, if it exists, as a
-- lazy bytestring.
getStoredBinary ::  FileName -> RequestProcessor Sitemap (Maybe ByteString)
getStoredBinary fn = withFileStore undefined

-- |Get the contents of a specific revision of a binary file, if it
-- exists, as a lazy bytestring.
getStoredBinaryAt ::  FileName -> Revision -> RequestProcessor Sitemap (Maybe ByteString)
getStoredBinaryAt fn r = withFileStore undefined

-- |Get the history of a file, if it exists, as a list of commits.
getHistory ::  FileName -> RequestProcessor Sitemap (Maybe [Commit])
getHistory fn = withFileStore undefined

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff ::  FileName -> Revision -> Revision -> RequestProcessor Sitemap (Maybe Differences)
getDiff fn r1 r2 = withFileStore undefined
