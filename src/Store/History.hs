-- |Functions for dealing with the history.
module Store.History where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.FileStore (TimeRange(..), latest, history, diff)
import Data.Maybe (isJust, maybeToList)
import Data.Text (pack)
import Store.Types
import Store.Utils
import Types
import Web.Routes (PathInfo)
import Web.Seacat (RequestProcessor)

-- *Informational

-- |Get the latest revision of a file, if it exists.
latestRevision :: PathInfo r => FilePath -> RequestProcessor r (Maybe Revision)
latestRevision fp = withFileStore $ \fs -> latestRevisionFS fs fp

-- |Alternative version of `latestRevision` which takes a file store.
latestRevisionFS :: MonadIO m => FileStore -> FilePath -> m (Maybe Revision)
latestRevisionFS fs fp = liftIO $ rev `onStoreExc` return Nothing
    where rev = toRevision . pack <$> latest fs fp

-- |Get the history of a file, if it exists, as a list of commits. If
-- no file is given, get all history. There is also an optional limit.
getHistory :: PathInfo r => Maybe FilePath -> Maybe Int -> RequestProcessor r (Maybe [Commit])
getHistory fp limit = withFileStore $ \fs -> getHistoryFS fs fp limit

-- |Alternative version of `getHistory` which takes a file store.
getHistoryFS :: MonadIO m => FileStore -> Maybe FilePath -> Maybe Int -> m (Maybe [Commit])
getHistoryFS fs fp limit = liftIO $ hist `onStoreExc` return Nothing
    where hist = (Just . mlimit . filter isWikiCommit . map fromStoreCommit) <$> history fs (maybeToList fp) (TimeRange Nothing Nothing) Nothing
          isWikiCommit = isJust . commitTarget
          mlimit xs = case limit of
                        Just n  -> take n xs
                        Nothing -> xs

-- *Comparison

-- |Get the diff of a file between two revisions, if they're good and
-- the file exists, as a list of changes.
getDiff :: PathInfo r => FilePath -> Revision -> Revision -> RequestProcessor r (Maybe [Difference])
getDiff fp r1 r2 = withFileStore $ \fs -> getDiffFS fs fp r1 r2

-- |Alternative version of `getDiff` which takes a file store.
getDiffFS :: MonadIO m => FileStore -> FilePath -> Revision -> Revision -> m (Maybe [Difference])
getDiffFS fs fp r1 r2 = liftIO $ dif `onStoreExc` return Nothing
    where dif = Just . map toText <$> diff fs fp (Just $ toStoreRevisionId r1) (Just $ toStoreRevisionId r2)
          toText (First  ls)   = First  $ map pack ls
          toText (Second ls)   = Second $ map pack ls
          toText (Both   ls _) = Both    (map pack ls) $ map pack ls
