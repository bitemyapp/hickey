{-# LANGUAGE FlexibleInstances, TupleSections, OverloadedStrings #-}

-- |Types for FileStores. The types exported from this module abstract
-- over the underlying Data.FileStore types.
module Store.Types
    ( module Store.Types
    , Contents
    , Diff(..)
    , FileStore
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.FileStore (Diff(..), revAuthor, revChanges, revDateTime, revDescription, revId)
import Data.FileStore.Types (Author(..), Change(..), Contents(..), FileStore, RevisionId, UTCTime)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack, splitOn)
import Types

import qualified Data.FileStore.Types as FS
import qualified Data.Text            as T

-- *Types

-- |A commit consists of a revision ID, time of commit, author text
-- (at least a name, may be an email), summary of changes, and a
-- target.
data Commit = Commit { commitRevision :: Revision
                     , commitTime     :: UTCTime
                     , commitAuthor   :: Text
                     , commitMessage  :: Text
                     , commitTarget   :: (WikiPage, Maybe FileName)
                     }

-- |A difference is a collection of some lines of text, with an
-- indication of whether they came from the first, second, or both
-- sources.
type Difference = Diff [Text]

-- |A merge consists of a revision the automated merge was attempted
-- with, along with the attempted merge.
data Merge = Merge { mergRevision  :: Revision
                   , mergConflicts :: Bool
                   , mergText      :: Text
                   }

-- |File retrieval returns an instance of `Contents`, as the majority
-- of the code deals with `Text`, this lets us avoid packing and
-- unpacking strings all the time.
instance Contents Text where
    fromByteString = pack . fromByteString
    toByteString   = toByteString . unpack

-- |Sometimes line-by-line access to a file is desired.
instance Contents [Text] where
    fromByteString = T.lines . fromByteString
    toByteString   = toByteString . T.unlines

-- *Conversion functions

-- |Turn a FileStore `Revision` into a `Commit`. This is only a maybe
-- because the target might not be valid.
fromStoreCommit :: FS.Revision -> Maybe Commit
fromStoreCommit r =  target <&> \target' -> Commit { commitRevision = fromStoreRevision r
                                                  , commitTime     = revDateTime r
                                                  , commitAuthor   = fromStoreAuthor $ revAuthor r
                                                  , commitMessage  = pack $ revDescription r
                                                  , commitTarget   = target'
                                                  }

    where target = case revChanges r of
                     [Added fp]    -> toWikiPath $ pack fp
                     [Modified fp] -> toWikiPath $ pack fp
                     _             -> Nothing

          toWikiPath fp = case splitOn ".md" fp of
                            [pagename, ""] -> (, Nothing) <$> toWikiPage pagename
                            _ -> case splitOn "-files/" fp of
                                   [pagename, filename] -> fmap Just <$> ((,) <$> toWikiPage pagename <*> toFileName filename)
                                   _ -> Nothing

          (<&>) = flip fmap

-- |Turn a FileStore `Revision` into a `Revision`.
fromStoreRevision :: FS.Revision -> Revision
fromStoreRevision = fromJust . toRevision . pack . revId

-- |Tuen a `Revision` into a FileStore `RevisionId`.
toStoreRevisionId :: Revision -> RevisionId
toStoreRevisionId = unpack . revisionTextId

-- |Turn a FileStore `Author` into a `Text`.
fromStoreAuthor :: Author -> Text
fromStoreAuthor = pack . authorName

-- |Turn a textual description of an author into an `Author`.
toStoreAuthor :: Text -> Author
toStoreAuthor name = Author { authorName  = unpack name
                            , authorEmail = ""
                            }
