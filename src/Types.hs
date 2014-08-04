{-# LANGUAGE OverloadedStrings #-}

module Types
    ( WikiPage
    , pageTextName
    , isPageName
    , toWikiPage

    , Revision
    , revisionTextId
    , revisionShortId
    , isRevisionId
    , toRevision

    , FileName
    , fileTextName
    , isFileName
    , toFileName

    , Plugin
    ) where

import Data.Char (isAlphaNum, isHexDigit)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, splitOn)
import System.FilePath (FilePath)

import qualified Data.Text as T

-- |A wiki page name is some sequence of alphanumeric characters.
newtype WikiPage = WikiPage { pageTextName :: Text }
    deriving (Eq, Show)

-- |A revision identifier is a hex string.
newtype Revision = Revision { revisionTextId :: Text }
    deriving (Eq, Show)

-- |A filename is a string [a-zA-Z0-9_-] with a single '.'.
newtype FileName = FileName { fileTextName :: Text }
    deriving (Eq, Show)

-- |A plugin has a name, and a path to an executable.
type Plugin = (String, FilePath)

-- |Check if some text is a valid page name
isPageName :: Text -> Bool
isPageName = T.all isAlphaNum

-- |Convert some text to a page, if valid.
toWikiPage :: Text -> Maybe WikiPage
toWikiPage p | isPageName p = Just $ WikiPage p
             | otherwise    = Nothing

-- |Get an abbreviated version of a revision ID.
revisionShortId :: Revision -> Text
revisionShortId = T.take 7 . revisionTextId

-- |Check if some text is a valid revision id
isRevisionId :: Text -> Bool
isRevisionId = T.all isHexDigit

-- |Convert some text to a revision, if valid.
toRevision :: Text -> Maybe Revision
toRevision r | isRevisionId r = Just $ Revision r
             | otherwise      = Nothing

-- |Check if some text is a valid filename.
isFileName :: Text -> Bool
isFileName fn = plen && palpha
    where palpha = all (T.all isFileChr) parts
          plen = length parts == 2
          parts = splitOn "." fn
          isFileChr c = isAlphaNum c || c == '-' || c == '_'

-- |Convert some text to a filename, if valid.
toFileName :: Text -> Maybe FileName
toFileName f | isFileName f = Just $ FileName f
             | otherwise    = Nothing
