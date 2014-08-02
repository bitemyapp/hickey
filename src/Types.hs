{-# LANGUAGE OverloadedStrings #-}

module Types
    ( WikiPage
    , pageTextName
    , isPageName
    , toWikiPage

    , Revision
    , revisionTextId
    , isRevisionId
    , toRevision

    , FileName
    , fileTextName
    , isFileName
    , toFileName
    ) where

import Data.Char (isAlphaNum, isHexDigit)
import Data.Text (Text, splitOn)

import qualified Data.Text as T

-- |A wiki page name is some sequence of alphanumeric characters.
newtype WikiPage = WikiPage { pageTextName :: Text }
    deriving (Eq, Show)

-- |A revision identifier is a hex string.
newtype Revision = Revision { revisionTextId :: Text }
    deriving (Eq, Show)

-- |A filename is an alphanumeric string with a single '.'.
newtype FileName = FileName { fileTextName :: Text }
    deriving (Eq, Show)

-- |Check if some text is a valid page name
isPageName :: Text -> Bool
isPageName = T.all isAlphaNum

-- |Convert some text to a page, if valid.
toWikiPage :: Text -> Maybe WikiPage
toWikiPage p | isPageName p = Just $ WikiPage p
             | otherwise    = Nothing

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
    where palpha = all (T.all isAlphaNum) parts
          plen = length parts == 2
          parts = splitOn "." fn

-- |Convert some text to a filename, if valid.
toFileName :: Text -> Maybe FileName
toFileName f | isFileName f = Just $ FileName f
             | otherwise    = Nothing
