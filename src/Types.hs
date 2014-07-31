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

import Data.Text (Text)

-- |A wiki page name is some sequence of alphanumeric characters.
newtype WikiPage = WikiPage { pageTextName :: Text }
    deriving (Eq, Show)

-- |A revision identifier is a hex string.
newtype Revision = Revision { revisionTextId :: Text }
    deriving (Eq, Show)

-- |A filename is a string with no slashes and a single '.'.
newtype FileName = FileName { fileTextName :: Text }
    deriving (Eq, Show)

-- |Check if some text is a valid page name
isPageName :: Text -> Bool
isPageName = undefined

-- |Convert some text to a page, if valid.
toWikiPage :: Text -> Maybe WikiPage
toWikiPage p | isPageName p = Just $ WikiPage p
             | otherwise    = Nothing

-- |Check if some text is a valid revision id
isRevisionId :: Text -> Bool
isRevisionId = undefined

-- |Convert some text to a revision, if valid.
toRevision :: Text -> Maybe Revision
toRevision r | isRevisionId r = Just $ Revision r
             | otherwise      = Nothing

-- |Check if some text is a valid filename.
isFileName :: Text -> Bool
isFileName = undefined

-- |Convert some text to a filename, if valid.
toFileName :: Text -> Maybe FileName
toFileName f | isFileName f = Just $ FileName f
             | otherwise    = Nothing
