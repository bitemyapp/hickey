module Types where

import Data.Char (isAlphaNum, isHexDigit)
import Data.Text (Text, strip, pack, unpack)
import Text.Regex (mkRegex, subRegex)

import qualified Data.Text as T

-- *Wiki pages

-- |A wiki page name is some sequence of alphanumeric characters.
newtype WikiPage = WikiPage { pageTextName :: Text }
    deriving (Eq, Show)

-- |Display a page name with spaces between words.
pageNiceName :: WikiPage -> Text
pageNiceName wp = strip . pack $ subRegex regex (unpack $ pageTextName wp) " \\0"
    where regex = mkRegex "[A-Z][a-z]"

-- |Check if some text is a valid page name
isPageName :: Text -> Bool
isPageName t = T.all isAlphaNum t && T.length t > 1

-- |Convert some text to a page, if valid.
toWikiPage :: Text -> Maybe WikiPage
toWikiPage p | isPageName p = Just $ WikiPage p
             | otherwise    = Nothing

-- *Revisions

-- |A revision identifier is a hex string.
newtype Revision = Revision { revisionTextId :: Text }
    deriving (Eq, Show)

-- |Get an abbreviated version of a revision ID.
revisionShortId :: Revision -> Text
revisionShortId = T.take 7 . revisionTextId

-- |Check if some text is a valid revision id
isRevisionId :: Text -> Bool
isRevisionId t = T.all isHexDigit t && T.length t > 0

-- |Convert some text to a revision, if valid.
toRevision :: Text -> Maybe Revision
toRevision r | isRevisionId r = Just $ Revision r
             | otherwise      = Nothing

-- *Files

-- |A filename is a string [a-zA-Z0-9_-] with a single '.'.
newtype FileName = FileName { fileTextName :: Text }
    deriving (Eq, Show)

-- |Check if some text is a valid filename.
isFileName :: Text -> Bool
isFileName fn = plen && palpha
    where palpha = all (T.all isFileChr) parts
          plen = length parts == 2
          parts = T.split (=='.') fn
          isFileChr c = isAlphaNum c || c == '-' || c == '_'

-- |Convert some text to a filename, if valid.
toFileName :: Text -> Maybe FileName
toFileName f | isFileName f = Just $ FileName f
             | otherwise    = Nothing

-- *Misc

-- |A plugin has a name, and a path to an executable.
type Plugin = (String, FilePath)
