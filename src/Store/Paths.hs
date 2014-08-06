{-# LANGUAGE OverloadedStrings #-}

module Store.Paths where

import Data.Monoid ((<>))
import Data.Text (unpack)
import System.FilePath.Posix (joinPath)
import Types

-- *Files

-- |Get the path to a wiki page in the store.
wikipage :: WikiPage -> FilePath
wikipage wp = unpack $ pageTextName wp <> ".md"

-- |Get the path to a file attached to a page.
attachment :: WikiPage -> FileName -> FilePath
attachment wp fn = joinPath [ attachmentdir wp
                            , unpack $ fileTextName fn
                            ]

-- |Get the path to a static file in the store.
static :: FileName -> FilePath
static fn = joinPath [ staticdir
                     , unpack $ fileTextName fn
                     ]

-- *Directories

-- |Get the path to the attachments of a page.
attachmentdir :: WikiPage -> FilePath
attachmentdir wp = unpack $ pageTextName wp <> "-files"

-- |Get the path to the static files.
staticdir :: FilePath
staticdir = "static"

-- |Get the path to the plugins.
plugindir :: FilePath
plugindir = "plugins"
