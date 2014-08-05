{-# LANGUAGE OverloadedStrings #-}

module Store.Paths
    ( wikipage
    , attachment
    , static
    , plugindir
    ) where

import Data.Monoid ((<>))
import Data.Text (unpack)
import System.FilePath.Posix (joinPath)
import Types

-- |Get the path to a wiki page in the store.
wikipage :: WikiPage -> FilePath
wikipage wp = unpack $ pageTextName wp <> ".md"

-- |Get the path to a file attached to a page.
attachment :: WikiPage -> FileName -> FilePath
attachment wp fn = joinPath [ unpack $ pageTextName wp <> "-files"
                            , unpack $ fileTextName fn
                            ]

-- |Get the path to a static file in the store.
static :: FileName -> FilePath
static fn = joinPath [ "static"
                     , unpack $ fileTextName fn
                     ]

-- |Get the path to the plugins.
plugindir :: FilePath
plugindir = "plugins"
