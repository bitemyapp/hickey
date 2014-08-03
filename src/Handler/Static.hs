{-# LANGUAGE OverloadedStrings #-}

module Handler.Static
    ( file
    , fileAtRevision
    , files
    , static) where

import Data.Monoid ((<>))
import Data.Text (unpack)
import Routes
import System.FilePath.Posix (joinPath)
import Types
import Web.Seacat (Handler, conf', textResponse, respondFile)

-- |Display a file as it exists currently.
file :: WikiPage -> FileName -> Handler Sitemap
file wp fn = undefined

-- |Display a file at the given revision.
fileAtRevision :: WikiPage -> FileName -> Revision -> Handler Sitemap
fileAtRevision wp fn r = undefined

-- |Display the list of files as it is now.
files :: WikiPage -> Handler Sitemap
files wp = undefined

-- |Display a static file, if it exists.
static :: FileName -> Handler Sitemap
static fn = do
  gpath <- conf' "git" "path"
  let fpath = joinPath [gpath, "static", unpack $ fileTextName fn]
  respondFile (textResponse $ "Cannot find " <> fileTextName fn) fpath
