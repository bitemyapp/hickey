module Store
    ( -- *Updates to the store
      Commit(..)

    -- *Updates to files
    , Difference
    , Diff(..)

    -- *Failed updates
    , Merge(..)

    -- *File retrieval
    , Contents
    , getStoredFile
    , getStoredFileAt
    , getStoredFileAt'

    -- *File history
    , getHistory
    , getDiff

    -- *File status
    , latestRevision
    , doesFileExist

    -- *File updates
    , create
    , save
    , overwrite

    -- *Utility functions
    , getFileStore
    , getPlugins
    , listFiles
    ) where

import Store.History
import Store.Retrieve
import Store.Types
import Store.Utils
import Store.Write
