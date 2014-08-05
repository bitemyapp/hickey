module Store
    ( -- *Updates to the store
      Commit
    , commitRevision
    , commitTime
    , commitAuthor
    , commitMessage

    -- *Updates to files
    , Difference
    , Diff(..)

    -- *Failed updates
    , Merge
    , mergRevision
    , mergConflicts
    , mergText

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
    ) where

import Store.History
import Store.Retrieve
import Store.Types
import Store.Utils
import Store.Write
