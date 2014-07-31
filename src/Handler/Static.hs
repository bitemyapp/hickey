module Handler.Static
    ( file
    , fileAtRevision
    , files
    , filesAtRevision
    , static) where

import Types
import Routes
import Web.Seacat (Handler)

-- |Display a file as it exists currently.
file :: WikiPage -> FileName -> Handler Sitemap
file wp fn = undefined

-- |Display a file at the given revision.
fileAtRevision :: WikiPage -> FileName -> Revision -> Handler Sitemap
fileAtRevision wp fn r = undefined

-- |Display the list of files as it is now.
files :: WikiPage -> Handler Sitemap
files wp = undefined

-- |Display the list of files as it was at the given revision, or an
-- error page if the revision is bad.
filesAtRevision :: WikiPage -> Revision -> Handler Sitemap
filesAtRevision wp r = undefined

-- |Display a static file, if it exists.
static :: FileName -> Handler Sitemap
static fn = undefined
