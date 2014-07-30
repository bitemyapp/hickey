module Handler.Static
    ( attachment
    , attachmentAtRevision
    , attachments
    , attachmentsAtRevision
    , static) where

import Types
import Routes
import Web.Seacat

-- |Display an attachment as it exists currently.
attachment :: WikiPage -> FileName -> Handler Sitemap
attachment wp fn = undefined

-- |Display an attachment at the given revision.
attachmentAtRevision :: WikiPage -> FileName -> Revision -> Handler Sitemap
attachmentAtRevision wp fn r = undefined

-- |Display the list of attachments as it is now.
attachments :: WikiPage -> Handler Sitemap
attachments wp = undefined

-- |Display the list of attachments as it was at the given revision,
-- or an error page if the revision is bad.
attachmentsAtRevision :: WikiPage -> Revision -> Handler Sitemap
attachmentsAtRevision wp r = undefined

-- |Display a static file, if it exists.
static :: FileName -> Handler Sitemap
static fn = undefined
