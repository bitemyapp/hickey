module Handler.Edit
    ( edit
    , commit) where

import Types
import Routes
import Web.Seacat

-- |Display an edit form for a page.
edit :: WikiPage -> Handler Sitemap
edit wp = undefined

-- |Save an edit, possibly uploading some new attachments in the
-- process.
commit :: WikiPage -> Handler Sitemap
commit wp = undefined
