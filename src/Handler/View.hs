module Handler.View
    ( page
    , pageAtRevision
    , history
    , diff
    ) where

import Types
import Routes
import Web.Seacat

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = undefined

-- |Display a page as it was at the given revision. If the revision ID
-- is bad (doesn't exist or predates this page), an error is displayed
-- instead.
pageAtRevision :: WikiPage -> Revision -> Handler Sitemap
pageAtRevision wp r = undefined

-- |Display all of the commits that have gone into a page.
history :: WikiPage -> Handler Sitemap
history wp = undefined

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = undefined
