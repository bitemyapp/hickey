{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , history
    , diff
    ) where

import Data.Maybe (fromJust)
import Data.Text (Text, append)
import Routes
import Store
import Types
import Web.Seacat

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = do
  wikiPage <- getStoredFile $ pageFileName wp
  case wikiPage of
    Just contents -> renderWikiPage wp contents
    _             -> renderNewPage wp

-- |Display a page as it was at the given revision. If the revision ID
-- is bad (doesn't exist or predates this page), an error is displayed
-- instead.
pageAtRevision :: WikiPage -> Revision -> Handler Sitemap
pageAtRevision wp r = do
  wikiPage <- getStoredFileAt (pageFileName wp) r
  case wikiPage of
    Just contents -> renderWikiPage' wp r contents
    _             -> renderBadRevision wp r

-- |Display all of the commits that have gone into a page.
history :: WikiPage -> Handler Sitemap
history wp = do
  hist <- getHistory $ pageFileName wp
  case hist of
    Just events -> renderHist wp events
    _           -> renderBadPage wp

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = do
  changelog <- getDiff (pageFileName wp) r1 r2
  case changelog of
    Just differences -> renderDiff wp r1 r2 differences
    _                -> renderBadDiff wp r1 r2

-----

-- |Get the filename of a wiki page.
pageFileName :: WikiPage -> FileName
pageFileName = fromJust . toFileName . (`append` ".md") . pageTextName

-- |Render a wiki page to HTML.
renderWikiPage :: WikiPage -> Text -> Handler Sitemap
renderWikiPage wp contents = undefined

-- |Render a wiki page (including revision information in the title) to HTML.
renderWikiPage' :: WikiPage -> Revision -> Text -> Handler Sitemap
renderWikiPage' wp r contents = undefined

-- |Render a new page, inviting users to create it.
renderNewPage :: WikiPage -> Handler Sitemap
renderNewPage wp = undefined

-- |Display a list of commits.
renderHist :: WikiPage -> [Commit] -> Handler Sitemap
renderHist wp hist = undefined

-- |Display a list of changes.
renderDiff :: WikiPage -> Revision -> Revision -> Differences -> Handler Sitemap
renderDiff wp r1 r2 diff = undefined

-- |Render an error page, saying that a revision is bad.
renderBadRevision :: WikiPage -> Revision -> Handler Sitemap
renderBadRevision wp r = undefined

-- |Render an error page, saying that a revision range is bad.
renderBadDiff :: WikiPage -> Revision -> Revision -> Handler Sitemap
renderBadDiff wp r1 r2 = undefined

-- |Render an error page, saying that a page name is bad.
renderBadPage :: WikiPage -> Handler Sitemap
renderBadPage wp = undefined
