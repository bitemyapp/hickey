{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , history
    , diff
    ) where

import Data.Maybe (fromJust)
import Handler.Error
import Routes
import Store
import Templates
import Templates.Utils
import Text.Blaze.Html5
import Types
import Web.Seacat (Handler, MkUrl)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Text.Blaze.Html5 as H

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = do
  wikiPage <- getStoredFile $ pageFileName wp
  htmlUrlResponse $
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
    Just contents -> htmlUrlResponse $ renderWikiPageAt wp r contents
    _             -> htmlUrlResponse $ renderBadRevision wp r

-- |Display all of the commits that have gone into a page.
history :: WikiPage -> Handler Sitemap
history wp = do
  hist <- getHistory $ pageFileName wp
  case hist of
    Just events -> renderHist wp events
    _           -> htmlUrlResponse $ renderBadPage wp

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = do
  changelog <- getDiff (pageFileName wp) r1 r2
  case changelog of
    Just differences -> renderDiff wp r1 r2 differences
    _                -> htmlUrlResponse $ renderBadDiff wp r1 r2

-----

-- |Render a new page, inviting users to create it.
renderNewPage :: WikiPage -> MkUrl Sitemap -> Html
renderNewPage wp = renderHtmlPage (pageTextName wp) $ \mkurl -> do
                     _ <- "This page does not exist "
                     link' mkurl "Create New Page" "why not create it?" $ Edit wp

-- |Display a list of commits.
renderHist :: WikiPage -> [Commit] -> Handler Sitemap
renderHist wp hist = undefined

-- |Display a list of changes.
renderDiff :: WikiPage -> Revision -> Revision -> Differences -> Handler Sitemap
renderDiff wp r1 r2 diff = undefined
