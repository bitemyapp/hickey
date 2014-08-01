{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , history
    , diff
    ) where

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Routes
import Store
import Text.Blaze.Html5 (Html)
import Templates
import Types
import Web.Seacat (Handler, MkUrl)
import Web.Seacat.RequestHandler (htmlUrlResponse)

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = do
  wikiPage <- getStoredFile $ pageFileName wp
  case wikiPage of
    Just contents -> htmlUrlResponse $ renderWikiPage wp contents
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

-- |Get the filename of a wiki page.
pageFileName :: WikiPage -> FileName
pageFileName = fromJust . toFileName . (<> ".md") . pageTextName

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
renderBadRevision :: WikiPage -> Revision -> MkUrl Sitemap -> Html
renderBadRevision wp r = renderNoticePage "Revision too old" $ "The page" <> pageTextName wp <> " did not exist at revision " <> revisionTextId r <> "."

-- |Render an error page, saying that a revision range is bad.
renderBadDiff :: WikiPage -> Revision -> Revision -> MkUrl Sitemap -> Html
renderBadDiff wp r1 r2 = renderNoticePage "Bad commit range" $ "The diff for " <> pageTextName wp <> " could not be generated for the range " <> revisionTextId r1 <> "–" <> revisionTextId r2 <> "."

-- |Render an error page, saying that a page name is bad.
renderBadPage :: WikiPage -> MkUrl Sitemap -> Html
renderBadPage wp = renderNoticePage "No such page" $ "The page " <> pageTextName wp <> " does not exist."
