{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , pages
    , history
    , diff
    ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (isSuffixOf, splitOn, pack)
import Routes
import Store
import Store.Paths
import Templates
import Templates.Utils (link', hist, ifPresent)
import Text.Blaze.Html5 (Html, (!), section, h2, pre, code, ul, li)
import Types
import Web.Seacat (Handler, MkUrl, askMkUrl, htmlResponse)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Templates.Utils             as T
import qualified Text.Blaze.Html5.Attributes as A

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = renderPage wp Nothing $ htmlUrlResponse $ renderNewPage wp

-- |Display a page as it was at the given revision. If the revision ID
-- is bad (doesn't exist or predates this page), an error is displayed
-- instead.
pageAtRevision :: WikiPage -> Revision -> Handler Sitemap
pageAtRevision wp r = renderPage wp (Just r) $ htmlUrlResponse err
    where err = renderNoticePage "Error" $ "Failed to retrieve " <> pageNiceName wp <> " at " <> revisionShortId r <> "."

-- |Display a list of all pages
pages :: Handler Sitemap
pages = do
  allpages <- filter (isSuffixOf ".md") <$> map pack <$> listFiles ""
  htmlUrlResponse $ renderHtmlPage Nothing "All Pages" $ thehtml allpages

  where thehtml allpages mkurl = ul $ mapM_ (pageHtml mkurl) allpages
        pageHtml mkurl pageName = case toWikiPage . head $ splitOn "." pageName of
                                    Just wp -> li $ T.link mkurl (pageTextName wp) $ View wp Nothing
                                    Nothing -> T.empty

-- |Display all of the commits that have gone into a page, if
-- given. If not, show all changes globally. Takes an optional limit.
history :: Maybe WikiPage -> Maybe Int -> Handler Sitemap
history wp limit = do
  hist <- getHistory (wikipage <$> wp) limit
  htmlUrlResponse $
    case hist of
      Just events -> renderHist wp events
      Nothing     -> renderNoticePage "Error" $
                      case wp of
                        Just wp' -> "Failed to retrieve history for " <> pageNiceName wp' <> "."
                        Nothing  -> "Failed to retrieve history."

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = do
  changelog <- getDiff (wikipage wp) r1 r2
  htmlUrlResponse $
    case changelog of
      Just differences -> renderDiff wp r1 r2 differences
      _                -> err

  where err = renderNoticePage "Error" $ "Failed to retrieve diff for " <> pageNiceName wp <> " for " <> revisionShortId r1 <> "–" <> revisionShortId r2 <> "."

-----

-- |Render a wiki page, with a possible revision, displaying a
-- fallback handler if the page doesn't exist.
renderPage :: WikiPage -> Maybe Revision -> Handler Sitemap -> Handler Sitemap
renderPage wp r fallback = do
  fs       <- getFileStore
  mkurl    <- askMkUrl
  plugins  <- getPlugins
  wikiPage <- getStoredFileAt' (wikipage wp) r

  case wikiPage of
    Just cntnts -> renderWikiPageAt' wp r cntnts plugins fs mkurl >>= htmlResponse
    Nothing     -> fallback

-- |Render a new page, inviting users to create it.
renderNewPage :: WikiPage -> MkUrl Sitemap -> Html
renderNewPage wp = renderHtmlPage Nothing (pageNiceName wp) $ \mkurl -> do
                     _ <- "This page does not exist "
                     link' mkurl "Create New Page" "why not create it?" $ Edit wp

-- |Display a list of commits.
renderHist :: Maybe WikiPage -> [Commit] -> MkUrl Sitemap -> Html
renderHist wp commits = renderHtmlPage wp title $ \mkurl -> do
  section $ do
    ifPresent wp $
      h2 $ T.toHtml "Commits"

    hist wp commits mkurl

  ifPresent wp $ section $ do
      h2  $ T.toHtml "Diff"
      pre $ code ! A.id "diff" $ T.empty

  where title = case wp of
                  Just wp' -> pageNiceName wp' <> " history"
                  Nothing  -> "Recent Changes"

-- |Display a list of changes.
renderDiff :: WikiPage -> Revision -> Revision -> [Difference] -> MkUrl Sitemap -> Html
renderDiff wp r1 r2 thediff = renderHtmlPage (Just wp) thetitle . const . pre . code $ T.diff thediff
    where thetitle = pageNiceName wp <> " at " <> revisionShortId r1 <> "–" <> revisionShortId r2
