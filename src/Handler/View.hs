{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , pages
    , history
    , diff
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (isPrefixOf, append)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Handler.Utils
import Routes
import Store
import Store.Paths
import Templates
import Templates.Utils (link', hist, ifPresent, catlist)
import Text.Blaze.Html5 (Html, (!), section, h2, pre, code, ul, li)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Types
import Web.Seacat (Handler, MkUrl, RequestProcessor, askMkUrl, htmlResponse)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Data.Text                   as Te
import qualified Templates.Utils             as T
import qualified Text.Blaze.Html5.Attributes as A

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = renderPage wp Nothing $ renderNewPage wp

-- |Display a page as it was at the given revision. If the revision ID
-- is bad (doesn't exist or predates this page), an error is displayed
-- instead.
pageAtRevision :: WikiPage -> Revision -> Handler Sitemap
pageAtRevision wp r = renderPage wp (Just r) $ htmlUrlResponse err
    where err = renderNoticePage "Error" $ "Failed to retrieve " <> pageNiceName wp <> " at " <> revisionShortId r <> "."

-- |Display a list of all pages
pages :: Handler Sitemap
pages = do
  allpages <- listpages
  htmlUrlResponse $ renderHtmlPage Nothing "All Pages" $ thehtml allpages

  where thehtml allpages mkurl = ul $ mapM_ (pageHtml mkurl) allpages
        pageHtml mkurl wp = li $ T.link mkurl (pageTextName wp) $ View wp Nothing

-- |Display all of the commits that have gone into a page, if
-- given. If not, show all changes globally. Takes an optional limit.
history :: Maybe WikiPage -> Maybe Int -> Handler Sitemap
history wp limit = do
  hist   <- getHistory (wikipage <$> wp) limit
  locked <- case wp of
             Just wp' -> liftM (\lck -> Just (wp', lck)) $ isLocked wp'
             Nothing  -> return Nothing
  htmlUrlResponse $
    case hist of
      Just events -> renderHist locked events
      Nothing     -> renderNoticePage "Error" $
                      case wp of
                        Just wp' -> "Failed to retrieve history for " <> pageNiceName wp' <> "."
                        Nothing  -> "Failed to retrieve history."

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = do
  changelog <- getDiff (wikipage wp) r1 r2
  locked    <- isLocked wp
  htmlUrlResponse $
    case changelog of
      Just differences -> renderDiff wp locked r1 r2 differences
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
  locked   <- isLocked wp

  -- Category pages have a list of their contents at the bottom
  cattxt <- toStrict . decodeUtf8 . renderHtml <$> catHtml wp mkurl

  case wikiPage of
    Just cntnts -> renderWikiPageAt' wp locked r (cntnts `append` cattxt) plugins fs mkurl >>= htmlResponse
    Nothing     -> fallback

-- |Render a new page, inviting users to create it.
renderNewPage :: WikiPage -> Handler Sitemap
renderNewPage wp = do
  mkurl   <- askMkUrl
  cathtml <- catHtml wp mkurl

  htmlUrlResponse $ renderHtmlPage Nothing (pageNiceName wp) $ const $ do
    T.toHtml "This page does not exist "
    link' mkurl "Create New Page" "why not create it?" $ Edit wp
    cathtml

-- |Get a list of the pages in a category
catHtml :: WikiPage -> MkUrl Sitemap -> RequestProcessor Sitemap Html
catHtml wp mkurl = if "Category" `isPrefixOf` pageTextName wp
                   then flip catlist mkurl <$> categoryPages catname
                   else return T.empty
    where catname = fromJust . toWikiPage . Te.drop (Te.length "Category") $ pageTextName wp

-- |Display a list of commits.
renderHist :: Maybe (WikiPage, Bool) -> [Commit] -> MkUrl Sitemap -> Html
renderHist wp commits = renderHtmlPage wp title $ \mkurl -> do
  section $ do
    ifPresent wp $
      h2 $ T.toHtml "Commits"

    hist (fst <$> wp) commits mkurl

  ifPresent wp $ section $ do
      h2  $ T.toHtml "Diff"
      pre $ code ! A.id "diff" $ T.empty

  where title = case fst <$> wp of
                  Just wp' -> pageNiceName wp' <> " history"
                  Nothing  -> "Recent Changes"

-- |Display a list of changes.
renderDiff :: WikiPage -> Bool -> Revision -> Revision -> [Difference] -> MkUrl Sitemap -> Html
renderDiff wp locked r1 r2 thediff = renderHtmlPage (Just (wp, locked)) thetitle . const . pre . code $ T.diff thediff
    where thetitle = pageNiceName wp <> " at " <> revisionShortId r1 <> "–" <> revisionShortId r2
