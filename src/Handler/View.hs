{-# LANGUAGE OverloadedStrings #-}

module Handler.View
    ( page
    , pageAtRevision
    , history
    , diff
    ) where

import Data.Monoid ((<>))
import Data.Time.Format (formatTime)
import Routes
import Store
import Store.Paths
import System.Locale (defaultTimeLocale)
import Templates
import Templates.Utils
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Types
import Web.Seacat (Handler, MkUrl, askMkUrl, htmlResponse)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Data.Text                   as Te
import qualified Templates.Utils             as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- |Display a page as it is now.
page :: WikiPage -> Handler Sitemap
page wp = renderPage wp Nothing $ htmlUrlResponse $ renderNewPage wp

-- |Display a page as it was at the given revision. If the revision ID
-- is bad (doesn't exist or predates this page), an error is displayed
-- instead.
pageAtRevision :: WikiPage -> Revision -> Handler Sitemap
pageAtRevision wp r = renderPage wp (Just r) $ htmlUrlResponse err
    where err = renderNoticePage "Error" $ "Failed to retrieve " <> pageTextName wp <> " at " <> revisionShortId r <> "."

-- |Display all of the commits that have gone into a page.
history :: WikiPage -> Handler Sitemap
history wp = do
  hist <- getHistory $ wikipage wp
  htmlUrlResponse $
    case hist of
      Just events -> renderHist wp events
      _           -> renderNoticePage "Error" $ "Failed to retrieve history for " <> pageTextName wp <> "."

-- |Display the diff between the two revisions. If either revision is
-- bad, display an error instead.
diff :: WikiPage -> Revision -> Revision -> Handler Sitemap
diff wp r1 r2 = do
  changelog <- getDiff (wikipage wp) r1 r2
  case changelog of
    Just differences -> renderDiff wp r1 r2 differences
    _                -> htmlUrlResponse err

  where err = renderNoticePage "Error" $ "Failed to retrieve diff for " <> pageTextName wp <> " for " <> revisionShortId r1 <> "–" <> revisionShortId r2 <> "."

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
renderNewPage wp = renderHtmlPage (pageTextName wp) $ \mkurl -> do
                     _ <- "This page does not exist "
                     link' mkurl "Create New Page" "why not create it?" $ Edit wp

-- |Display a list of commits.
renderHist :: WikiPage -> [Commit] -> MkUrl Sitemap -> Html
renderHist wp hist = renderHtmlPage (pageTextName wp <> " History") $ \mkurl -> do
  section $ do
    h2 $ T.toHtml "Commits"
    table ! class_ "history" $
      mapM_ (trow mkurl) hist

  section $ do
    h2  $ T.toHtml "Diff"
    pre $ code ! A.id "diff" $ T.empty

  where trow mkurl commit =
            let revid  = revisionTextId  $ commitRevision commit
                revid' = revisionShortId $ commitRevision commit
                when   = commitTime commit
                who    = commitAuthor commit
                why    = commitMessage commit
            in tr $ do
                 td $ do
                   h2 $ T.toHtml why
                   p $ do
                     H.span ! class_ "author" $ T.toHtml who
                     T.toHtml " authored at "
                     H.span ! class_ "when" $ H.toHtml $ formatTime defaultTimeLocale "%R (%F)" when
                 td $
                   T.link mkurl revid' $ View wp . Just $ commitRevision commit
                 td $ H.input ! type_ "radio" ! name "r1" ! value (textValue revid) ! onclick (textValue $ "setr1('" <> pageTextName wp <> "','" <> revid <> "')")
                 td $ H.input ! type_ "radio" ! name "r2" ! value (textValue revid) ! onclick (textValue $ "setr2('" <> pageTextName wp <> "','" <> revid <> "')")

-- |Display a list of changes.
renderDiff :: WikiPage -> Revision -> Revision -> [Difference] -> Handler Sitemap
renderDiff wp r1 r2 thediff = do
  let thetitle = pageTextName wp <> " at " <> revisionShortId r1 <> "–" <> revisionShortId r2
  htmlUrlResponse . renderHtmlPage thetitle . const . pre . code $ mapM_ render thediff

  where render (First  ls)   = H.span ! class_ "first"  $ T.toHtml $ Te.unlines ls
        render (Second ls)   = H.span ! class_ "second" $ T.toHtml $ Te.unlines ls
        render (Both   ls _) = H.span ! class_ "both"   $ T.toHtml $ Te.unlines ls
