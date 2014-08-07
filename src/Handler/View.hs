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
import Data.Time.Format (formatTime)
import Routes
import Store
import Store.Paths
import System.Locale (defaultTimeLocale)
import Templates
import Templates.Utils (link', with)
import Text.Blaze.Html5 hiding (head, map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Types
import Web.Seacat (Handler, MkUrl, askMkUrl, htmlResponse)
import Web.Seacat.RequestHandler (htmlUrlResponse)

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
                        Just wp' -> "Failed to retrieve history for " <> pageTextName wp' <> "."
                        Nothing  -> "Failed to retrieve history."

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
renderNewPage wp = renderHtmlPage Nothing (pageTextName wp) $ \mkurl -> do
                     _ <- "This page does not exist "
                     link' mkurl "Create New Page" "why not create it?" $ Edit wp

-- |Display a list of commits.
renderHist :: Maybe WikiPage -> [Commit] -> MkUrl Sitemap -> Html
renderHist wp hist = renderHtmlPage wp title $ \mkurl -> do
  section $ do
    with wp $ const $ h2 $ T.toHtml "Commits"
    table ! class_ "history" $
      mapM_ (trow mkurl) hist

  with wp $
    const $ section $ do
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
                 case wp of
                   Just wp' -> td $ do
                     T.link mkurl revid' $ View wp' . Just $ commitRevision commit
                     compare "r1" revid wp'
                     compare "r2" revid wp'

                   Nothing  -> do
                     td $ case commitTarget commit of
                            Just (wp', Nothing) -> T.link mkurl (pageTextName wp') $ View wp' Nothing
                            Just (wp', Just fn) -> do
                              T.link mkurl (pageTextName wp') $ View wp' Nothing
                              br
                              T.link mkurl (fileTextName fn) $ File wp' fn Nothing

                     td $ T.link mkurl revid' $ commitLink commit

        title = case wp of
                  Just wp' -> pageTextName wp' <> " History"
                  Nothing  -> "Recent Changes"

        compare rn revid wp' = td $ H.input ! type_ "radio" ! name (textValue rn) ! value (textValue revid) ! onclick (textValue $ "set" <> rn <> "('" <> pageTextName wp' <> "','" <> revid <> "')")

        commitLink commit = case commitTarget commit of
                              Just (wp', Nothing) -> View wp'    . Just $ commitRevision commit
                              Just (wp', Just fn) -> File wp' fn . Just $ commitRevision commit

-- |Display a list of changes.
renderDiff :: WikiPage -> Revision -> Revision -> [Difference] -> Handler Sitemap
renderDiff wp r1 r2 thediff = do
  let thetitle = pageTextName wp <> " at " <> revisionShortId r1 <> "–" <> revisionShortId r2
  htmlUrlResponse . renderHtmlPage (Just wp) thetitle . const . pre . code $ T.diff thediff
