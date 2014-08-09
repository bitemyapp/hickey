{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( renderWikiPage
    , renderWikiPageAt
    , renderWikiPageAt'
    , renderNoticePage
    , renderHtmlPage
    , renderBareMarkup) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.FileStore (FileStore)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Routes
import Templates.MarkdownToHtml
import Templates.Transformation
import Templates.Utils (with)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Text.Pandoc.Definition (Pandoc)
import Types
import Web.Seacat

import qualified Templates.Utils  as T
import qualified Text.Blaze.Html5 as H

-- |Render a wiki page (written in Markdown) to HTML.
renderWikiPage :: (Functor m, MonadIO m)
               => WikiPage
               -- ^The page
               -> Bool
               -- ^Whether the page is protected
               -> Text
               -- ^The contents
               -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPage wp protected = renderWikiPageAt' wp protected Nothing

-- |Render a wiki page (written in Markdown) + revision information to HTML.
renderWikiPageAt :: (Functor m, MonadIO m)
                 => WikiPage
                 -- ^The page
                 -> Bool
                 -- ^Whether the page is protected
                 -> Revision
                 -- ^The revision
                 -> Text
                 -- ^The contents
                 -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPageAt wp protected r = renderWikiPageAt' wp protected $ Just r

-- |Render a wiki page (possibly with revision info in title).
renderWikiPageAt' :: (Functor m, MonadIO m) => WikiPage -> Bool -> Maybe Revision -> Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPageAt' wp protected r md plugins fs mkurl = do
  thehtml <- article . writeDocument <$> readWiki md plugins fs mkurl
  return $ renderHtmlPage (Just (wp, protected)) thetitle (const thehtml) mkurl

    where thetitle = case r of
                       Just rev -> pageNiceName wp <> " at " <> revisionShortId rev
                       Nothing  -> pageNiceName wp

-- |Render a notice (written in plain text) to HTML.
renderNoticePage :: Text
                 -- ^Page title
                 -> Text
                 -- ^Notice text
                 -> MkUrl Sitemap -> Html
renderNoticePage thetitle text = renderHtmlPage Nothing thetitle $ const inner
    where inner = H.div ! class_ "notice" $ toHtml text

-- |Render some HTML into a full page.
renderHtmlPage :: Maybe (WikiPage, Bool)
               -- ^If present, display the page navigation (bool is
               -- whether it's protected).
               -> Text
               -- ^Page title
               -> (MkUrl Sitemap -> Html)
               -- ^Body
               -> MkUrl Sitemap -> Html
renderHtmlPage wp thetitle thebody mkurl = applyHeaderAndFooter wp thetitle (thebody mkurl) mkurl

-- |Render just some markup, and don't wrap it in the header and
-- footer (eg, for a preview pane)
renderBareMarkup :: (Functor m, MonadIO m) => Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderBareMarkup md plugins fs mkurl = writeFragment <$> readWiki md plugins fs mkurl

-----

-- |Parse WikiMarkdown into regular Markdown and produce a Pandoc AST.
readWiki :: (Functor m, MonadIO m) => Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Pandoc
readWiki md plugins fs mkurl = postprocess plugins fs mkurl . readMarkdown $ preprocess md

-- |Apply the header and footer to a rendered page.
applyHeaderAndFooter :: Maybe (WikiPage, Bool)
                     -- ^If Just, History/Files links will be
                     -- displayed. If True, the page is protected and
                     -- the edit link will *not* be displayed.
                     -> Text
                     -- ^The title of the page
                     -> Html
                     -- ^Body
                     -> MkUrl Sitemap -> Html
applyHeaderAndFooter wp thetitle thehtml mkurl = docTypeHtml $ do
  H.head $ do
   H.title $ toHtml thetitle
   H.link ! rel "stylesheet" ! type_ "text/css" ! href (sfile "style.css")
   scripts [ sfile "wiki.js"
           , "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
           ]

  body $ do
    header $ do
      h1 $ toHtml thetitle
      nav $
        ul $ do
          li $ T.link mkurl "Front Page"     FrontPage
          li $ T.link mkurl "All Pages"      AllPages
          li $ T.link mkurl "Recent Changes" RecentChanges

          pageNav

    H.div ! class_ "container" $ thehtml

  where pageNav = with wp $ \(wikiPage, protected) -> do
                    unless protected $ li $ T.link mkurl "Edit" $ Edit wikiPage

                    li $ T.link mkurl "History" $ History wikiPage
                    li $ T.link mkurl "Files"   $ Files   wikiPage

        sfile fn = textValue $ flip mkurl [] $ Static . fromJust . toFileName $ fn

        scripts = mapM_ $ \uri -> script ! type_ "text/javascript" ! src uri $ T.empty
