{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( renderWikiPage
    , renderWikiPageAt
    , renderWikiPageAt'
    , renderNoticePage
    , renderHtmlPage
    , renderBareMarkup) where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.FileStore (FileStore)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Routes
import Templates.MarkdownToHtml
import Templates.Transformation
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Text.Pandoc.Definition (Pandoc)
import Types
import Web.Seacat

import qualified Templates.Utils             as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as B

-- |Render a wiki page (written in Markdown) to HTML.
renderWikiPage :: (Functor m, MonadIO m)
               => WikiPage
               -- ^The page
               -> Text
               -- ^The contents
               -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPage wp = renderWikiPageAt' wp Nothing

-- |Render a wiki page (written in Markdown) + revision information to HTML.
renderWikiPageAt :: (Functor m, MonadIO m)
                 => WikiPage
                 -- ^The page
                 -> Revision
                 -- ^The revision
                 -> Text
                 -- ^The contents
                 -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPageAt wp r = renderWikiPageAt' wp $ Just r

-- |Render a wiki page (possibly with revision info in title).
renderWikiPageAt' :: (Functor m, MonadIO m) => WikiPage -> Maybe Revision -> Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderWikiPageAt' wp r md plugins fs mkurl = do
  html <- article . writeDocument <$> readWiki md plugins fs mkurl
  return $ applyHeaderAndFooter (Just wp) title html mkurl

    where title = case r of
                    Just rev -> pageTextName wp <> " at " <> revisionShortId rev
                    Nothing  -> pageTextName wp

-- |Render a notice (written in plain text) to HTML.
renderNoticePage :: Text
                 -- ^Page title
                 -> Text
                 -- ^Notice text
                 -> MkUrl Sitemap -> Html
renderNoticePage title text = renderHtmlPage title $ const inner
    where inner = H.div ! class_ "notice" $ toHtml text

-- |Render some HTML into a full page.
renderHtmlPage :: Text
               -- ^Page title
               -> (MkUrl Sitemap -> Html)
               -- ^Body
               -> MkUrl Sitemap -> Html
renderHtmlPage title body mkurl = applyHeaderAndFooter Nothing title (body mkurl) mkurl

-- |Render just some markup, and don't wrap it in the header and
-- footer (eg, for a preview pane)
renderBareMarkup :: (Functor m, MonadIO m) => Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Html
renderBareMarkup md plugins fs mkurl = writeFragment <$> readWiki md plugins fs mkurl

-----

-- |Parse WikiMarkdown into regular Markdown and produce a Pandoc AST.
readWiki :: (Functor m, MonadIO m) => Text -> [Plugin] -> FileStore -> MkUrl Sitemap -> m Pandoc
readWiki md plugins fs mkurl = postprocess plugins fs mkurl . readMarkdown $ preprocess md

-- |Apply the header and footer to a rendered page.
applyHeaderAndFooter :: Maybe WikiPage
                     -- ^If Just, Edit/History/etc links will be displayed
                     -> Text
                     -- ^The title of the page
                     -> Html
                     -- ^Body
                     -> MkUrl Sitemap -> Html
applyHeaderAndFooter wp title html mkurl = docTypeHtml $ do
  H.head $ do
   H.title $ toHtml title
   H.link ! rel "stylesheet" ! type_ "text/css" ! href (sfile "style.css")
   script ! type_ "text/javascript" ! src (sfile "wiki.js") $ T.empty

  body $ do
    header $ do
      h1 $ toHtml title
      nav $
        ul $ do
          li $ T.link mkurl "FrontPage" $ View (fromJust $ toWikiPage "FrontPage") Nothing
          pageNav

    H.div ! class_ "container" $ html

  where pageNav = when (isJust wp) $ do
                    let wikiPage = fromJust wp

                    li $ T.link mkurl "Edit"    $ Edit    wikiPage
                    li $ T.link mkurl "History" $ History wikiPage
                    li $ T.link mkurl "Files"   $ Files   wikiPage

        sfile fn = textValue $ flip mkurl [] $ Static . fromJust . toFileName $ fn
