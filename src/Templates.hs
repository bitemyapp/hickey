{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( renderWikiPage
    , renderWikiPageAt
    , renderNoticePage
    , renderHtmlPage) where

import Control.Monad (when)
import Data.Default (def)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Routes
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Types
import Web.Seacat

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- |Render a wiki page (written in Markdown) to HTML.
renderWikiPage :: WikiPage
               -- ^The page
               -> Text
               -- ^The contents
               -> MkUrl Sitemap -> Html
renderWikiPage wp = applyHeaderAndFooter (Just wp) (pageTextName wp) . renderMarkdown

-- |Render a wiki page (written in Markdown) + revision information to HTML.
renderWikiPageAt :: WikiPage
                 -- ^The page
                 -> Revision
                 -- ^The revision
                 -> Text
                 -- ^The contents
                 -> MkUrl Sitemap -> Html
renderWikiPageAt wp r = applyHeaderAndFooter (Just wp) (pageTextName wp <> " at " <> revisionTextId r) . renderMarkdown

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

-----

-- |Render some markdown to HTML.
-- TODO: Autolinking of WikiWords
-- TODO: Plugins
renderMarkdown :: Text -> Html
renderMarkdown = writeHtml writerOptions . readMarkdown readerOptions . unpack
    where readerOptions = def
          writerOptions = def { writerTableOfContents = True
                              , writerHtml5 = True
                              , writerHighlight = True
                              }

-- |Apply the header and footer to a rendered page.
applyHeaderAndFooter :: Maybe WikiPage
                     -- ^If Just, Edit/History/etc links will be displayed
                     -> Text
                     -- ^The title of the page
                     -> Html
                     -- ^Body
                     -> MkUrl Sitemap -> Html
applyHeaderAndFooter wp title html mkurl = docTypeHtml $ do
  H.head $
   H.title $ toHtml title
    -- Load some CSS stylesheets here
  body $ do
    header $ do
      h1 $ toHtml title
      nav $ do
        li $ link "FrontPage" $ View (fromJust $ toWikiPage "FrontPage") Nothing
        pageNav

    H.div ! class_ "page" $ html

  where link title target = a ! href (textValue $ mkurl target []) ! A.title (textValue title) $ toHtml title

        pageNav = when (isJust wp) $ do
                    let wikiPage = fromJust wp

                    li $ link "Edit"    $ Edit    wikiPage
                    li $ link "History" $ History wikiPage
                    li $ link "Files"   $ Files   wikiPage Nothing
