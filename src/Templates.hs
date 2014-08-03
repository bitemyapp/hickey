{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( renderWikiPage
    , renderWikiPageAt
    , renderNoticePage
    , renderHtmlPage
    , renderBareMarkup) where

import Control.Monad (when)
import Data.Default (def)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, replace)
import Routes
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Types
import Web.Seacat

import qualified Templates.Utils as T
import qualified Text.Blaze.Html5 as H

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

-- |Render just some markup, and don't wrap it in the header and
-- footer (eg, for a preview pane)
renderBareMarkup :: Text
                 -- ^The markup
                 -> Html
renderBareMarkup = renderMarkdown

-----

-- |Render some markdown to HTML.
-- TODO: Autolinking of WikiWords
-- TODO: Plugins
renderMarkdown :: Text -> Html
renderMarkdown = writeHtml writerOptions . readMarkdown readerOptions . unpack . replace "\r\n" "\n"
    where readerOptions = def
          writerOptions = def { -- In order to include a table of
                                -- contents, the writer must be in
                                -- "standalone" mode - but we don't
                                -- want the default header and footer,
                                -- just the toc.
                                --
                                -- We also use this opportunity to
                                -- make semantically nicer output,
                                -- too.
                                writerStandalone = True
                              , writerTemplate = unlines [ "<section id=\"toc\">"
                                                         , "<h1>Table of Contents</h1>"
                                                         , "$toc$"
                                                         , "</section>"
                                                         , "<section id=\"article\">"
                                                         , "$body$"
                                                         , "</section>"
                                                         ]
                              , writerTableOfContents = True
                              , writerSectionDivs = True

                                -- The rest of the options are fairly
                                -- self-explanatory
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
    -- Load some CSS stylesheets and magical javascript here
  body $ do
    header $ do
      h1 $ toHtml title
      nav $ do
        li $ T.link mkurl "FrontPage" $ View (fromJust $ toWikiPage "FrontPage") Nothing
        pageNav

    article html

  where pageNav = when (isJust wp) $ do
                    let wikiPage = fromJust wp

                    li $ T.link mkurl "Edit"    $ Edit    wikiPage
                    li $ T.link mkurl "History" $ History wikiPage
                    li $ T.link mkurl "Files"   $ Files   wikiPage Nothing
