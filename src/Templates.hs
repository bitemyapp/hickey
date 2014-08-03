{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( renderWikiPage
    , renderWikiPageAt
    , renderNoticePage
    , renderHtmlPage
    , renderBareMarkup) where

import Control.Monad (when, unless)
import Data.Default (def)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, replace)
import Routes
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Types
import Web.Seacat

import qualified Templates.Utils             as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as B

-- |Render a wiki page (written in Markdown) to HTML.
renderWikiPage :: WikiPage
               -- ^The page
               -> Text
               -- ^The contents
               -> MkUrl Sitemap -> Html
renderWikiPage wp = applyHeaderAndFooter (Just wp) (pageTextName wp) . article . renderMarkdown

-- |Render a wiki page (written in Markdown) + revision information to HTML.
renderWikiPageAt :: WikiPage
                 -- ^The page
                 -> Revision
                 -- ^The revision
                 -> Text
                 -- ^The contents
                 -> MkUrl Sitemap -> Html
renderWikiPageAt wp r = applyHeaderAndFooter (Just wp) (pageTextName wp <> " at " <> revisionTextId r) . article . renderMarkdown

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
renderBareMarkup = renderMarkdownNoToC' . preprocess

-----

-- |Render some markdown to HTML, with a table of contents if there are any headings.
renderMarkdown :: Text -> Html
renderMarkdown md = let preprocessed = preprocess md
                    in renderMarkdownToToC' preprocessed >> renderMarkdownNoToC' preprocessed

-- |Render some markdown to HTML, without a table of contents.
-- TODO: Autolinking of WikiWords
-- TODO: Plugins
renderMarkdownNoToC :: Text -> Html
renderMarkdownNoToC = renderMarkdownNoToC' . preprocess

renderMarkdownNoToC' :: String -> Html
renderMarkdownNoToC' = writeHtml writerOptions . readMarkdown readerOptions
    where readerOptions = def
          writerOptions = def { writerSectionDivs = True
                              , writerHtml5       = True
                              , writerHighlight   = True
                              }

-- |Render some markdown to a table of contents, output is empty if
-- there are no headings.
renderMarkdownToToC :: Text -> Html
renderMarkdownToToC = renderMarkdownNoToC' . preprocess

renderMarkdownToToC' :: String -> Html
renderMarkdownToToC' md = let toc = writeHtml writerOptions $ readMarkdown readerOptions md
                          in unless (B.null toc) $
                               aside $
                                 nav ! A.id "toc" $ do
                                   h1 $ T.toHtml "Table of Contents"
                                   toc

    where readerOptions = def
          writerOptions = def { writerStandalone      = True
                              , writerTemplate        = "$toc$"
                              , writerTableOfContents = True
                              }

-- |Preprocess some wiki-markdown into regular markdown
-- TODO: Autolinking of WikiWords
-- TODO: Plugins
preprocess :: Text -> String
preprocess = unpack . replace "\r\n" "\n"

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
   H.link ! rel "stylesheet" ! type_ "text/css" ! href (textValue $ flip mkurl [] $ Static . fromJust . toFileName $ "style.css")

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
