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
import Data.Text (Text, unpack)
import Routes
import Templates.MarkdownToHtml
import Templates.Transformation
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
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
renderWikiPage wp = renderWikiPage' wp Nothing

-- |Render a wiki page (written in Markdown) + revision information to HTML.
renderWikiPageAt :: WikiPage
                 -- ^The page
                 -> Revision
                 -- ^The revision
                 -> Text
                 -- ^The contents
                 -> MkUrl Sitemap -> Html
renderWikiPageAt wp r = renderWikiPage' wp $ Just r

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
renderBareMarkup = writeFragment . postprocess . readMarkdown . preprocess

-----

-- |Render a wiki page (possibly with revision info in title).
renderWikiPage' :: WikiPage -> Maybe Revision -> Text -> MkUrl Sitemap -> Html
renderWikiPage' wp r = applyHeaderAndFooter (Just wp) title . article . writeDocument . postprocess . readMarkdown . preprocess

    where title = case r of
                    Just rev -> pageTextName wp <> " at " <> revisionShortId rev
                    Nothing  -> pageTextName wp

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
