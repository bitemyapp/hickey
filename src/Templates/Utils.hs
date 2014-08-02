{-# LANGUAGE OverloadedStrings #-}

module Templates.Utils
    ( link
    , link'

    , input
    , input'

    , toHtml
    , empty) where

import Data.Text (Text)
import Routes (Sitemap)
import Text.Blaze.Html5 (Html, (!), a, label)
import Text.Blaze.Html5.Attributes (for, href, name, required, title, type_)
import Text.Blaze.Internal (textValue)
import Web.Seacat (MkUrl)

import qualified Text.Blaze.Html5 as H

-- |Render a link to HTML, where the inner text is the same as the
-- title.
link :: MkUrl Sitemap
     -- ^The URL renderer
     -> Text
     -- ^The title
     -> Sitemap
     -- ^The target
     -> Html
link mkurl ttl = link' mkurl ttl ttl

-- |Render a link to HTML, where the inner text is different to the
-- title.
link' :: MkUrl Sitemap
      -- ^The URL renderer
      -> Text
      -- ^The title
      -> Text
      -- ^The inner text
      -> Sitemap
      -- ^The target
      -> Html
link' mkurl ttl text target = a ! href (textValue $ mkurl target []) ! title (textValue ttl) $ toHtml text

-- |Render an optional input element with a label.
input :: Text
      -- ^The label text
      -> Text
      -- ^The field name
      -> Text
      -- ^The field type
      -> Html
input lbl nam typ = do
  label ! for (textValue nam) $ toHtml lbl
  H.input ! name (textValue nam) ! type_ (textValue typ)

-- |Render a required input element with a label.
input' :: Text
       -- ^The label text
       -> Text
       -- ^The field name
       -> Text
       -- ^The field type
       -> Html
input' lbl nam typ = do
  label ! for (textValue nam) $ toHtml lbl
  H.input ! name (textValue nam) ! type_ (textValue typ) ! required (textValue "required")

-- |A specialised toHtml, to get around the issues caused by
-- OverloadedStrings.
toHtml :: Text -> Html
toHtml = H.toHtml

-- |Some empty HTML
empty :: Html
empty = toHtml ""
