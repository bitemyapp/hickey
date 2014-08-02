module Templates.Utils
    ( link
    , link') where

import Data.Text (Text)
import Routes (Sitemap)
import Text.Blaze.Html5 (Html, (!), a, toHtml)
import Text.Blaze.Html5.Attributes (href, title)
import Text.Blaze.Internal (textValue)
import Web.Seacat (MkUrl)

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
