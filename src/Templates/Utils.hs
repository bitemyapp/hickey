{-# LANGUAGE OverloadedStrings #-}

module Templates.Utils where

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Routes (Sitemap)
import Store.Types (Difference, Diff(..))
import Text.Blaze.Html5 (Html, (!), a, label, fieldset, ol, li)
import Text.Blaze.Html5.Attributes (href, title, for, name, type_, required, class_, method, action, enctype, value)
import Text.Blaze.Internal (textValue)
import Web.Seacat (MkUrl)

import qualified Data.Text        as Te
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
  H.input ! name (textValue nam) ! type_ (textValue typ) ! required "required"

-- |Render a diff.
diff :: [Difference] -> Html
diff = mapM_ step
    where step (First  ls)   = H.span ! class_ "first"  $ toHtml $ Te.unlines ls
          step (Second ls)   = H.span ! class_ "second" $ toHtml $ Te.unlines ls
          step (Both   ls _) = H.span ! class_ "both"   $ toHtml $ Te.unlines ls

-- |Simpler version of `form'` with no excess HTML.
form :: Sitemap -> [(Html, Maybe Text)] -> Text -> Maybe Text -> Maybe Text -> MkUrl Sitemap -> Html
form target inputs submit err help = form' target inputs submit err help Nothing Nothing

-- |Render a POST form, with optional error text, explanation text, and extra HTML.
form' :: Sitemap
      -- ^Target
      -> [(Html, Maybe Text)]
      -- ^Input elements, with optional default values
      -> Text
      -- ^Caption of submit button
      -> Maybe Text
      -- ^Possible error message
      -> Maybe Text
      -- ^Possible explanation text
      -> Maybe Html
      -- ^Possible HTML between the error and the form. Note that this
      -- is placed in a block context.
      -> Maybe Html
      -- ^Possible HTML in the form, after the submit button. Note that
      -- this is placed in a list context.
      -> MkUrl Sitemap -> Html
form' target inputs submit err help before after mkurl = do
  when (isJust err) $
    H.div ! class_ "error" $ toHtml (fromJust err)

  when (isJust before) $ fromJust before

  H.form ! method "post" ! action (textValue $ mkurl target []) ! enctype "multipart/form-data" $
    fieldset $
      ol $ do
        mapM_ renderEle inputs
        li $ H.input ! type_ "submit" ! value (textValue submit)
        when (isJust after) $ fromJust after

  when (isJust help) $
    H.p $ toHtml $ fromJust help

  where renderEle (ele, Just def) = li $ ele ! value (textValue def)
        renderEle (ele, Nothing)  = li ele

-- |A specialised toHtml, to get around the issues caused by
-- OverloadedStrings.
toHtml :: Text -> Html
toHtml = H.toHtml

-- |Some empty HTML
empty :: Html
empty = toHtml ""
