{-# LANGUAGE OverloadedStrings #-}

module Templates.Utils where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Format (formatTime)
import Routes
import Store.Types (Commit(..), Difference, Diff(..))
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!), a, label, fieldset, ol, li, table, tr, td, h2, p, br)
import Text.Blaze.Html5.Attributes (href, title, for, name, type_, required, class_, method, action, enctype, value, onclick)
import Text.Blaze.Internal (Attribute, Attributable, AttributeValue, textValue)
import Types
import Web.Seacat (MkUrl)

import qualified Data.Text                   as Te
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- *Simple elements

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

-- *Complex elements

-- |Render a diff.
diff :: [Difference] -> Html
diff = mapM_ step
    where step (First  ls)   = H.span ! class_ "first"  $ toHtml $ Te.unlines ls
          step (Second ls)   = H.span ! class_ "second" $ toHtml $ Te.unlines ls
          step (Both   ls _) = H.span ! class_ "both"   $ toHtml $ Te.unlines ls

-- |Render a history table
hist :: Maybe WikiPage -> [Commit] -> MkUrl Sitemap -> Html
hist wp commits mkurl = table ! class_ "history" $ mapM_ trow commits
    where trow commit = let revid  = revisionTextId  $ commitRevision commit
                            revid' = revisionShortId $ commitRevision commit
                            when   = commitTime    commit
                            who    = commitAuthor  commit
                            why    = commitMessage commit
                        in tr $ do
                          -- Display summary of commit.
                          td $ do
                            h2 $ toHtml why
                            p $ do
                              H.span ! class_ "author" $ toHtml who
                              toHtml " authored at "
                              H.span ! class_ "when"   $ (H.toHtml $ formatTime defaultTimeLocale "%R (%F)" when :: Html)

                          -- Display page/file if there is no specific page.
                          ifNotPresent wp $ td $
                            let (wp', fn) = commitTarget commit
                            in do
                              -- Always link to the page.
                              link mkurl (pageTextName wp') $ View wp' Nothing

                              -- And the file too, if there is one.
                              with fn $ \fn' -> do
                                br
                                link mkurl (fileTextName fn') $ File wp' fn' Nothing

                          -- Link to revision.
                          td $ link mkurl revid' $
                            case commitTarget commit of
                              (wp', Just fn) -> File wp' fn . Just $ commitRevision commit
                              (wp', Nothing) -> View wp'    . Just $ commitRevision commit

                          -- If this is about a specific page, display comparison radios.
                          with wp $ \wp' -> do
                            td $ compare "r1" revid wp'
                            td $ compare "r2" revid wp'

          compare rn revid wikiPage = H.input ! type_   "radio"
                                              ! name    (textValue rn)
                                              ! value   (textValue revid)
                                              ! onclick (textValue $ "set" <> rn <> "('" <> pageTextName wikiPage <> "','" <> revid <> "')")

-- |Simpler version of `form'` with no excess HTML.
form :: Sitemap -> [(Html, Maybe Text)] -> Text -> Maybe Text -> Maybe Text -> MkUrl Sitemap -> Html
form target inputs submit err help = form' Nothing target inputs submit err help Nothing Nothing

-- |Render a POST form, with optional error text, explanation text, and extra HTML.
form' :: Maybe Text
      -- ^Optional form ID
      -> Sitemap
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
form' fid target inputs submit err help before after mkurl = do
  with err $ (H.div ! class_ "error") . toHtml

  extract before

  H.form ! method "post" ! action (textValue $ mkurl target []) ! enctype "multipart/form-data" !?? (A.id, fid) $
    fieldset $
      ol $ do
        mapM_ renderEle inputs
        li $ H.input ! type_ "submit" ! value (textValue submit)
        extract after

  with help $ H.p . toHtml

  where renderEle (ele, Just def) = li $ ele ! value (textValue def)
        renderEle (ele, Nothing)  = li ele

-- *Combinators

-- |A specialised toHtml, to get around the issues caused by
-- OverloadedStrings.
toHtml :: Text -> Html
toHtml = H.toHtml

-- |Some empty HTML
empty :: Html
empty = toHtml ""

-- |Render some html conditional on a value being present
with :: Maybe a -> (a -> Html) -> Html
with (Just x) f = f x
with Nothing  _ = empty

-- |Display a value in a Maybe, if it's there.
extract :: Maybe Html -> Html
extract = flip with id

-- |Display some HTML if there is a value in the Maybe, but don't care
-- about what the value actually is.
ifPresent :: Maybe a -> Html -> Html
ifPresent (Just _) h = h
ifPresent Nothing  _ = empty

-- |Display some HTML if there is not a value in the Maybe, but don't
-- care about what the value actually is.
ifNotPresent :: Maybe a -> Html -> Html
ifNotPresent (Just _) _ = empty
ifNotPresent Nothing  h = h

-- |Set an attribute if a Maybe is Just.
(!??) :: Attributable h => h -> (AttributeValue -> Attribute, Maybe Text) -> h
h !?? (a, Just v)  = h ! a (textValue v)
h !?? (_, Nothing) = h
