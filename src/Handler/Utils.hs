{-# LANGUAGE OverloadedStrings #-}

-- |Shared utilities for request handlers.
module Handler.Utils where

import Control.Applicative ((<$>))
import Control.Monad (liftM, filterM)
import Data.Default (def)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (unpack, breakOn, pack, strip, replace)
import Routes
import Store (getStoredFile, listFiles)
import Store.Paths (wikipage)
import Templates (renderNoticePage)
import Text.Pandoc.Definition (Pandoc(..), Meta, MetaValue(..), Inline(..), lookupMeta)
import Text.Pandoc.Options ()
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Types
import Web.Seacat (Handler, RequestProcessor)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import Control.Monad.IO.Class

-- * Page protection

-- |Determine if a page is locked from editing.
isLocked :: WikiPage -> RequestProcessor Sitemap Bool
isLocked wp = liftM check $ getStoredFile "locked.conf"
    where check (Just ps) = any (`elem` ["*", pageTextName wp]) ps

          -- If there was an error retrieving the file, it's probably
          -- not present - in which case nothing is protected.
          --
          -- It could also indicate a problem with the store, but in
          -- that case edits probably wouldn't get saved anyway.
          check Nothing = False

-- |Run the provided handler if the page is unlocked, otherwise
-- display an error page.
protect :: WikiPage -> Handler Sitemap -> Handler Sitemap
protect wp h = do
  locked <- isLocked wp
  if locked
  then htmlUrlResponse $ renderNoticePage "Locked" $ pageTextName wp <> " is locked for editing."
  else h

-- *Categorisation

-- |Get the categories a page belongs to. The categories of a page are
-- specified in the "category" or "categories" field in the YAML
-- front-matter.
categories :: WikiPage -> RequestProcessor Sitemap [Category]
categories wp = do
  wikiPage <- getStoredFile $ wikipage wp
  return $
    case wikiPage of
      Just wp' -> let (Pandoc meta _) = readMarkdown def $ unpack wp'
                      category        = categoriesFrom <$> lookupMeta "category" meta
                      categories      = categoriesFrom <$> lookupMeta "categories" meta
                 in concat $ maybeToList category ++ maybeToList categories
      Nothing  -> []

  where categoriesFrom (MetaList ms)    = concatMap categoriesFrom ms
        categoriesFrom (MetaInlines ms) = concatMap categoriesFrom' ms
        categoriesFrom (MetaString str) = toCat str

        categoriesFrom' (Str str) = toCat str
        categoriesFrom' _         = []

        toCat = maybeToList . toWikiPage . strip . replace "," "" . pack

-- |Get the pages which belong to a category.
categoryPages :: Category -> RequestProcessor Sitemap [WikiPage]
categoryPages cat = listpages >>= filterM inCat
    where inCat wp = elem cat <$> categories wp

-- *Misc

-- |Get all pages
listpages :: RequestProcessor Sitemap [WikiPage]
listpages = concatMap (toWP . pack) <$> listFiles ""
    where toWP wp = case breakOn "." wp of
                      (wp', ".md") -> maybeToList $ toWikiPage wp'
                      _            -> []
