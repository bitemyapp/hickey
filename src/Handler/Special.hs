{-# LANGUAGE OverloadedStrings #-}

module Handler.Special
    ( preview
    , plaindiff) where

import Routes
import Store
import Store.Paths
import Templates (renderBareMarkup)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Types
import Web.Seacat (Handler, param', htmlResponse, textResponse, askMkUrl)

import qualified Data.Text        as Te
import qualified Templates.Utils  as T
import qualified Text.Blaze.Html5 as H

-- |Render a preview of some posted markup.
preview :: Handler Sitemap
preview = do
  fs      <- getFileStore
  mkurl   <- askMkUrl
  markup  <- param' "markup" ""
  plugins <- getPlugins
  thehtml <- renderBareMarkup markup plugins fs mkurl

  htmlResponse thehtml

-- |Render a diff of two revisions
plaindiff :: Handler Sitemap
plaindiff = do
  wp <- param' "wp" ""
  r1 <- param' "r1" ""
  r2 <- param' "r2" ""

  case (toWikiPage wp, toRevision r1, toRevision r2) of
    (Just wikiPage, Just rev1, Just rev2) -> do
      diff <- getDiff (wikipage wikiPage) rev1 rev2
      htmlResponse $ render diff

    _ -> textResponse ""

  where render (Just diff) = mapM_ render' diff
        render Nothing     = T.empty

        render' (First  ls)   = H.span ! class_ "first"  $ toHtml $ Te.unlines ls
        render' (Second ls)   = H.span ! class_ "second" $ toHtml $ Te.unlines ls
        render' (Both   ls _) = H.span ! class_ "both"   $ toHtml $ Te.unlines ls
