{-# LANGUAGE OverloadedStrings #-}

module Handler.Special where

import Routes
import Store
import Store.Paths
import Templates (renderBareMarkup)
import Types
import Web.Seacat (Handler, param', htmlResponse, textResponse, askMkUrl)

import qualified Templates.Utils as T

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
      htmlResponse $ maybe T.empty T.diff diff

    _ -> textResponse ""
