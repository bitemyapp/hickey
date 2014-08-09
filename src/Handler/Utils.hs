{-# LANGUAGE OverloadedStrings #-}

-- |Shared utilities for request handlers.
module Handler.Utils where

import Control.Monad (liftM)
import Data.Monoid ((<>))
import Routes
import Store (getStoredFile)
import Templates (renderNoticePage)
import Types
import Web.Seacat (Handler, RequestProcessor)
import Web.Seacat.RequestHandler (htmlUrlResponse)

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
