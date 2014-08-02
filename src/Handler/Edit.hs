{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( edit
    , commit
    , preview) where

import Data.Maybe (fromMaybe)
import Templates
import Types
import Routes
import Web.Seacat

-- |Display an edit form for a page.
edit :: WikiPage -> Handler Sitemap
edit wp = undefined

-- |Save an edit, possibly uploading some new attachments in the
-- process.
commit :: WikiPage -> Handler Sitemap
commit wp = undefined

-- |Render a preview of some posted markup.
preview :: Handler Sitemap
preview = param "markup" >>= htmlResponse . renderBareMarkup . fromMaybe ""
