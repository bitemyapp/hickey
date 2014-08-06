{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import Data.Text (pack)
import Handler.Edit
import Handler.Special
import Handler.Static
import Handler.View
import Network.HTTP.Types (StdMethod(..))
import Types
import Routes
import Web.Seacat (Handler, seacat, defaultSettings, redirect)
import Web.Seacat.RequestHandler (textResponse)

-- |Start up the web server with default settings.
main :: IO ()
main = seacat route error500 defaultSettings

-- |Report 404 errors to the user.
-- TODO: Prettify.
error404 :: String -> Handler Sitemap
error404 = textResponse . pack

-- |Report 500 errors to the user.
-- TODO: Prettify
error500 :: String -> Handler Sitemap
error500 = textResponse . pack

-- |Route a request to its handler.
route :: StdMethod -> Sitemap -> Handler Sitemap
route GET  FrontPage                = redirect $ View (fromJust $ toWikiPage "FrontPage") Nothing
route GET  AllPages                 = pages
route GET  RecentChanges            = history Nothing $ Just 64
route GET  (View    wp Nothing)     = page wp
route GET  (View    wp (Just r))    = pageAtRevision wp r
route GET  (Edit    wp)             = edit wp
route GET  (History wp)             = history (Just wp) Nothing
route GET  (Diff    wp r1 r2)       = diff wp r1 r2
route GET  (File    wp fn Nothing)  = file wp fn
route GET  (File    wp fn (Just r)) = fileAtRevision wp fn r
route GET  (Files   wp)             = files wp
route GET  (Static  fn)             = static fn
route POST (Edit    wp)             = commit wp
route POST (Files   wp)             = upload wp
route POST (Special s)              = routeSpecial s
route _    _                        = error404 "No such page"

-- |Route a special page.
routeSpecial :: SpecialPage -> Handler Sitemap
routeSpecial Preview   = preview
routeSpecial PlainDiff = plaindiff
