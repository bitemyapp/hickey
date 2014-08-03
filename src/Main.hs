module Main where

import Data.Text (pack)
import Handler.View
import Handler.Edit
import Handler.Static
import Network.HTTP.Types (StdMethod(..))
import Routes
import Web.Seacat (Handler, seacat, defaultSettings)
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
route GET  (View    wp Nothing)     = page wp
route GET  (View    wp (Just r))    = pageAtRevision wp r
route GET  (Edit    wp)             = edit wp
route GET  (History wp)             = history wp
route GET  (Diff    wp r1 r2)       = diff wp r1 r2
route GET  (File    wp fn Nothing)  = file wp fn
route GET  (File    wp fn (Just r)) = fileAtRevision wp fn r
route GET  (Files   wp)             = files wp
route GET  (Static  fn)             = static fn
route POST (Edit    wp)             = commit wp
route POST  Preview                 = preview
route _    _                        = error404 "No such page"
