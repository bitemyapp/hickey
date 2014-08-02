{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit
    ( edit
    , commit
    , preview) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Templates
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (textValue)
import Types
import Routes
import Store
import Web.Seacat (Handler, MkUrl, htmlResponse, param)
import Web.Seacat.RequestHandler (htmlUrlResponse)

import qualified Templates.Utils as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- |Display an edit form for a page.
edit :: WikiPage -> Handler Sitemap
edit wp = do
  wikiPage <- getStoredFile $ pageFileName wp
  htmlUrlResponse $ renderEditPage wp $ fromMaybe "" wikiPage

-- |Save an edit, possibly uploading some new attachments in the
-- process.
commit :: WikiPage -> Handler Sitemap
commit wp = undefined

-- |Render a preview of some posted markup.
preview :: Handler Sitemap
preview = param "markup" >>= htmlResponse . renderBareMarkup . fromMaybe ""

-----

-- |Display a page with an area for displaying a preview, and a box
-- for editing the markup.
renderEditPage :: WikiPage -> Text -> MkUrl Sitemap -> Html
renderEditPage wp md = renderHtmlPage "Edit" $ \mkurl -> do
  -- Inside here lives a rendered preview, we'll have some javascript pull it in
  H.div ! A.id (textValue "preview") $ T.empty

  -- And here is the edit form
  H.form ! method (textValue "post") ! action (textValue $ flip mkurl [] $ Edit wp) ! enctype (textValue "multipart/form-data") $
    fieldset $ do
      ol ! A.id (textValue "mainform") $ do
        li $
          textarea ! name (textValue "markup") $ toHtml md
        li $
          T.input' "Your Handle:" "who" "text"
        li $
          T.input' "Edit Summary:" "desc" "text"
        li $
          H.input ! type_ (textValue "submit") ! value (textValue "Save Changes")

      hr

      -- Now we have new attachments. Javascript can add on new
      -- entries and update the counter.
      ol ! A.id (textValue "fileform") $ do
       li $
         input ! name (textValue "file0") ! type_ (textValue "file")

       -- This will ordinarily be hidden by aforementioned magical
       -- javascript.
       li ! A.id (textValue "counter") $ do
         H.label ! for (textValue "numfiles") $ T.toHtml "Number of Files:"
         input ! name (textValue "numfiles") ! type_ (textValue "number") ! value (textValue "0") ! required (textValue "required")

      -- Don't say you haven't been warned…
      p ! class_ (textValue "notice") $ T.toHtml "Filenames must be of the form [a-zA-Z0-9]+.[a-zA-Z0-9]+. If a file is uploaded with the same name as a currently existing file attached to this page, the existing file will be replaced."
      
