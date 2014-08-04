-- |Apply arbitrary transformations to the content of wiki pages.
module Templates.Transformation
    ( preprocess
    , postprocess
    ) where

import Data.Text (Text, unpack)
import Text.Pandoc.Definition (Pandoc(..))

-- |Turn the input text into a format suitable for parsing.
-- TODO: Plugins
preprocess :: Text -> String
preprocess = unpack

-- |Transform the Pandoc AST before handing it off to the HTML writer.
-- TODO: WikiWords
-- TODO: Empty links
postprocess :: Pandoc -> Pandoc
postprocess = id
