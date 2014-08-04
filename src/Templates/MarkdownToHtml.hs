{-# LANGUAGE OverloadedStrings #-}

-- |Read Markdown to Pandoc, and write Pandoc to HTML. This module
-- ensures that all reading/writing functions use the same options.
module Templates.MarkdownToHtml
    ( readerOptions
    , writerOptions

    , readMarkdown

    , writeDocument
    , writeFragment
    , writeToC
    ) where

import Control.Monad (unless)
import Data.Default (def)
import Templates.Utils (toHtml)
import Text.Blaze.Html5 (Html, (!), aside, nav, h1)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))

import qualified Text.Blaze.Internal          as B
import qualified Text.Blaze.Html5.Attributes  as A
import qualified Text.Pandoc.Readers.Markdown as R
import qualified Text.Pandoc.Writers.HTML     as W

-- |Options used for the reader.
readerOptions :: ReaderOptions
readerOptions = def

-- |Options used for the writer. This does NOT include table of contents generation.
writerOptions :: WriterOptions
writerOptions = def { writerSectionDivs = True
                    , writerHtml5       = True
                    , writerHighlight   = True
                    }

-- |Parse some Markdown. This does no pre- or post-processing.
readMarkdown :: String -> Pandoc
readMarkdown = R.readMarkdown readerOptions

-- |Write out some parsed Pandoc into a full document. This includes a
-- table of contents (if there are any headings).
writeDocument :: Pandoc -> Html
writeDocument p = writeToC p >> writeFragment p

-- |Compile Pandoc to HTML *without* a table of contents.
writeFragment :: Pandoc -> Html
writeFragment = W.writeHtml writerOptions

-- |Compile Pandoc to HTML with a table of contents (the resultant
-- html is empty if there are no headings in the document)
writeToC :: Pandoc -> Html
writeToC p = let toc = W.writeHtml options p
             in unless (B.null toc) $
                  aside ! A.id "toc" $
                    nav $ do
                      h1 $ toHtml "Table of Contents"
                      toc

    where options = writerOptions { writerStandalone      = True
                                  , writerTemplate        = "$toc$"
                                  , writerTableOfContents = True
                                  }
