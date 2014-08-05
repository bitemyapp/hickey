{-# LANGUAGE OverloadedStrings #-}

-- |Apply arbitrary transformations to the content of wiki pages.
module Templates.Transformation
    ( preprocess
    , postprocess
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.FileStore (FileStore)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack, unpack, split)
import Routes
import Store.Retrieve (doesFileExistFS)
import Store.Paths
import System.Process (readProcess)
import Templates.MarkdownToHtml (readMarkdown)
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Walk
import Text.Regex (mkRegex, matchRegex)
import Types
import Web.Seacat (MkUrl)

-- |Turn the input text into a format suitable for parsing.
preprocess :: Text -> String
preprocess = unpack

-- |Transform the Pandoc AST before handing it off to the HTML writer.
postprocess :: (Functor m, MonadIO m) => [Plugin] -> FileStore -> MkUrl Sitemap -> Pandoc -> m Pandoc
postprocess plugins fs mkurl = broken fs mkurl <=< return . emptyLinks mkurl . wikiWords mkurl <=< expandPlugins plugins

-- |Expand plugins iteratively until the AST settles (with a depth
-- limit to prevent infinite loops)
expandPlugins :: (Functor m, MonadIO m) => [Plugin] -> Pandoc -> m Pandoc
expandPlugins = expandPlugins' (100 :: Integer)

  where expandPlugins' 0 _ p  = return p
        expandPlugins' _ [] p = return p
        expandPlugins' n plugins p = do
          expanded <- walkM (expandOnce plugins) p
          if p == expanded
          then return p
          else expandPlugins' (n - 1) plugins expanded

        expandOnce plugins cb@(CodeBlock a@(_, [ty], _) s) = case lookup ty plugins of
                                                             Just plugin -> execPlugin plugin a s
                                                             Nothing     -> return cb
        expandOnce _ b = return b

        execPlugin plugin a s = do
          Pandoc _ blocks <- readMarkdown <$> liftIO (readProcess plugin [] s)
          return $ Div a blocks

-- |Autolink WikiWords
wikiWords :: MkUrl Sitemap -> Pandoc -> Pandoc
wikiWords mkurl = walk ww
    where ww (Plain is)          = Plain $ map expand is
          ww (Para is)           = Para  $ map expand is
          ww (DefinitionList ds) = DefinitionList $ map (first $ map expand) ds
          ww (Header n a is)     = Header n a $ map expand is
          ww (Table is as ds tcs tccs) = Table (map expand is) as ds tcs tccs
          ww b = b

          expand (Str s)          = if isCCased s
                                    then Link [Str s] (link s, s)
                                    else Str s
          expand (Emph is)        = Emph        $ map expand is
          expand (Strong is)      = Strong      $ map expand is
          expand (Strikeout is)   = Strikeout   $ map expand is
          expand (Superscript is) = Superscript $ map expand is
          expand (Subscript is)   = Subscript   $ map expand is
          expand (SmallCaps is)   = SmallCaps   $ map expand is
          expand (Quoted q is)    = Quoted q    $ map expand is
          expand (Cite cs is)     = Cite cs     $ map expand is
          expand (Span a is)      = Span a      $ map expand is
          expand i = i

          isCCased = isJust . matchRegex regex
          regex    = mkRegex "([A-Z]+[a-z]+){2,}"
          link s   = unpack $ mkurl (View (fromJust . toWikiPage . pack $ s) Nothing) []

-- |Turn empty links into wikilinks.
--
-- This lets you link to the article "Git" with `[Git]()`.
emptyLinks :: MkUrl Sitemap -> Pandoc -> Pandoc
emptyLinks mkurl = walk lnk
    where lnk l@(Link [Str s] ("", title)) = if isPageName $ pack s
                                             then Link [Str s] (link s, title)
                                             else l
          lnk i = i

          link s = unpack $ mkurl (View (fromJust . toWikiPage . pack $ s) Nothing) []

-- |Apply a "broken" class to all internal broken links.
broken :: (Functor m, MonadIO m) => FileStore -> MkUrl Sitemap -> Pandoc -> m Pandoc
broken fs mkurl = walkM bork
    where bork l@(Link _ (target, _)) = let tlast = getLast target
                                            wlink = unpack $ mkurl (View (fromJust $ toWikiPage tlast) Nothing) []
                                        in if isPageName tlast && wlink == target
                                           then do
                                             exists <- doesFileExistFS fs $ wikipage . fromJust $ toWikiPage tlast
                                             return $ if exists
                                                      then l
                                                      else Span ("", ["broken"], []) [l]
                                           else return l
          bork i = return i

          getLast = last . split (=='/') . pack
