{-# LANGUAGE OverloadedStrings #-}

-- |Apply arbitrary transformations to the content of wiki pages.
module Templates.Transformation
    ( preprocess
    , postprocess
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad ((>=>), liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.FileStore (FileStore)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack, unpack, replace, split, strip, breakOn)
import Routes
import Store.Retrieve (getStoredFileFS, doesFileExistFS)
import Store.Paths
import System.Process (readProcess)
import Templates.MarkdownToHtml (readMarkdown)
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Walk
import Text.Regex (mkRegex, matchRegex)
import Types
import Web.Seacat (MkUrl)

import qualified Data.Text as T

-- |Turn the input text into a format suitable for parsing.
preprocess :: Text -> String
preprocess = unpack . replace "\r\n" "\n"

-- |Transform the Pandoc AST before handing it off to the HTML writer.
postprocess :: (Functor m, MonadIO m) => [Plugin] -> FileStore -> MkUrl Sitemap -> Pandoc -> m Pandoc
postprocess plugins fs mkurl = foldl1 (>=>) [ expandPlugins plugins
                                            , return . wikiWords mkurl
                                            , broken fs mkurl
                                            , interWiki fs
                                            ]

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

-- |Autolink WikiWords and empty links
wikiWords :: MkUrl Sitemap -> Pandoc -> Pandoc
wikiWords mkurl = wikilinks $ Right mkurl

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

-- |Expand inter-wiki links into actual links.
--
-- Inter-wiki links are of the form prefix:link or [prefix:link](),
-- where the prefix identifies the wiki to link to.
interWiki :: (Functor m, MonadIO m) => FileStore -> Pandoc -> m Pandoc
interWiki fs p = liftM (foldl (\p' iwl -> wikilinks (Left iwl) p') p) getInterWikiLinks
    where getInterWikiLinks = do
            contents <- getStoredFileFS fs "interwiki.conf"
            return $
              case contents of
                Just txt -> filter (/=("","")) $ map (breakOn " " . strip) $ T.lines txt
                Nothing  -> []

-- |Expand wiki links (including those in empty links, like
-- `[Git]()`), with a possible prefix. If the prefix is not given, use
-- the supplied url making function to construct internal links.
wikilinks :: Either (Text, Text) (MkUrl Sitemap) -> Pandoc -> Pandoc
wikilinks urls = walk empties . walk ww
    where ww (Plain is)          = Plain $ map expand is
          ww (Para is)           = Para  $ map expand is
          ww (DefinitionList ds) = DefinitionList $ map (first $ map expand) ds
          ww (Header n a is)     = Header n a $ map expand is
          ww (Table is as ds tcs tccs) = Table (map expand is) as ds tcs tccs
          ww b = b

          expand r@(Str s)        = link True s r
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

          empties r@(Link strs ("", title)) | plainText strs = link False (catText strs) r
          empties i = i

          isCCased = isJust . matchRegex regex
          regex    = mkRegex "([A-Z]+[a-z]+){2,}"

          plainText [Str _]    = True
          plainText [Space]    = True
          plainText (Space:ss) = plainText ss
          plainText (Str s:ss) = plainText ss
          plainText _          = False

          catText [Str s]    = s
          catText [Space]    = " "
          catText (Space:ss) = ' ' : catText ss
          catText (Str s:ss) = s ++ catText ss

          -- Turn text into links. If strict is True, require
          -- non-prefix links to be CamelCased as well as valid page
          -- names.
          link strict s r = case urls of
                              Left (pref, url) ->
                                let (p, l) = breakOn ":" $ pack s
                                in if p == pref
                                   then Link [Str s] (elink url $ T.drop 1 l, s)
                                   else r

                              Right mkurl ->
                                if isPageName (pack s) && (not strict || isCCased s)
                                then Link [Str s] (ilink mkurl s, s)
                                else r

          -- Internal link
          ilink mkurl s = unpack $ mkurl (View (fromJust . toWikiPage . pack $ s) Nothing) []

          -- External link
          elink url s = unpack $ replace "{}" s url
