{-# LANGUAGE OverloadedStrings #-}

-- |Apply arbitrary transformations to the content of wiki pages.
module Templates.Transformation
    ( preprocess
    , postprocess
    ) where

import Control.Applicative ((<$>))
import Control.Monad ((>=>), liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.FileStore (FileStore)
import Data.Foldable (foldMap)
import Data.Generics (everywhereBut, mkQ, mkT, toConstr, empty)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, replace, split, strip, breakOn, isPrefixOf)
import Routes
import Store.Retrieve (getStoredFileFS, doesFileExistFS)
import Store.Paths
import System.Process (readProcess)
import Templates.MarkdownToHtml (readMarkdown)
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Walk
import Text.Regex (mkRegex, matchRegex, matchRegexAll)
import Types
import Web.Seacat (MkUrl)

import qualified Data.Text as T

-- *Batch transformations

-- |Turn the input text into a format suitable for parsing.
preprocess :: Text -> String
preprocess = unpack . replace "\r\n" "\n"

-- |Transform the Pandoc AST before handing it off to the HTML writer.
postprocess :: (Functor m, MonadIO m) => [Plugin] -> FileStore -> MkUrl Sitemap -> Pandoc -> m Pandoc
postprocess plugins fs mkurl = foldl1 (>=>) [ expandPlugins plugins
                                            , return . wikiWords mkurl
                                            , broken fs mkurl
                                            , interWiki fs
                                            , return . bareURLs
                                            ]

-- *Individual transformations

-- |Expand plugins recursively until the AST settles (with a depth
-- limit to prevent infinite loops)
expandPlugins :: (Functor m, MonadIO m) => [Plugin] -> Pandoc -> m Pandoc
expandPlugins = expandPlugins' (100 :: Int)
        -- If the limit is hit, or there are no plugins, halt.
  where expandPlugins' 0 _  p = return p
        expandPlugins' _ [] p = return p

        -- Otherwise, try expanding.
        expandPlugins' n plugins p = do
          expanded <- walkM (expandOnce plugins) p
          if p == expanded
          then return p
          else expandPlugins' (n - 1) plugins expanded

        expandOnce plugins cb@(CodeBlock a@(_, [ty], _) s) = case lookup ty plugins of
                                                               Just plugin -> execPlugin plugin a s
                                                               Nothing     -> return cb
        expandOnce _ b = return b

        -- Execute the plugin, with the block contents as stdin, and
        -- render the stdout.
        execPlugin plugin a s = do
          Pandoc _ blocks <- readMarkdown <$> liftIO (readProcess plugin [] s)
          return $ Div a blocks

-- |Autolink WikiWords and empty links
wikiWords :: MkUrl Sitemap -> Pandoc -> Pandoc
wikiWords mkurl = wikilinks $ Internal mkurl

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
interWiki fs p = liftM (foldl (flip wikilinks) p) getInterWikiLinks
    where getInterWikiLinks = do
            contents <- getStoredFileFS fs "interwiki.conf"
            return $
              case contents of
                Just txt -> unzipWith External [breakOn " " t | t <- filter (not . T.null) $ map strip txt]
                Nothing  -> []

-- |Expand bare URLs into actual links. A bare URL is (in regular
-- text) a string starting with http://, https://, or ftp://
-- containing no spaces, and may contain spaces in link tags.
--
-- This re-uses the inter-wiki linking machinery.
bareURLs :: Pandoc -> Pandoc
bareURLs p = foldl (flip $ flip wikilinks' $ isPrefixOf "//") p targets
    where targets   = map (\proto -> External proto $ proto <> ":{}") protocols
          protocols = ["http", "https", "ftp"]

-- *Helpers

-- |Distinguish between desired link types.
data LinkType = Internal (MkUrl Sitemap)
              | External Prefix Target

type Prefix = Text
type Target = Text

-- |Like `wikiLinks'`, but takes no predicate.
wikilinks :: LinkType -> Pandoc -> Pandoc
wikilinks = flip wikilinks' $ const True

-- |Expand wiki links (including those in empty links, like
-- `[Git]()`).
--
-- For links not inside empty link tags, trailing punctuation is
-- stripped and not considered to be part of the link definition. This
-- allows "FooBar." to be interpreted as a link to "FooBar" followed
-- by a full stop, rather than a link to "FooBar."
--
-- Matches can be further refined by the supplied predicate function
-- which takes (for external links) the text after the prefix and (for
-- internal links) the page name.
wikilinks' :: LinkType -> (Text -> Bool) -> Pandoc -> Pandoc
wikilinks' t p = walk empties . everywhereBut (mkQ False isImgLink) (mkT ww)
          -- Expand WikiWords in plain text
    where ww (Str s : xs) = case matchRegexAll trailingPunct s of
                              Just (txt, punct, _, _) -> Str txt `fromMaybe` wikilink t p True txt : Str punct : xs
                              Nothing                 -> Str s   `fromMaybe` wikilink t p True s               : xs
          ww a = a

          trailingPunct = mkRegex "[\\.,!?\"':;-]+$"
          isImgLink     = hasConstr [Link empty empty, Image empty empty]

          -- Expand empty link tags
          empties r@(Link strs ("", _)) | all plainText strs = r `fromMaybe` wikilink t p False (catText strs)
          empties i = i

          plainText = hasConstr [Str empty, Space]
          catText   = foldMap $ \i -> case i of
                                       Str s -> s
                                       Space -> " "

-- |Try to turn text into a link. If strict, internal links must be
-- CamelCased.
wikilink :: LinkType -> (Text -> Bool) -> Bool -> String -> Maybe Inline
wikilink target pred strict = case target of
                                Internal mkurl    -> link' "/" iconds        $ ilink mkurl
                                External pref url -> link' ":" (econds pref) $ elink url
          -- Handle a prefix/link text pair
    where link' chr conds tourl txt = let (pref, suff) = T.drop 1 <$> breakOn chr (pack txt)
                                      in if and $ conds pref suff
                                         then Just $ Link [Str txt] (tourl pref suff, txt)
                                         else Nothing

          -- Internal and external link conditions
          iconds      p r = [isPageName p, not strict || isCCased (unpack p), r == "" || isRevisionId r, pred p]
          econds pref p l = [p == pref, T.length l > 0, pred l]

          isCCased = isJust . matchRegex camCase
          camCase  = mkRegex "([A-Z]+[a-z]+){2,}"

          -- Internal and external link constructors
          ilink mkurl s r = unpack $ mkurl (View (fromJust $ toWikiPage s) $ toRevision r) []
          elink url _ s   = unpack $ replace "{}" s url

-- |Determine if a value has a constructor in a given set.
hasConstr :: Data a => [a] -> a -> Bool
hasConstr ds i = toConstr i `elem` map toConstr ds

-- |Unzip a list of pairs with a given function.
unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith = map . uncurry
