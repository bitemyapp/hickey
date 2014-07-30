{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Types
import Web.Routes (PathInfo(..), patternParse)

data Sitemap = View WikiPage (Maybe Revision)
             | Edit WikiPage
             | History WikiPage
             | Diff WikiPage Revision Revision
             | Attachment WikiPage FileName (Maybe Revision)
             | Attachments WikiPage (Maybe Revision)
             | Static FileName
             | Error404
               deriving (Eq, Show)

instance PathInfo Sitemap where
    toPathSegments (View wp Nothing)  = [pageTextName wp]
    toPathSegments (View wp (Just r)) = [pageTextName wp, revisionTextId r]

    -- Because "edit" and "hist" aren't valid hexadecimal strings,
    -- it's ok to "overload" the URLs like this.
    toPathSegments (Edit wp)          = [pageTextName wp, "edit"]
    toPathSegments (History wp)       = [pageTextName wp, "hist"]

    toPathSegments (Diff wp r1 r2)    = [pageTextName wp, revisionTextId r1, revisionTextId r2]

    toPathSegments (Attachment wp fn Nothing)  = [pageTextName wp, "attachments", fileTextName fn]
    toPathSegments (Attachment wp fn (Just r)) = [pageTextName wp, revisionTextId r, "attachments", fileTextName fn]
    toPathSegments (Attachments wp Nothing)    = [pageTextName wp, "attachments"]
    toPathSegments (Attachments wp (Just r))   = [pageTextName wp, revisionTextId r, "attachments"]

    toPathSegments (Static fn)        = ["static", fileTextName fn]

    fromPathSegments = patternParse parse
        where parse = Right . parse'

              parse' :: [Text] -> Sitemap

              parse' [] = View (fromJust $ toWikiPage "FrontPage") Nothing

              parse' [p] = case toWikiPage p of
                             Just wikiPage -> View wikiPage Nothing
                             _             -> Error404

              parse' [p, "edit"] = case toWikiPage p of
                                     Just wikiPage -> Edit wikiPage
                                     _             -> Error404

              parse' [p, "hist"] = case toWikiPage p of
                                     Just wikiPage -> History wikiPage
                                     _             -> Error404

              parse' [p, r] = case toWikiPage p of
                                Just wikiPage -> View wikiPage $ toRevision r
                                _             -> Error404

              parse' [p, r1, r2] = let diff = Diff <$> toWikiPage p <*> toRevision r1 <*> toRevision r2
                                   in Error404 `fromMaybe` diff

              parse' [p, "attachments", f] = let attachment = Attachment <$> toWikiPage p <*> toFileName f <*> Just Nothing
                                             in Error404 `fromMaybe` attachment

              parse' [p, "attachments", f, r] = let attachment = Attachment <$> toWikiPage p <*> toFileName f <*> Just (toRevision r)
                                                in Error404 `fromMaybe` attachment

              parse' [p, "attachments"] = case toWikiPage p of
                                            Just wikiPage -> Attachments wikiPage Nothing
                                            _             -> Error404

              parse' [p, r, "attachments"] = case toWikiPage p of
                                               Just wikiPage -> Attachments wikiPage $ toRevision r
                                               _             -> Error404

              parse' ["static", fn] = case toFileName fn of
                                        Just fileName -> Static fileName
                                        _             -> Error404

              parse' _ = Error404
