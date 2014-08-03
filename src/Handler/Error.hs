{-# LANGUAGE OverloadedStrings #-}

module Handler.Error
    ( renderBadPage
    , renderBadRevision
    , renderBadDiff
    , renderBadFiles) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Routes (Sitemap)
import Templates (renderNoticePage)
import Text.Blaze.Html (Html)
import Types
import Web.Seacat (MkUrl)

-- |Render an error page, saying that a page name is bad.
renderBadPage :: WikiPage -> MkUrl Sitemap -> Html
renderBadPage wp = notice "No such page"
                     [ "The page "
                     , pageTextName wp
                     , " does not exist."
                     ]

-- |Render an error page, saying that a revision is bad.
renderBadRevision :: WikiPage -> Revision -> MkUrl Sitemap -> Html
renderBadRevision wp r = notice "Revision too old"
                           [ "The page"
                           , pageTextName wp
                           , " did not exist at revision "
                           , revisionTextId r
                           , "."
                           ]

-- |Render an error page, saying that a revision range is bad.
renderBadDiff :: WikiPage -> Revision -> Revision -> MkUrl Sitemap -> Html
renderBadDiff wp r1 r2 = notice "Bad commit range"
                           [ "The diff for "
                           , pageTextName wp
                           , " could not be generated for the range "
                           , revisionTextId r1
                           , "–"
                           , revisionTextId r2
                           , "."
                           ]

-- |Display an error page, saying that a filename is bad.
renderBadFiles :: MkUrl Sitemap -> Html
renderBadFiles = notice "Invalid Form Submission" ["Filenames must be an alphanumeric string containing a single '.'."]

-----

-- |Render a notice page from a title and body.
notice :: Text -> [Text] -> MkUrl Sitemap -> Html
notice title = renderNoticePage title . foldl1 (<>)
