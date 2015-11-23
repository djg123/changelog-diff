{-# LANGUAGE OverloadedStrings #-}
module FormatHTML (format) where

import qualified Changelog                     as C
import           Control.Monad                 (forM_, replicateM_)
import qualified Data.ByteString.Lazy          as L
import qualified Data.Map                      as M
import           Data.Monoid                   ((<>))
import           Text.Blaze                    (Markup)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Types                         as T

format :: FilePath -> C.Changelog -> L.ByteString
format cssPath cl = renderHtml $
  let m = C.groupByModule cl
  in H.docTypeHtml $ do
    H.head $ do
      H.title "Changelog"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue cssPath)
    H.body $ do
      H.h1 "Changelog"
      H.table $ mapM_ (runMarkup . buildHtml) (M.toList m)

runMarkup :: [H.Html] -> Markup
runMarkup = foldr1 (>>) . zipWith ($) (cycle [evenEntry, oddEntry])

buildHtml :: (T.ModuleName, C.Changelog) -> [H.Html]
buildHtml (m, cl) =
  [H.tr (H.th ! A.colspan (H.stringValue $ show cols) $ H.h2 (H.toHtml m))] ++

  map (\(_, f, t) -> H.tr (td "+" >> td f >> td "::" >> td t)) (C.clAdded cl) ++


  map (\(_, f, t) -> H.tr (td "-" >> td f >> td "::" >> td t)) (C.clDeleted cl) ++


  map
    (\((_, f, t), (_, _, t')) -> H.tr (H.td "~" >> td f >> td "::" >> td t') <>
                                 H.tr (emptyCell >> emptyCell >> emptyCell >> td t))
    (C.clChangedType cl)

  where
    cols = 4
    blankRow = H.tr (replicateM_ cols emptyCell) ! A.rowspan "2"
    emptyCell = td ""
    td = H.td . H.toHtml :: String -> H.Html

oddEntry :: H.Html -> H.Html
oddEntry = (! A.class_ "odd-entry")

evenEntry :: H.Html -> H.Html
evenEntry = (! A.class_ "even-entry")
