{-# LANGUAGE OverloadedStrings #-}
module FormatHTML (format) where

import qualified Changelog as C
import           Control.Monad (forM_, replicateM_)
import           Data.ByteString.Lazy
import qualified Data.Map as M
import           Text.Blaze (Markup)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

format :: FilePath -> C.Changelog -> ByteString
format cssPath cl = renderHtml $
  let m = C.groupByModule cl
  in H.docTypeHtml $ do
    H.head $ do
      H.title "Changelog"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue cssPath)
    H.body $ do
      H.h1 "Changelog"
      H.table $ mapM_ buildHtml (M.toList m)



buildHtml
  :: H.ToMarkup a => (a, C.Changelog) -> Markup
buildHtml (m, cl) = do
  H.tr (H.th ! A.colspan (H.stringValue $ show cols) $ H.h2 (H.toHtml m))

  forM_ (C.clAdded cl) $ \(_, f, t) -> 
    H.tr $ td "+" >> td f >> td "::" >> td t 
  blankRow

  forM_ (C.clDeleted cl) $ \(_, f, t) ->
    H.tr $ td "-" >> td f >> td "::" >> td t 
  blankRow

  forM_ (C.clChangedType cl) $ \((_, f, t), (_, _, t')) -> do
    H.tr $ H.td "~" >> td f >> td "::" >> td t'
    H.tr $ emptyCell >> emptyCell >> emptyCell >> td t
  blankRow
  blankRow

  where
    cols = 4
    blankRow = H.tr (replicateM_ cols emptyCell)
    emptyCell = td ""
    td = H.td . H.toHtml
