{-# LANGUAGE OverloadedStrings #-}
module FormatHTML (format) where

import qualified Changelog as C
import           Control.Monad (forM_, replicateM_)
import           Data.ByteString.Lazy hiding (cycle, zip)
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

  forWithParityM_ (C.clAdded cl) $ \((_, f, t), parityF) ->
    parityF $ H.tr (td "+" >> td f >> td "::" >> td t )
  blankRow

  forWithParityM_ (C.clDeleted cl) $ \((_, f, t), parityF) ->
    parityF $ H.tr (td "-" >> td f >> td "::" >> td t )
  blankRow

  forWithParityM_ (C.clChangedType cl) $ \(((_, f, t), (_, _, t')), parityF) -> do
    parityF $ H.tr (H.td "~" >> td f >> td "::" >> td t')
    parityF $ H.tr (emptyCell >> emptyCell >> emptyCell >> td t)
  blankRow
  blankRow

  where
    forWithParityM_ xs = forM_ (xs `zip` cycle [evenEntry, oddEntry])
    oddEntry = (! A.class_ "odd-entry")
    evenEntry = (! A.class_ "even-entry")
    cols = 4
    blankRow = H.tr (replicateM_ cols emptyCell)
    emptyCell = td ""
    td = H.td . H.toHtml
