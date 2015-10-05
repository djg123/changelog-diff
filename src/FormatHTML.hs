{-# LANGUAGE OverloadedStrings #-}
module FormatHTML (format) where

import qualified Changelog as C
import           Data.ByteString.Lazy
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

format :: C.Changelog -> ByteString
format cl = renderHtml $
  let m = C.groupByModule cl
  in H.docTypeHtml $ do 
    H.head (H.title "Changelog")
    H.body $ 
      H.ul $ mapM_ buildHtml (M.toList m)



buildHtml (m, cl) = do
  H.h1 (H.toHtml m)

  H.h2 "Added"
  H.ul $ toLiTag C.clAdded

  H.h2 "Deleted"
  H.ul $ toLiTag C.clDeleted

  H.h2 "Changed Type"
  H.ul $ mapM_ (H.li . H.toHtml . untupleChangedType) (C.clChangedType cl)
  where
    untuple :: (String, String, String) -> String
    untuple (_, f, t) = f ++ " :: " ++ t

    untupleChangedType :: (C.FunctionSignature, C.FunctionSignature) -> String
    untupleChangedType ((_, f, t), (_, _, t')) = f ++ " :: " ++ t' ++ "( " ++ t ++ " )"

    toLiTag f = mapM_ (H.li . H.toHtml . untuple) (f cl)
