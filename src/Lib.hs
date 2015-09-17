module Lib
    ( 
    ) where

import qualified Data.Set as S
import qualified Hoogle as H

type FunctionSignature = String

getTags :: [(a, H.Result)] -> S.Set FunctionSignature
getTags = S.fromList
            . map (H.showTagText . H.self . snd) 
        
        
queryString :: String
queryString = ":: a"

getHoogleTags dbPath = do
  d <- H.loadDatabase dbPath
  case  H.parseQuery H.Haskell queryString of
      Left _ -> error "Didn't work..."
      Right q -> return $ H.search d q


