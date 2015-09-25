module FormatConsole (format) where

import qualified Changelog as C
import           Data.List (intercalate)
import qualified Data.Map as M


format :: C.Changelog -> String
format cl = let m = C.groupByModule cl
            in unlines $ map (\(mN, cl') ->  mN    
                                          ++ (clPerModule cl'))
                             (M.toAscList m)

clPerModule :: C.Changelog -> String
clPerModule cl = sep ++ (intercalate sep $ map sigToString (C.clAdded cl)
                                                         ++ map sigToString (C.clDeleted cl)
                                                         ++ map changedTypeSigToString (C.clChangedType cl))
  where 
    sigToString (_, fN, tN) = fN ++ " :: " ++ tN
    changedTypeSigToString ((_, fN, tN),
                            (_, _, tN')) = intercalate sep [fN ++ " :: " ++ tN'
                                                           ,fN ++ " :: " ++ tN']

    sep = "\n  "



