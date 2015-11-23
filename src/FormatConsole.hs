module FormatConsole (format) where

import qualified Changelog as C
import           Data.List (intercalate)
import qualified Data.Map as M


format :: C.Changelog -> String
format cl =
  let m = C.groupByModule cl
      spacer = "\n\n"
  in intercalate spacer $
    map (\(mN, cl') -> mN ++ clPerModule cl' ++ spacer) (M.toAscList m)

clPerModule :: C.Changelog -> String
clPerModule cl = sep ++ intercalate (sep ++ sep) (filter (not . null) [added, deleted, changedType])
  where
    added = inter $ map (("+ " ++) . sigToString) (C.clAdded cl)
    deleted = inter $ map (("- " ++) . sigToString) (C.clDeleted cl)
    changedType = inter $ map changedTypeSigToString (C.clChangedType cl)

    sigToString :: (String, String, String) -> String
    sigToString (_, fN, tN) = fN ++ " :: " ++ tN
    changedTypeSigToString ((_, fN, tN), (_, _, tN')) =
      let topLine = "~ " ++ fN ++ " :: " ++ tN'
      in inter [topLine, replicate (length topLine - length tN') ' ' ++ tN]

    sep = "\n    "
    inter = intercalate sep
