-- GenChangelog Project
module Lib
    ( 
    ) where

import           Control.Arrow ((***))
import qualified Data.Set as S
import qualified Hoogle as H
import qualified Data.Map as M
import           Control.Applicative ((<$>), (<*>), pure)

type FunctionSignature = (FunctionName, Type)
type FunctionName = String
type Type = String

data Changelog = Changelog {added 
                           ,deleted 
                           ,changedType ::  S.Set FunctionSignature}
                            deriving Show

compareModules
  :: FilePath
     -> FilePath
     -> IO Changelog
compareModules oldVersion newVersion = do
  [old, new] <- mapM (fmap processResults . getHoogleTags)
                                        [oldVersion, newVersion]
  return $ buildChangelog new old

buildChangelog :: [FunctionSignature] -> [FunctionSignature] -> Changelog
buildChangelog new old = Changelog (S.fromList added') (S.fromList deleted') (S.fromList changedType')
  where
    (added', deleted', changedType') = foldr chooseBucket ([], [], []) (old ++ new)
                 
    chooseBucket sig@(n, t) (as, ds, cs)
      | n `S.member` bothSet = (as, ds, sig:cs)
      | n `S.member` newSet  = (sig:as, ds, cs)
      | n `S.member` oldSet  = (as, sig:ds, cs)
    newSet = S.fromList $ map fst new
    oldSet = S.fromList $ map fst old
    bothSet = newSet `S.intersection` oldSet




processResults :: [(a, H.Result)] -> [FunctionSignature]
processResults = map (parseSignature . H.self . snd)
  where 
    parseSignature = both H.showTagText . split . unwrap

    unwrap (H.Tags x) = x
    unwrap x = error $ "Unexpected Tags format: " ++ show x
      
    split = (head *** H.Tags) . splitAt 2 
    both f = f *** f



queryString :: String
queryString = ":: a"

query :: H.Query
query = either err id (H.parseQuery H.Haskell queryString)
  where 
    err x = error $ ("Hoogle has changed its query language!"
                     ++ " and has thrown error: " ++ show x) 

getHoogleTags :: FilePath -> IO [(H.Score, H.Result)]
getHoogleTags dbPath = H.search <$> H.loadDatabase dbPath <*> pure query
