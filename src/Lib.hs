-- GenChangelog Project
module Lib
    ( 
    ) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Arrow ((***), (&&&))
import qualified Data.Map as M
import qualified Data.Set as S
import           Debug.Trace
import qualified Hoogle as H

type FunctionSignature = (ModuleName, FunctionName, Type)
type FunctionName = String
type Type = String
type ModuleName = String

{-data Changelog = Changelog {added :: S.Set FunctionSignature
                                  ,deleted :: S.Set FunctionSignature
                                  ,changedType ::  S.Set FunctionSignature}
                                   deriving Show-}
       
data Changelog a = Changelog {added :: [FunctionSignature]
                             ,deleted :: [FunctionSignature]
                             ,changedType :: [(FunctionSignature,
                                               FunctionSignature)]
                             ,unchanged :: [FunctionSignature]} deriving Show



{-compareModules
  :: FilePath
     -> FilePath
     -> IO Changelog-}
compareModules oldVersion newVersion = do
  [old, new] <- mapM (fmap processResults . getHoogleTags)
                                        [oldVersion, newVersion]
  return $ buildChangelog' new old

data FunctionStatus a = Deleted a | ChangedType {old :: a
                                                ,new :: a} | Unchanged a | Added a
                         deriving Show
                         
 

--buildChangelog' :: [FunctionSignature] -> [FunctionSignature] -> Changelog
buildChangelog' new old = Changelog added' deleted' changeType' unchanged'
  where 
    (added', deleted', changeType', unchanged') = foldr step ([], [], [], []) 
                                                             (oldInNew ++ newInOld)

    step x (a, d, c, u) = case x of
                            Added v -> (v:a, d, c, u)
                            Deleted v -> (a, v:d, c, u)
                            ChangedType o n -> (a, d, (o, n):c, u)
                            Unchanged v -> (a, d, c, v:u)

    oldInNew = do
      sig@(n, m, t) <- old
      return $ case (n, m) `M.lookup` newM of
                 Nothing -> Deleted sig
                 Just typeName -> if typeName == t 
                                    then Unchanged sig 
                                    else ChangedType {old = sig,new = (n,m, typeName)}

    newInOld = do
      sig@(n, m, t) <- new
      return $ case (n, m) `M.lookup` oldM of
               Nothing -> Added sig
               Just typeName -> if typeName == t
                                  then Unchanged  sig
                                  else ChangedType {old = (n,
                                                           m,
                                                           typeName),
                                                    new = sig}

    makeMap = M.fromList . map (\(a,b,c) -> ((a, b), c))
    [newM, oldM] = map makeMap [new, old]

--buildChangelog :: [FunctionSignature] -> [FunctionSignature] -> Changelog
--buildChangelog new old = Changelog (S.fromList added') (S.fromList deleted') (S.fromList changedType')
  --where
    {-(added', deleted', changedType') = foldr chooseBucket ([], [], []) (old ++ new)
                 
    chooseBucket sig@(m, n, t) (as, ds, cs)
      | (m, n) `S.member` bothSet = case version sig of
                                       NewVersion -> (as, ds, cs)
                                       OldVersion -> (as, ds, sig:cs)
                                       BothVersions -> undefined

      | (m, n) `S.member` newSetNameModule  = (sig:as, ds, cs)
      | (m, n) `S.member` oldSetNameModule  = (as, sig:ds, cs)
    newSetNameModule = S.fromList $ map sndThree new
    oldSetNameModule = S.fromList $ map sndThree old
    bothSet = newSetNameModule `S.intersection` oldSetNameModule
    newSetSig = S.fromList new
    oldSetSig = S.fromList old
    version x = if x `S.member` newSetSig
                then if x `S.member` oldSetSig
                        then BothVersions
                        else NewVersion
                else if x `S.member` oldSetSig
                     then OldVersion
                     else error $ show x ++ " not showing up in either set."
    sndThree (a, b, c) = (a, b)-}





processResults :: [(H.Score, H.Result)] ->  [(ModuleName, FunctionName, Type)]
processResults = map (combine . (getModuleName &&& getFunction) . snd )
  where 
    
    combine (a, (b, c)) = (a, b, c)
    

    
      
     
both f = f *** f
unwrap (H.Tags x) = x
unwrap x = error $ "Unexpected Tags format: " ++ show x         
split = (head *** H.Tags) . splitAt 2       
parseSignature :: H.TagStr -> (FunctionName, Type)
parseSignature = both H.showTagText . split . unwrap             

getFunction =  parseSignature . H.self
getModuleName = snd . (!! 1) . snd . head . H.locations 



queryString :: String
queryString = ":: a"

query :: H.Query
query = either err id (H.parseQuery H.Haskell queryString)
  where 
    err x = error $ ("Hoogle has changed its query language!"
                     ++ " and has thrown error: " ++ show x) 

getHoogleTags :: FilePath -> IO [(H.Score, H.Result)]
getHoogleTags dbPath = H.search <$> H.loadDatabase dbPath <*> pure query
