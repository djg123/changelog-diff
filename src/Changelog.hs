<<<<<<< HEAD
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelog
    (compareModules
    ,Changelog(..)
    ,groupByModule
    ,FunctionSignature
    ,FunctionName
    ,Type
    ,ModuleName
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Arrow ((&&&), (***))
=======
module Changelog (compareModules, Changelog(..), groupByModule
                 ,                               buildChangelog') where

import           Control.Monad (replicateM)
>>>>>>> cleanup
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Tuple.Extra (both, (&&&), (***))
import qualified Hoogle as H
import           Types (FunctionSignature, ModuleName, Type, FunctionName)


data Changelog = Changelog
  {clAdded       :: [FunctionSignature]
  ,clDeleted     :: [FunctionSignature]
  ,clChangedType :: [(FunctionSignature, FunctionSignature)]
  ,clUnchanged   :: [FunctionSignature]}
  deriving (Show, Eq)

<<<<<<< HEAD
data Changelog =
       Changelog
         { clAdded :: [FunctionSignature]
         , clDeleted :: [FunctionSignature]
         , clChangedType :: [(FunctionSignature, FunctionSignature)]
         , clUnchanged :: [FunctionSignature]
         }
  deriving Show

instance Monoid Changelog where
  mempty = Changelog {clAdded = []
                     ,clDeleted = []
                     ,clChangedType = []
                     ,clUnchanged = []}

  mappend cl1 cl2 = Changelog {clAdded = clAdded cl1 <> clAdded cl2
                              ,clDeleted = clDeleted cl1 <> clDeleted cl2
                              ,clChangedType = clChangedType cl1 <> clChangedType cl2
                              ,clUnchanged = clUnchanged cl1 <> clUnchanged cl2}

instance Arbitrary Changelog where
  arbitrary = Changelog 
                <$> arbitrary 
                <*> arbitrary 
                <*> arbitrary 
                <*> arbitrary
=======
instance Monoid Changelog where
  mempty = Changelog { clAdded = [], clDeleted = [], clChangedType = [], clUnchanged = [] }
  mappend cl1 cl2 = Changelog
    { clAdded = clAdded cl1 <> clAdded cl2
    , clDeleted = clDeleted cl1 <> clDeleted cl2
    , clChangedType = clChangedType cl1 <> clChangedType cl2
    , clUnchanged = clUnchanged cl1 <> clUnchanged cl2
    }
>>>>>>> cleanup


insertAdded :: FunctionSignature -> Changelog -> Changelog
insertAdded v cl = cl {clAdded = v : clAdded cl}

insertDeleted :: FunctionSignature -> Changelog -> Changelog
<<<<<<< HEAD
insertDeleted v cl = cl {clDeleted = v : clDeleted cl}                      
=======
insertDeleted v cl = cl {clDeleted = v : clDeleted cl}
>>>>>>> cleanup

insertChangedType
  :: (FunctionSignature, FunctionSignature) -> Changelog -> Changelog
insertChangedType v cl = cl {clChangedType = v : clChangedType cl}

<<<<<<< HEAD

=======
>>>>>>> cleanup
insertUnchanged :: FunctionSignature -> Changelog -> Changelog
insertUnchanged v cl = cl {clUnchanged = v : clUnchanged cl}

groupByModule :: Changelog -> M.Map ModuleName Changelog
groupByModule (Changelog
<<<<<<< HEAD
              {clAdded = added
              ,clDeleted = deleted
              ,clChangedType = changedType
              ,clUnchanged = unchanged}) = foldr (M.unionWith (<>)) mempty [added', deleted', unchanged', changedType']
=======
                 { clAdded = added
                 , clDeleted = deleted
                 , clChangedType = changedType
                 , clUnchanged = unchanged
                 }) = foldr (M.unionWith (<>)) mempty [added', deleted', unchanged', changedType']
>>>>>>> cleanup
  where
    added' = foldr (step insertAdded) M.empty added
    deleted' = foldr (step insertDeleted) M.empty deleted
    unchanged' = foldr (step insertUnchanged) M.empty unchanged
    changedType' = foldr stepChangedType M.empty changedType

    step f tup@(m, _, _) z =
      if m `M.member` z
        then M.insert m (f tup mempty) z
        else M.adjust (f tup) m z

    stepChangedType tup@((m, _, _), _) z = if m `M.member` z
                                             then M.insert m (insertChangedType tup mempty) z
                                             else M.adjust (insertChangedType tup) m z

compareModules :: FilePath -> FilePath -> IO Changelog
compareModules oldVersion newVersion = do
  [old, new] <- mapM (fmap processResults . getHoogleTags) [oldVersion, newVersion]
  return $ buildChangelog' new old

data FunctionStatus a = Deleted a
                      | ChangedType { old :: a, new :: a }
                      | Unchanged a
                      | Added a
  deriving Show



buildChangelog'
  :: [(ModuleName, FunctionName, Type)]
     -> [(ModuleName, FunctionName, Type)] -> Changelog
buildChangelog' new old = Changelog added' deleted' (nub changeType') (nub unchanged')
  where
    (added', deleted', changeType', unchanged') = foldr step ([], [], [], []) (oldInNew ++ newInOld)

    step x (a, d, c, u) =
      case x of
        Added v         -> (v : a, d, c, u)
        Deleted v       -> (a, v : d, c, u)
        ChangedType o n -> (a, d, (o, n) : c, u)
        Unchanged v     -> (a, d, c, v : u)

    oldInNew = do
      sig@(n, m, t) <- old
      return $
        case (n, m) `M.lookup` newM of
          Nothing -> Deleted sig
          Just typeName -> if typeName == t
                             then Unchanged sig
                             else ChangedType { old = sig, new = (n, m, typeName) }

    newInOld = do
      sig@(n, m, t) <- new
      return $
        case (n, m) `M.lookup` oldM of
          Nothing -> Added sig
          Just typeName -> if typeName == t
                             then Unchanged sig
                             else ChangedType { old = (n, m, typeName), new = sig }

    makeMap = M.fromList . map (\(a, b, c) -> ((a, b), c))
    [newM, oldM] = map makeMap [new, old]


processResults :: [(H.Score, H.Result)] ->  [(ModuleName, FunctionName, Type)]
processResults = map (combine . (getModuleName &&& getFunction) . snd )
  where
    combine (a, (b, c)) = (a, b, c)


getFunction :: H.Result -> (FunctionName, Type)
getFunction = parseSignature . H.self

getModuleName :: H.Result -> String
getModuleName = snd . (!! 1) . snd . head . H.locations

parseSignature :: H.TagStr -> (FunctionName, Type)
parseSignature = both H.showTagText . split . unwrap
  where
    split = (head *** H.Tags) . splitAt 2

    unwrap (H.Tags x) = x 
    unwrap x = error $ "Unexpected Tags format: " ++ show x



queryString :: String
queryString = ":: a"

query :: H.Query
query = either err id (H.parseQuery H.Haskell queryString)
  where
    err x = error ("Hoogle has changed its query language!"
                   ++ " and has thrown error: " ++ show x)

getHoogleTags :: FilePath -> IO [(H.Score, H.Result)]
getHoogleTags dbPath = H.search <$> H.loadDatabase dbPath <*> pure query
