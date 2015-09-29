module Changelog (compareModules, Changelog(..), groupByModule) where

import           Control.Monad (replicateM)
import           Data.Char (isAlpha)
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Tuple.Extra (both, (&&&), (***), fst3)
import qualified Hoogle as H
import           Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as T

type FunctionSignature = (ModuleName, FunctionName, Type)
type FunctionName = String
type Type = String
type ModuleName = String


data Changelog = Changelog
  {clAdded       :: [FunctionSignature]
  ,clDeleted     :: [FunctionSignature]
  ,clChangedType :: [(FunctionSignature, FunctionSignature)]
  ,clUnchanged   :: [FunctionSignature]}
  deriving Show

instance Monoid Changelog where
  mempty = Changelog { clAdded = [], clDeleted = [], clChangedType = [], clUnchanged = [] }
  mappend cl1 cl2 = Changelog
    { clAdded = clAdded cl1 <> clAdded cl2
    , clDeleted = clDeleted cl1 <> clDeleted cl2
    , clChangedType = clChangedType cl1 <> clChangedType cl2
    , clUnchanged = clUnchanged cl1 <> clUnchanged cl2
    }

instance Arbitrary Changelog where
  arbitrary = do
    oldList <- makeFunctionSignatureList
    let modules = map fst3 oldList
    (deleted, notDeleted) <- sublistOfWithRemainder oldList -- Entries from oldList that were
                                                            -- "removed" in later


    (toChangeType, unchanged) <- sublistOfWithRemainder notDeleted -- Entries not deleted, but to
                                                                   -- have

             -- their types changed
    changedType <- mapM
                     (\(m, n, t) ->
                        ( nonNullString) >>= \t' ->
                          return (m, n, t'))
                     toChangeType
    added <- makeFunctionAndTypeList modules -- List of random functions and types only to be "added"

    return
      Changelog
        { clAdded = added
        , clDeleted = deleted
        , clChangedType = toChangeType `zip` changedType
        , clUnchanged = unchanged
        }
    where
      nonNullString = head <$> arbitraryStringsGTZero 1 :: T.Gen String

-- | Like makeFunctionSignatureList, but with existing modules.
makeFunctionAndTypeList :: [ModuleName] -> T.Gen [FunctionSignature]
makeFunctionAndTypeList mns = fmap (zipJoin (cycle mns) . map (\(_, b, c) -> (b, c)))
                                makeFunctionSignatureList
  where
    xs `zipJoin` ys = map (\(a, (b, c)) -> (a, b, c)) (xs `zip` ys)

-- | Generate function signature lists
makeFunctionSignatureList :: T.Gen [FunctionSignature]
makeFunctionSignatureList = do
  fc <- T.choose (1, 50) :: T.Gen Int
  mc <- T.choose (1, fc)
  tc <- T.choose (1, fc)

  fs <- arbitraryStringsGTZero fc
  ms <- arbitraryStringsGTZero mc
  ts <- arbitraryStringsGTZero tc

  return (distribute ms fs ts)
    
arbitraryStringsGTZero
  :: Int -> T.Gen [String]
arbitraryStringsGTZero n = replicateM n $ T.suchThat arbitrary (\s -> not (null s) && all isAlpha s) 

-- | Make a sublist, but also return all the elements of the original
-- | list that were not in the sublist.
sublistOfWithRemainder :: Eq a => [a] -> T.Gen ([a], [a])
sublistOfWithRemainder xs = saveRemainder <$> T.sublistOf xs
  where
    saveRemainder ys = (ys, diff)
      where
        diff = filter (not . flip elem ys) xs

-- | Assign functions to modules and types
distribute :: [ModuleName] -> [FunctionName] -> [Type] -> [FunctionSignature]
distribute ms fs ts = [(mn, fn, tn) | fn <- fs, mn <- ms, tn <- ts]


insertAdded :: FunctionSignature -> Changelog -> Changelog
insertAdded v cl = cl {clAdded = v : clAdded cl}

insertDeleted :: FunctionSignature -> Changelog -> Changelog
insertDeleted v cl = cl {clDeleted = v : clDeleted cl}

insertChangedType
  :: (FunctionSignature, FunctionSignature) -> Changelog -> Changelog
insertChangedType v cl = cl {clChangedType = v : clChangedType cl}

insertUnchanged :: FunctionSignature -> Changelog -> Changelog
insertUnchanged v cl = cl {clUnchanged = v : clUnchanged cl}

groupByModule :: Changelog -> M.Map ModuleName Changelog
groupByModule (Changelog
                 { clAdded = added
                 , clDeleted = deleted
                 , clChangedType = changedType
                 , clUnchanged = unchanged
                 }) = foldr (M.unionWith (<>)) mempty [added', deleted', unchanged', changedType']
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
