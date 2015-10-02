{-# OPTIONS_GHC -F -pgmF htfpp #-}
import           Changelog
import           Control.Monad
import           Control.Monad.Loops (iterateUntilM)
import           Data.Char (isAlpha)
import           Data.List (nub)
import qualified Data.Set as S
import           Data.Tuple.Extra (fst3, thd3)
import           Test.Framework
import           Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as T
import           Types

main :: IO ()
main = htfMain htf_thisModulesTests


data Changelists =
       Changelists
         { oldList   :: [FunctionSignature] -- ^- Old versions list of function signatures
         , newList   :: [FunctionSignature] -- ^- New version's list of function signature
         , changeLog :: Changelog         -- ^- Changelog built together with new list.
         }
  deriving Show


prop_correctChangelog :: Changelists -> Bool
prop_correctChangelog (Changelists { oldList = old, newList = new, changeLog = manualCLog }) = newCLog == manualCLog
  where
    newCLog = buildChangelog' new old

instance Arbitrary Changelists where
  arbitrary = do
    l <- makeFunctionSignatureList

    (added, notAdded) <- sublistOfWithRemainder l
    (deleted, notAddedOrDeleted) <- sublistOfWithRemainder notAdded
    (toChangeType, unchanged) <- sublistOfWithRemainder notAddedOrDeleted

    changedType <- combineWithNewTypes toChangeType
    let new = unchanged ++ added ++ map snd changedType
    let manualCLog = Changelog {clAdded = added, clDeleted = deleted, clChangedType = changedType, clUnchanged = unchanged}
    return
      Changelists
        { oldList = notAddedOrDeleted
        , newList = new
        , changeLog = manualCLog
        }

combineWithNewTypes :: [FunctionSignature] -> T.Gen [(FunctionSignature, FunctionSignature)]
combineWithNewTypes xs = let oldTypes = S.fromList (map thd3 xs)
                         in do
                           types <- genTypesNotIn oldTypes
                           return $ map (\((m, n, t), t') -> ((m, n, t), (m, n, t'))) (xs `zip` types)

-- | Generate a list of types that are not already in the Set.
-- The list should be the same length as the size of the set.
genTypesNotIn :: S.Set Type -> T.Gen [Type]
genTypesNotIn set = T.suchThat (arbitraryStringsGTZero sz) (not . any (`S.member` set))
  where
    sz = S.size set

instance Arbitrary Changelog where
  arbitrary = do
    oldList <- makeFunctionSignatureList
    let modules = map fst3 oldList
    (deleted, notDeleted) <- sublistOfWithRemainder oldList -- Entries from oldList that were
                                                            -- "removed" in later



    (toChangeType, unchanged) <- sublistOfWithRemainder notDeleted -- Entries not deleted, but to
                                                                   -- have there types changed


    changedType <- mapM (\(m, n, _) -> nonNullString >>= \t' -> return (m, n, t')) toChangeType
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
-- A module will contain fc functions, mc modules and tc types.
  fc <- T.choose (1, 3) :: T.Gen Int
  mc <- T.choose (1, fc)
  tc <- T.choose (1, fc)

  fs <- arbitraryStringsGTZero fc
  ms <- arbitraryStringsGTZero mc
  ts <- arbitraryStringsGTZero tc

  return (distribute ms fs ts)


arbitraryStringsGTZero
  :: Int -> T.Gen [String]
arbitraryStringsGTZero n = S.toList <$> iterateUntilM ((== n) . S.size) (\s -> S.insert <$> genString <*> pure s)
                             S.empty

genString :: T.Gen String
genString = T.suchThat arbitrary (\s -> not (null s) && all isAlpha s)

-- | Make a sublist, but also return all the elements of the original
-- | list that were not in the sublist.
sublistOfWithRemainder :: Eq a => [a] -> T.Gen ([a], [a])
sublistOfWithRemainder xs = saveRemainder <$> T.sublistOf xs
  where
    saveRemainder ys = (ys, diff)
      where
        diff = filter (not . flip elem ys) xs

-- | Assign functions to modules and types
-- TODO: "Randomly" assign types to functions,
-- then "randomly" assign x *unique* (Function, Type) pairs
-- to each module. We want to simulate the situation where
-- different modules can have functions of the same name and type
-- but we don't want it so duplicate (Function, Type) pairs can be
-- in the same module.
distribute :: [ModuleName] -> [FunctionName] -> [Type] -> [FunctionSignature]
distribute ms fs ts = zip3 (cycle ms) fs (cycle ts)
