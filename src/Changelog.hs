module Changelog (compareModules, Changelog(..), groupByModule, buildChangelog') where

import           Control.Arrow ((&&&), (***))
import           Control.Exception (try, catch, SomeException)
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Debug.Trace
import qualified Hoogle as H
import           System.Directory (getCurrentDirectory)
import           Types

data Changelog =
       Changelog
         { clAdded       :: [FunctionSignature]
         , clDeleted     :: [FunctionSignature]
         , clChangedType :: [(FunctionSignature, FunctionSignature)]
         , clUnchanged   :: [FunctionSignature]
         }
  deriving (Show, Eq)

instance Monoid Changelog where
  mempty = Changelog { clAdded = [], clDeleted = [], clChangedType = [], clUnchanged = [] }
  mappend cl1 cl2 = Changelog
    { clAdded = clAdded cl1 <> clAdded cl2
    , clDeleted = clDeleted cl1 <> clDeleted cl2
    , clChangedType = clChangedType cl1 <> clChangedType cl2
    , clUnchanged = clUnchanged cl1 <> clUnchanged cl2
    }


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

    step f tup@(m, _, _) z = if m `M.member` z
                               then M.adjust (f tup) m z
                               else M.insert m (f tup mempty) z



    stepChangedType tup@((m, _, _), _) z =
      if m `M.member` z
        then M.adjust (insertChangedType tup) m z
        else M.insert m (insertChangedType tup mempty) z

-- | Takes in the filepaths of the Hoogle databases (*.hoo files)
-- of the old and new modules and produces a Changelog.
compareModules :: FilePath -- ^ Path of old version's Hoogle database.
               -> FilePath -- ^ Path of new version's Hoogle database.
               -> IO Changelog -- ^ Resulting Changelog.
compareModules oldVersion newVersion = do
  [oldProcessed, newProcessed] <- mapM (fmap processResults . getHoogleTags)
                                    [oldVersion, newVersion]
  return $ buildChangelog' newProcessed oldProcessed

-- | Used in buildChangelog' to assign a status to each of the individual
-- functions in the old module in the new module (or vice versa.)
data FunctionStatus a
    = Deleted a -- ^ Status of a function that exists only in the old module.
    | ChangedType { old :: a
                  , new :: a} -- ^ Status of a function that has the same name, but different types in each of the two modules.
    | Unchanged a -- ^ Status of a function that has both the same name and type in each of the versions of the module.
    | Added a -- ^ Status of a function that exists only in the new module, but not the old module.
    deriving (Show)



-- | Builds Changelog from lists of function signatures
buildChangelog' :: [FunctionSignature] -- ^ List of function signatures from new version.
                -> [FunctionSignature] -- ^ List of function signatures from old version.
                -> Changelog -- ^ Resulting Changelog
buildChangelog' new old = Changelog added' deleted'
                                           (nub changeType')
                                           (nub unchanged')
  where
    (added', deleted', changeType', unchanged') = foldr step ([], [], [], [])
                                                             (oldInNew ++ newInOld)

    step x (a, d, c, u) = case x of
                            Added v -> (v:a, d, c, u)
                            Deleted v -> (a, v:d, c, u)
                            ChangedType o n -> (a, d, (o, n):c, u)
                            Unchanged v -> (a, d, c, v:u)

    -- | The status of all of the functions from the old module in the new module.
    -- For example, a function in the old module that does not exist in the new module
    -- will be assigned the status Deleted, while a function that has the same name,
    -- but different types in the old and new modules respectively will be assigned
    -- a status of ChangedType.
    oldInNew :: [FunctionStatus FunctionSignature]
    oldInNew = do
      sig@(n, m, t) <- old
      return $ case (n, m) `M.lookup` newM of
                 Nothing -> Deleted sig
                 Just typeName -> if typeName == t
                                    then Unchanged sig
                                    else ChangedType {old = sig,new = (n,m, typeName)}

    -- | The status of all of the functions from the new module in the old module.
    -- Similar to oldInNew.
    newInOld :: [FunctionStatus FunctionSignature]
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



-- | Tags in Hoogle tags and extracts FunctionSignatures.
processResults :: [(H.Score, H.Result)] ->  [FunctionSignature]
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
    err x = error ("Hoogle has changed its query language!"
                   ++ " and has thrown error: " ++ show x)

getHoogleTags :: FilePath -> IO [(H.Score, H.Result)]
getHoogleTags dbPath =
    catch
        (H.search <$> H.loadDatabase dbPath <*> pure query)
        (\e ->
              do pwd <- getCurrentDirectory
                 error $
                     "Excpetion: " ++
                     show (e :: SomeException) ++ " " ++
                     "Trouble getting Hoogle tags for directory: " ++
                     pwd ++ " and path: " ++ dbPath)
