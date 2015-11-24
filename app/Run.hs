{-# LANGUAGE OverloadedStrings #-}
module Run (runLocal, runRemote) where

import Control.Exception (catch, SomeException)
import qualified Changelog as C
import Control.Monad.Managed (with)
import Css (writeCss)
import qualified Data.ByteString.Lazy as L
import Data.Text (intercalate, pack, split, unpack)
import qualified FormatConsole as C
import qualified FormatHTML as H
import OutputType (OutputType(..))
import qualified System.Directory as D
import System.FilePath ((</>))
import Turtle ((<.>), (<>))
import qualified Turtle as T
import Turtle.Prelude ((.&&.), (.||.))

-- | Name of css file within output directory
cssPath :: FilePath
cssPath = "out.css"

-- | Output dir path for Html output
defaultOutputPath :: FilePath
defaultOutputPath = "out"

-- | Run on existing *.hoo files.
runLocal
    :: FilePath -- ^ Path of earlier version's .hoo output.
    -> FilePath -- ^ Path of later version's .hoo output.
    -> OutputType -- ^ Type of output (HTML/Console).
    -> IO ()
runLocal oldPath newPath type_ = do
    cl <- C.compareModules oldPath newPath
    case type_ of
        Console -> writeConsole cl
        Html -> writeHtml defaultOutputPath cl


-- | Calls either writeHtml or writeConsole based on OutputType.
write :: OutputType -> C.Changelog -> IO ()
write Html = writeHtml defaultOutputPath
write Console = writeConsole

-- | Writes Changelog to given output path.
writeHtml :: FilePath -- ^ Directory to write HTML output to
          -> C.Changelog -- ^ Changelog
          -> IO ()
writeHtml outputPath cl = do
  D.createDirectoryIfMissing True outputPath
  L.writeFile (outputPath </> "out.html") (H.format cssPath cl)
  writeCss (outputPath </> cssPath)

-- | Writes Changelog to STDOUT.
writeConsole :: C.Changelog -> IO ()
writeConsole cl = putStrLn (C.format cl)


-- | Just print out exit code for now.
handleExitFailure :: Int -> IO ()
handleExitFailure n = putStrLn $ "Failed with exit code: " ++ show n 


-- | Use cabal to download two different versions of the package,
-- generate *.hoo files, and then output them based on the OutputType.
-- Cleans up all extra files created when finished.
runRemote :: String -- ^ Old version of package, example: cassava-0.1.0.1
          -> String -- ^ New version of package, example: cassava-0.4.4.0
          -> OutputType -- ^ Output to console or write html to disk.
          -> IO ()
runRemote oldVersion newVersion outputType =
  with (T.mktempdir "." "") (\dir -> putStrLn "Calling runRemoteExplicitDir" >> runRemoteExplicitDir oldVersion newVersion outputType dir)
  `catch` \e -> error $ "Failed with: " ++ show (e :: SomeException)

-- | Same as runRemote, but specifies where to save packages to rather than using
-- a temp directory.
runRemoteExplicitDir :: String -> String -> OutputType -> T.FilePath -> IO ()
runRemoteExplicitDir oldVersion newVersion outputType dir =
  do
    T.cd dir
    putStrLn "Running cabal commands"
    exitCode <- cabalUnpack oldVersion'
                .&&. cabalUnpack newVersion'
                .&&. mkHooFile oldVersion'
                .&&. mkHooFile newVersion'
                .&&. hoogleConvert oldVersion'
                .&&. hoogleConvert newVersion'
    case exitCode of
      T.ExitFailure n -> handleExitFailure n
      T.ExitSuccess -> do
        putStrLn "Exit Success, running Changelog.compareModules"
        cl <- C.compareModules (oldVersion ++ ".hoo") (newVersion ++ ".hoo")
        T.cd ".."
        write outputType cl
  where
    hoogleConvert :: T.Text -> IO T.ExitCode
    hoogleConvert package = T.shell ("hoogle convert " <> package <> ".txt") T.empty

    cabalUnpack :: T.Text -> IO T.ExitCode
    cabalUnpack package = T.shell ("cabal unpack " <> package) T.empty

    cabalSandboxInit :: IO T.ExitCode
    cabalSandboxInit = T.shell "cabal sandbox init" T.empty

    cabalInstallDependencies :: IO T.ExitCode
    cabalInstallDependencies = T.shell "cabal install --dependencies-only" T.empty

    cabalConfigure :: IO T.ExitCode
    cabalConfigure = T.shell "cabal configure" T.empty

    oldVersion' = pack oldVersion
    newVersion' = pack newVersion

    mkHooFile :: T.Text -> IO T.ExitCode
    mkHooFile package = do
      T.cd package'

      exitCode <- cabalSandboxInit
                  .&&. cabalInstallDependencies
                  .&&. cabalConfigure
                  .&&. (cabalHaddockHoogle .||. error ("Failed running cabal haddock --hoogle for " ++ unpack package))
      case exitCode of
        T.ExitSuccess -> do
          T.cp ("dist/doc/html" <> package'noVersion <> package'noVersion <.> "txt") (".." <> package' <.> "txt")
          T.cd ".."
          return T.ExitSuccess
        _ -> return exitCode

      where
        package' = T.fromText package
        package'noVersion = T.fromText . intercalate "-" . init . split (== '-') $ package

cabalHaddockHoogle :: IO T.ExitCode
cabalHaddockHoogle = T.shell "cabal haddock --hoogle" T.empty

-- | Temporarily chdir to a given directory, then execute an IO action.
-- Finally, chdirs back to the original directory.
withChdir :: T.MonadIO m 
          => T.FilePath -- ^ Directory path to temporarily chdir to
          -> m b -- ^ IO Action to execute
          -> m b
withChdir path io = do
  curDir <- T.pwd
  T.cd path
  result <- io
  T.cd curDir
  return result
