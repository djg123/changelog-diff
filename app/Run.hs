module Run (
    runLocal,
    runRemote,
    runVersions,
    ) where

import OutputType (OutputType(..))
import qualified Changelog as C
import qualified FormatConsole as C
import qualified FormatHTML as H
import           System.FilePath
import Css (writeCss)
import qualified Data.ByteString.Lazy as L
import qualified System.Directory as D

-- Name of css file within output directory
cssPath :: FilePath
cssPath = "out.css"

-- Output dir path for Html output
outputPath :: FilePath
outputPath = "out"

runLocal :: FilePath -> FilePath -> OutputType -> IO ()
runLocal oldPath newPath type_ = do
  cl <- C.compareModules oldPath newPath
  case type_ of
    Console -> writeConsole cl
    Html    -> writeHtml outputPath cl



writeHtml :: FilePath -> C.Changelog -> IO ()
writeHtml outputPath cl = do
  D.createDirectoryIfMissing True outputPath
  L.writeFile (outputPath </> "out.html") (H.format cssPath cl)
  writeCss (outputPath </> cssPath)

writeConsole :: C.Changelog -> IO ()
writeConsole cl = putStrLn (C.format cl)

runRemote = undefined

runVersions = undefined
