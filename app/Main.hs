module Main where

import           Changelog
import           Css
import qualified Data.ByteString.Lazy as L
import           FormatHTML (format)
import qualified System.Directory as D
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  self <- getProgName
  args <- getArgs
  if length args /= 3
    then putStrLn
           ("Usage:\n"
            ++ self
               ++ " [old hoogle db path] [new hoogle db path] [output directory]\n")
    else let [oldVersion, newVersion, outputPath] = args
         in do
           cl <- compareModules oldVersion newVersion
           writeHtml outputPath cl

cssPath :: FilePath
cssPath = "out.css"

writeHtml :: FilePath -> Changelog -> IO ()
writeHtml outputPath cl = do 
  D.createDirectoryIfMissing True outputPath
  L.writeFile (outputPath </> "out.html") (format cssPath cl) 
  writeCss (outputPath </> cssPath)



