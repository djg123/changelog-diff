module Main where

import           Changelog
import           Css
import qualified Data.ByteString.Lazy as L
import qualified FormatHTML as H
import qualified FormatConsole as C
import qualified System.Directory as D
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  self <- getProgName
  args <- getArgs
  if length args /= 4
    then putStrLn
           ("Usage:\n"
            ++ self
               ++ " [old hoogle db path] [new hoogle db path] [output directory] [html/console]\n")
    else let [oldVersion, newVersion, outputPath, outputType] = args
         in do
           cl <- compareModules oldVersion newVersion
           case outputType of
             "html" -> writeHtml outputPath cl
             "console" ->
               putStrLn (C.format cl)
             e -> error $ "Unsupported output type: " ++ e

cssPath :: FilePath
cssPath = "out.css"

writeHtml :: FilePath -> Changelog -> IO ()
writeHtml outputPath cl = do 
  D.createDirectoryIfMissing True outputPath
  L.writeFile (outputPath </> "out.html") (H.format cssPath cl) 
  writeCss (outputPath </> cssPath)



