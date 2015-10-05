module Main where

import Changelog
import qualified Data.ByteString.Lazy as L
import FormatHTML (format)
import System.Environment

main :: IO ()
main = do
  self <- getProgName
  args <- getArgs
  if length args /= 3
     then putStrLn $ ("Usage:\n" 
                           ++ self 
                           ++ " [old hoogle db path] [new hoogle db path] [output path]\n")
     else let [oldVersion, newVersion, outputPath] = args
          in do
              cl <- compareModules oldVersion newVersion
              L.writeFile outputPath (format cl)
