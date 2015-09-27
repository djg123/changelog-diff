module Main where

import           Changelog
import           FormatConsole      (format)
import           System.Environment

main :: IO ()
main = do
  self <- getProgName
  args <- getArgs
  if length args /= 2
     then putStrLn
       ("Usage:\n"
        ++ self
        ++ " [old hoogle db path] [new hoogle db path]\n")
     else let [oldVersion, newVersion] = args
          in do
              cl <- compareModules oldVersion newVersion
              putStrLn $ format cl

