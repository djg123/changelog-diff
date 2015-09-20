module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  self <- getProgName
  args <- getArgs
  if length args /= 2
     then putStrLn $ ("Usage:\n" 
                           ++ self 
                           ++ " [old hoogle db path] [new hoogle db path]\n")
     else let [oldVersion, newVersion] = args
          in do
              cl <- fmap show $  compareModules oldVersion newVersion
              putStrLn $ show cl
