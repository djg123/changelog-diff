{-# LANGUAGE OverloadedStrings #-}
module Main where

import Run
import           Data.Maybe (fromMaybe)
import           Options.Applicative
import           OutputType

data RemoteOptions =
       RemoteOptions
         { roPackageName :: String
         , roOldVersion :: String
         , roNewVersion :: String
         }
  deriving Show
data LocalOptions = LocalOptions { loOldPath :: FilePath, loNewPath :: FilePath }
  deriving Show

data Command = Remote RemoteOptions OutputType
             | Local LocalOptions OutputType
  deriving Show


htmlConsoleFlag :: Parser OutputType
 

htmlConsoleFlag =
    fromMaybe Html <$>
    optional
        (option auto (metavar "Html/Console output" <> long "output-type"))
remoteOptions :: Parser Command
remoteOptions = Remote <$> (RemoteOptions <$> argument str (metavar "Package name"
                                                            <> help "Name of Hackage package")
                                          <*> argument str (metavar "Old version number"
                                                            <> help "Version number of old package")
                                          <*> argument str (metavar "New version number"
                                                            <> help "Version number of new package"))
                       <*> htmlConsoleFlag

localOptions :: Parser Command
localOptions = Local <$> (LocalOptions <$> argument str
                                             (metavar "Path of old local package"
                                              <> help
                                                   "Path of either package directory or .hoo file for package.")
                                       <*> argument str
                                             (metavar "Path of new local"
                                              <> help
                                                   "Path of either package directory or .hoo file for package."))
                     <*> htmlConsoleFlag


main :: IO ()
main = do
    command' <- execParser opts
    case command' of
        Local (LocalOptions{loOldPath = oldPath,loNewPath = newPath}) outputType ->
            runLocal oldPath newPath outputType
        Remote (RemoteOptions package oldVersionNum newVersionNum) outputType ->
            runRemoteExplicitDir
                (package ++ "-" ++ oldVersionNum)
                (package ++ "-" ++ newVersionNum)
                outputType
                "."
  where
    opts = info (commands <**> helper) idm


commands :: Parser Command
commands = subparser
             (command "remote"
                          (info remoteOptions
                             (progDesc
                                  "Downloads two versions of the same package from hackage and diffs them."))
             <> command "local" (info localOptions (progDesc "Compares two packages locally.")))
