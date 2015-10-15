module Main where

import           Changelog
import           Css
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromMaybe)
import qualified FormatConsole as C
import qualified FormatHTML as H
import           Options.Applicative
import qualified System.Directory as D
import           System.Environment
import           System.FilePath

data VersionsOptions = VersionsOptions { voPackageName :: String } deriving (Read, Show)
data RemoteOptions =
       RemoteOptions
         { roPackageName :: String
         , roOldVersion :: String
         , roNewVersion :: String
         } deriving Show
data LocalOptions = LocalOptions { loOldPath :: FilePath, loNewPath :: FilePath } deriving (Read, Show)

data Command = Versions VersionsOptions OutputType
             | Remote RemoteOptions OutputType
             | Local LocalOptions OutputType deriving Show

data OutputType = Html
                | Console deriving Show

instance Read OutputType where
  readsPrec _ "html" = [ (Html, "html") ]
  readsPrec _ "console" = [ (Console, "console") ]
  readsPrec _ _ = []

htmlConsoleFlag :: Parser OutputType
htmlConsoleFlag = fromMaybe Html <$> optional (option auto (metavar "Html/Console output"
                                                        <> long "output-type")) 

versionsOptions :: Parser Command
versionsOptions = Versions <$> option auto
                                 (metavar "Package name"
                                  <> help "Name of Hackage package to request version numbers for")
                           <*> htmlConsoleFlag

remoteOptions :: Parser Command
remoteOptions = Remote <$> (RemoteOptions <$> option auto (metavar "Package name"
                                                           <> help "Name of Hackage package")
                                          <*> option auto (metavar "Old version number"
                                                           <> help "Version number of old package")
                                          <*> strOption (metavar "New version number"
                                                         <> help "Version number of new package"))
                       <*> htmlConsoleFlag

localOptions :: Parser Command
localOptions = Local <$> option auto
                           (metavar "Path of old local package"
                            <> help "Path of either package directory or .hoo file for package.")
                     <*> option auto
                           (metavar "Path of new local"
                            <> help "Path of either package directory or .hoo file for package.")

main :: IO ()
main = execParser opts >>= print
  where
    opts = info (commands <**> helper) idm


commands :: Parser Command
commands = subparser
                  (command "versions"
                     (info versionsOptions
                        (progDesc "Show versions of a given package from Hackage."))
                   <> command "remote"
                        (info remoteOptions
                           (progDesc
                              "Downloads two versions of the same package from hackage and diffs them."))
                   <> command "local" (info localOptions (progDesc "Compares two packages locally.")))

cssPath :: FilePath
cssPath = "out.css"

writeHtml :: FilePath -> Changelog -> IO ()
writeHtml outputPath cl = do
  D.createDirectoryIfMissing True outputPath
  L.writeFile (outputPath </> "out.html") (H.format cssPath cl)
  writeCss (outputPath </> cssPath)
