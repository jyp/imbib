module Config where

import System.FilePath
import System.Process
import System.Directory
import Data.ConfigFile

rightOrDie :: Show a => Either a p -> p
rightOrDie (Left err) = error (show err)
rightOrDie (Right x) = x


configFileName :: [Char]
configFileName = ".imbib"

getConfiguration :: IO ConfigParser
getConfiguration = do
  homeDirectory <- getHomeDirectory
  let startConfig = rightOrDie $ set (emptyCP {accessfunc = interpolatingAccess 10}) "DEFAULT" "home" homeDirectory
  let c0 = rightOrDie (readstring startConfig dflt)
  let user = homeDirectory </> configFileName
  ex <- doesFileExist user
  if ex then rightOrDie <$> readfile c0 user else return c0



data InitFile = InitFile
     {
       attachmentsRoot, downloadsDirectory, bibfile :: FilePath,
       runViewer :: String -> IO ProcessHandle,
       runEditor :: Int -> String -> IO ProcessHandle 
     }

dflt :: String
dflt = "\
  \watched = %(home)s/Downloads\n\
  \archive = %(home)s/Papers\n\
  \library = %(home)s/library.bib\n\
  \viewer = /usr/bin/evince\n\
  \editor = /usr/bin/emacs\n\
  \"

loadConfiguration :: IO InitFile
loadConfiguration = do
  cfg <- getConfiguration
  let getOption = get cfg "DEFAULT"
  return $ rightOrDie $ do
    downloadsDirectory <- getOption "watched"
    attachmentsRoot <- getOption "archive"
    bibfile <- getOption "library"
    editor <- getOption "editor"
    viewer <- getOption "viewer"
    let runEditor lineNumber file = runProcess' editor ['+':show lineNumber,file]
        runViewer file = runProcess' viewer [file]
    return InitFile{..}

-- The following should probably not change
-- iconFile = unsafePerformIO $ getDataFileName "icon.svg"

runProcess' :: FilePath -> [String] -> IO ProcessHandle
runProcess' = \bin args -> runProcess bin args Nothing Nothing Nothing Nothing Nothing
