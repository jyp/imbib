module Config where
    
import Paths_imbib
import System.IO.Unsafe
import System.FilePath
import System.Process
import System.Directory
import Data.ConfigFile
import Control.Applicative

rightOrDie (Left err) = error (show err)
rightOrDie (Right x) = x

startConfig = rightOrDie $ 
  set (emptyCP {accessfunc = interpolatingAccess 10}) "DEFAULT" "home" homeDirectory

configFileName = ".imbib"

configuration = unsafePerformIO $ do
  c0f <- getDataFileName configFileName                  
  c0 <- rightOrDie <$> readfile startConfig c0f
  let user = homeDirectory </> configFileName
  ex <- doesFileExist user
  if ex then  rightOrDie <$> readfile c0 user else return c0
  
getOption = rightOrDie . get configuration "DEFAULT"

downloadsDirectory = getOption "watched"
attachmentsRoot = getOption "archive"
bibfile = getOption "library"
runEditor lineNumber file = runProcess' (getOption "editor") ['+':show lineNumber,file]
runViewer file = runProcess' (getOption "viewer") [file] 

-- The following should probably not change
homeDirectory = unsafePerformIO $ getHomeDirectory
iconFile = unsafePerformIO $ getDataFileName "icon.svg"

runProcess' = \bin args -> runProcess bin args Nothing Nothing Nothing Nothing Nothing
