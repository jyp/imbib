module MaybeIO (
                MaybeIO,
                doesFileExist,
                renameFile, removeFile,
                putString,
                getDirectoryContents,
                uncheckedHarmless, safely,
                run,
               ) where

import qualified System.Directory as R
import Prelude hiding (putStrLn)
import qualified Prelude as R
import Control.Monad (ap,liftM)
import Control.Applicative

data MaybeIO a where
    Return :: a -> MaybeIO a
    (:>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    Safe :: IO a -> MaybeIO a
    Harmful :: String -> IO () -> MaybeIO ()

instance Functor MaybeIO where
    fmap = liftM

instance Applicative MaybeIO where
    pure = Return
    (<*>) = ap

run :: Bool -> MaybeIO a -> IO a
run dry mio = run' mio
 where
  run' :: forall a. MaybeIO a -> IO a
  run' (Return a) = return a
  run' (a :>>= b) = do x <- run' a
                       run' (b x)
  run' (Safe i) = i
  run' (Harmful msg i) = if dry then R.putStrLn ('|':msg) else i

instance Monad MaybeIO where
    (>>=) = (:>>=)
    return = Return                     

uncheckedHarmless = Safe
safely = Harmful


doesFileExist f = uncheckedHarmless $ R.doesFileExist f

putString = uncheckedHarmless . R.putStrLn

getDirectoryContents = uncheckedHarmless . R.getDirectoryContents

renameFile :: String -> String -> MaybeIO ()
renameFile old new = do  
  safely ("RENAME: " ++ old ++ " TO " ++ new)
         (R.renameFile old new)

 
removeFile :: String -> MaybeIO ()
removeFile f = do
  safely ("DELETE: " ++ f) (R.removeFile f)