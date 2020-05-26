module BibAttach where

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tree
import System.Directory
import System.FilePath
import System.Process
import Data.Function
import System.FilePath

import TypedBibData
import Config

--------------------------------------
-- Attachment manipulation

sanitize = filter (\c -> c`notElem` ";:{}/\\" && ord c < 128) 
-- there seems to be a problem with big chars in filenames at the moment.
-- see http://hackage.haskell.org/trac/ghc/ticket/3307

-- save a (binary) file, safely
saveFile name contents = do
  putStrLn $ "Creating directory for " ++ name
  createDirectoryIfMissing True (dropFileName name)
  putStrLn $ "Writing..."
  BS.writeFile name contents
  return name


findAttachName cfg entry ext = attachmentsRoot cfg </> (sanitize $ findTitle entry ++ "-" ++ findYear entry ++ "." ++ ext)


guessTypeByName fname = drop 1 $ takeExtension fname


guessType :: FilePath -> BS.ByteString -> String
guessType fname contents 
    | BS.head contents == ord' '@' = "bib"
    | magic == pdfMagic = "pdf"
    | magic == psMagic = "ps"
    | otherwise = guessTypeByName fname
   where magic = BS.take 4 contents
         pdfMagic = BS.pack $ map ord' "%PDF"
         psMagic  = BS.pack $ map ord' "%!PS"
         ord' = fromIntegral . ord 

