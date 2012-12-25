module BibAttach where

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text.Lazy.IO as BS
import qualified Data.Text.Lazy as BS
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

findAttachName entry ext = attachmentsRoot </> (sanitize $ findTitle entry ++ "-" ++ findYear entry ++ "." ++ ext)


guessType :: FilePath -> BS.Text -> String
guessType fname contents 
    | BS.head contents == '@' = "bib"
    | magic == pdfMagic = "pdf"
    | magic == psMagic = "ps"
    | otherwise = drop 1 $ takeExtension fname
   where magic = BS.take 4 contents
         pdfMagic = BS.pack "%PDF"
         psMagic  = BS.pack "%!PS"
        