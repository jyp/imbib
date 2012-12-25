module BibDB where


import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.Trans
-- import qualified Data.ByteString 
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tree
import System.Directory
import System.FilePath
import Data.Function

import Text.ParserCombinators.Parsec (parseFromFile)
import TypedBibData
import Config

------------
-- DB


loadBibliography = loadBibliographyFrom bibfile

loadBibliographyFrom fileName = do
  mBib <- fmap bibToForest <$> parseFromFile parseBib fileName 
  case mBib of
    Left err -> return (Left err)
    Right bib -> do putStrLn $ show (length $ bib) ++ " entries loaded." -- force it right here
                    return (Right bib)

formatBib = concatMap formatEntry . map treeToEntry

saveBibliography :: [TypedBibData.Entry] -> IO ()
saveBibliography bib = do
  writeFile bibfile (formatBib bib)
  putStrLn $ show (length bib) ++ " entries saved to " ++ bibfile


