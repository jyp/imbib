module BibDB where

import Text.ParserCombinators.Parsec (parseFromFile)
import TypedBibData
import Config
import Control.Monad

------------
-- DB


loadBibliography cfg = loadBibliographyFrom (bibfile cfg)

loadBibliographyFrom fileName = do
  putStrLn ("Loading " ++ fileName)
  mBib <- fmap bibToForest <$> parseFromFile parseBib fileName
  case join mBib of
    Left err -> return (Left err)
    Right bib -> do putStrLn $ show (length $ bib) ++ " entries loaded."
                    return (Right bib)

formatBib :: [Entry] -> [Char]
formatBib = concatMap formatEntry . map treeToEntry

saveBibliography :: InitFile -> [Entry] -> IO ()
saveBibliography cfg bib = do
  let formatted = formatBib bib
  writeFile (bibfile cfg) formatted 
  putStrLn $ show (length bib) ++ " entries saved to " ++ (bibfile cfg)


