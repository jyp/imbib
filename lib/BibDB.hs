module BibDB where

import Text.ParserCombinators.Parsec (parseFromFile)
import TypedBibData
import Config

------------
-- DB


loadBibliography cfg = loadBibliographyFrom (bibfile cfg)

loadBibliographyFrom fileName = do
  putStrLn ("Loading " ++ fileName)
  mBib <- fmap bibToForest <$> parseFromFile parseBib fileName 
  case mBib of
    Left err -> return (Left err)
    Right bib -> do putStrLn $ show (length $ bib) ++ " entries loaded." -- force it right here
                    return (Right bib)

formatBib :: [Entry] -> [Char]
formatBib = concatMap formatEntry . map treeToEntry

saveBibliography :: InitFile -> [Entry] -> IO ()
saveBibliography cfg bib = do
  writeFile (bibfile cfg) (formatBib bib)
  putStrLn $ show (length bib) ++ " entries saved to " ++ (bibfile cfg)


