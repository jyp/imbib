{-# LANGUAGE RecordWildCards, TupleSections  #-}

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import qualified Data.Text.Lazy as BS
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Data.Traversable
import System.FilePath
import Data.Function
import System.Environment
import qualified Data.Map as M
import Config

import TypedBibData
import BibDB
import BibAttach
import qualified SuffixTreeCluster as SC
-- import Text.Groom
import MaybeIO

-------------------------------------------------------------------------
-- CheckDuplicates, method 1


pairs (x:xs) = map (x,) xs ++ pairs xs
pairs _ = []

checkDup bib = do
  -- mapM_ putString $  [ groom $ (map findTitle es, shared) | (es,shared) <- common ]
  putString "Possible duplicates:"
  mapM_ putString $ map (show . map findTitle) dups
  saveBib $ uniqDups ++ (bib \\ uniqDups) -- clump together the duplicates
  where
    uniqDups = nub $ concat $ dups
    trueDups es =  filter (\e -> any (not . (areRelated e)) es) es
    dups = [ es | (es0,shared) <- common, 
             let es = trueDups es0, 
             not (null es),
             sum (map length shared) >= threshold (minimum (map (length . project . findTitle) es))]
    threshold x = (9 * x) `div` 10
    -- SC.printTree $ SC.select ("", clusterTree)
    common = M.toList $ SC.commonStrings info
    clusterTree = SC.construct info
    info = [(project $ findTitle e,[e]) | e <- bib]
 
-------------------------------------------------------------------------
-- Auto-renaming of attachments

rename :: (String -> String) -> (String,String) -> MaybeIO (String,String) 
rename new (oldfname,typ) 
    | oldfname == newfname = bail
    | otherwise = do
  oex <- doesFileExist oldfname
  nex <- doesFileExist newfname
  case (oex,nex) of
    (True,False) -> do renameFile oldfname newfname
                       return (newfname,typ)
    (False,_) -> putString ("old does not exist: " ++ oldfname) >> bail
    (_,True) -> putString  ("new already exists: " ++ newfname) >> bail
    -- (_,True) -> do removeFile oldfname >> return (newfname,typ)
                        
  where newfname = new typ
        bail = return (oldfname,typ)

renamer :: Entry -> MaybeIO Entry
renamer t@Entry{..} = do 
  files <- traverse (rename (findAttachName t)) files
  return $ Entry{..}

renameAttachments bib = do 
  bib' <- traverse renamer bib
  saveBib bib'

------------------------------------------------------------------------
-- Check for orphans

check :: (String,String) -> MaybeIO () 
check (fname,typ) = do
  ex <- doesFileExist fname
  if (not ex) then putString $ "missing: " ++ fname else return ()

checker Entry{..} = mapM_ check files

getDirectoryContents' d = map (d </>) <$> getDirectoryContents d

checkAttachments bib = do
  fnames <- getDirectoryContents' "/home/bernardy/Papers"
  let attachments = [ f | e <- bib, (f,t) <- files e] 
      
  mapM_ putString (fnames \\ attachments)

----------------------------------------------------------------------
-- Merge another bibtex file

mergeIn bib fname = do
  bib2 <- uncheckedHarmless $ rightOrDie <$> loadBibliographyFrom fname
  checkDup $ bib2 ++ bib
  return ()


{-
mergeBibs bib1 bib2 = 
    where bibM1 = M.fromList [(project $ findTitle e, e) | e <- bib1]
-}          


-----------------------------------------------------------------------
-- Harvest Downloads

harvest bib = do
  contents <- getDirectoryContents' downloadsDirectory
  let oldFiles = concatMap (map snd . files) bib
      newFiles = contents \\ oldFiles 
      papers = [Entry {kind = "download", 
                       seeAlso = [],
                       authors = [],
                       files = [(fname,guessTypeByName fname)],
                       otherFields = [("title",fname),("date","2010")]
                      } | fname <- newFiles, takeExtension fname `elem` [".pdf",".ps"]]
  saveBib $ papers ++ bib

------------------------------------------------------------------------
-- Driver

saveBib b = safely "Saving bibfile" $ saveBibliography b


go (command: ~(arg1:_)) = do 
  bib <- uncheckedHarmless $ (rightOrDie <$> loadBibliography)
  case command of
    "check" -> checkAttachments bib
    "rename" -> renameAttachments bib
    "merge" -> mergeIn bib arg1
    "harvest" -> harvest bib
    "dup" -> checkDup bib

dryRun = run True
trueRun = run False

main = do 
  args <- getArgs
  case args of
    ("dry":args) -> dryRun $ go args
    _ -> trueRun $ go args