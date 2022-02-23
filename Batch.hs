{-# LANGUAGE RecordWildCards, TupleSections  #-}

import Data.List
import System.FilePath
import qualified Data.Map as M
import Config
import Options.Applicative

import TypedBibData
import BibDB
import BibAttach
import qualified SuffixTreeCluster as SC
import Text.Groom
import MaybeIO
import Diff
import Data.Function (on)
import Control.Monad
-------------------------------------------------------------------------
-- CheckDuplicates, method 1


pairs :: [t] -> [(t, t)]
pairs (x:xs) = map (x,) xs ++ pairs xs
pairs _ = []

dist :: String -> String -> Int
dist x y = length $ filter (/= B) $ map fst $ diff' (project x) (project y)

checkDup :: InitFile -> [Entry] -> MaybeIO ()
checkDup cfg bib' = do
  -- mapM_ putString $  [ groom $ (map findTitle es, shared) | (es,shared) <- common ]
  putString ("Removing exact duplicates:" ++ show (length bib' - length bib))
  -- putString ("Candidate duplicate groups:\n" ++ (groom $ [(shared,map findTitle es) | (es,shared) <- dupGroups]))
  uncheckedHarmless $ mapM_ print [(findTitle e1,findTitle e2) | (e1,e2) <- dups]
  sortBib cfg $ bib -- (uniqDups ++ (bib \\ uniqDups))
  where dups = [(e1,e2) |
                (es,_) <- dupGroups,
                (e1,e2) <- pairs $ es,
                not (areRelated e1 e2), -- no "see also" field
                dist (findTitle e1) (findTitle e2) <= 10 -- edit distance
               ]
        dupGroups = [(es,shared) | (es,shared) <- common,
                     -- a sufficently long substring is shared
                     maximum (map length shared) >= threshold (minimum (map (length . project . findTitle) es))]
        threshold x = (9 * x) `div` 10
        -- SC.printTree $ SC.select ("", clusterTree)
        common = M.toList $ SC.commonStrings info
        -- clusterTree = SC.construct info
        info = [(project $ findTitle e,[e]) | e <- bib]
        bib = nub bib'

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

renamer :: InitFile -> Entry -> MaybeIO Entry
renamer cfg t@Entry{..} = do
  files <- traverse (rename (findAttachName cfg t)) files
  return $ Entry{..}

renameAttachments :: InitFile -> [Entry] -> MaybeIO ()
renameAttachments cfg bib = do 
  bib' <- traverse (renamer cfg) bib
  saveBib cfg bib'

------------------------------------------------------------------------
-- Check for orphans

check :: (String,String) -> MaybeIO () 
check (fname,_typ) = do
  ex <- doesFileExist fname
  if (not ex) then putString $ "missing: " ++ fname else return ()

checker :: Entry -> MaybeIO ()
checker Entry{..} = mapM_ check files

getDirectoryContents' :: FilePath -> MaybeIO [FilePath]
getDirectoryContents' d = map (d </>) <$> getDirectoryContents d

checkAttachments :: InitFile -> [Entry] -> MaybeIO ()
checkAttachments cfg bib = do
  fnames <- getDirectoryContents' (attachmentsRoot cfg)
  let attachments = [ f | e <- bib, (f,_t) <- files e] 

  mapM_ putString (fnames \\ attachments)

----------------------------------------------------------------------
-- Merge another bibtex file

cleanImported :: Entry -> Entry
cleanImported Entry {..} = Entry {files=[],..}

mergeIn :: InitFile -> [Entry] -> String -> MaybeIO ()
mergeIn cfg bib fname = do
  bib2 <- uncheckedHarmless (rightOrDie =<< loadBibliographyFrom fname)
  
  saveBib cfg $ map cleanImported bib2 ++ bib
  return ()


{-
mergeBibs bib1 bib2 =
    where bibM1 = M.fromList [(project $ findTitle e, e) | e <- bib1]
-}


-----------------------------------------------------------------------
-- Harvest Downloads

harvest :: InitFile -> [Entry] -> MaybeIO ()
harvest cfg bib = do
  contents <- getDirectoryContents' (downloadsDirectory cfg)
  let oldFiles = concatMap (map snd . files) bib
      newFiles = contents \\ oldFiles
      papers = [Entry {kind = "download",
                       seeAlso = [],
                       authors = [],
                       files = [(fname,guessTypeByName fname)],
                       otherFields = [("title",fname),("date","2010")]
                      } | fname <- newFiles, takeExtension fname `elem` [".pdf",".ps"]]
  saveBib cfg $ papers ++ bib


------------------------------------------------------------------------
-- Driver

sortBib :: InitFile -> [Entry] -> MaybeIO ()
sortBib cfg b = saveBib cfg (sortBy (compare `on` \x -> (findFirstAuthor x,findYear x,findTitle x)) b)

saveBib :: InitFile -> [Entry] -> MaybeIO ()
saveBib cfg b = safely "Saving bibfile" $ saveBibliography cfg b

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: InitFile -> [Entry] -> Parser (MaybeIO ())
parseCommand cfg bib = subparser $
    command "check"   ((pure (checkAttachments cfg bib)                     `withInfo` "check that attachment exist")) <>
    command "rename"  ((pure (renameAttachments cfg bib)                    `withInfo` "rename/move atachments to where they belong")) <>
    command "import"  ((mergeIn cfg bib <$> (argument str (metavar "FILE")) `withInfo` "merge a bibfile into the database")) <>
    command "harvest" ((pure (harvest cfg bib)                              `withInfo` "harvest attachments (???)")) <>
    command "dup"     ((pure (checkDup cfg bib)                             `withInfo` "check for duplicates")) <>
    command "cleanup" ((pure (saveBib cfg bib)                              `withInfo` "cleanup keys etc.")) <>
    command "sort"    ((pure (sortBib cfg bib)                              `withInfo` "sort entries by key"))

main          :: IO ()
main = do
  cfg <- loadConfiguration
  bib <- rightOrDie =<< loadBibliography cfg
  let options :: ParserInfo (Bool, MaybeIO ())
      options =
        (((,) <$> 
           switch (short 'd' <> long "dry-run" <> help "don't perform any change (dry run)") <*>
           parseCommand cfg bib
         ) `withInfo` "batch handling of bib db")
  (dry,cmd) <- execParser options
  run dry cmd

dryRun :: MaybeIO a -> IO a
dryRun = run True

trueRun :: MaybeIO a -> IO a
trueRun = run False


-- Local Variables:
-- dante-target: "imbibatch"
-- End:
