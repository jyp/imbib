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
-- import Text.Groom
import MaybeIO

-------------------------------------------------------------------------
-- CheckDuplicates, method 1


pairs :: [t] -> [(t, t)]
pairs (x:xs) = map (x,) xs ++ pairs xs
pairs _ = []

checkDup :: InitFile -> [Entry] -> MaybeIO ()
checkDup cfg bib = do
  -- mapM_ putString $  [ groom $ (map findTitle es, shared) | (es,shared) <- common ]
  putString "Possible duplicates:"
  mapM_ putString $ map (show . map findTitle) dups
  saveBib cfg $ uniqDups ++ (bib \\ uniqDups) -- clump together the duplicates
  where uniqDups = nub $ concat $ dups
        trueDups es = -- filter (\e -> any (not . (areRelated e)) es)
                      es
        dups = [es | (es0,shared) <- common,
                let es = trueDups es0,
                not (null es),
                sum (map length shared) >= threshold (maximum (map (length . project . findTitle) es))]
        threshold x = (8 * x) `div` 10
        -- SC.printTree $ SC.select ("", clusterTree)
        common = M.toList $ SC.commonStrings info
        -- clusterTree = SC.construct info
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

mergeIn :: InitFile -> [Entry] -> String -> MaybeIO ()
mergeIn cfg bib fname = do
  bib2 <- uncheckedHarmless $ rightOrDie <$> loadBibliographyFrom fname
  checkDup cfg $ bib2 ++ bib
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

saveBib :: InitFile -> [Entry] -> MaybeIO ()
saveBib cfg b = safely "Saving bibfile" $ saveBibliography cfg b

main :: IO ()
main = do
  cfg <- loadConfiguration
  bib <- rightOrDie <$> loadBibliography cfg
  let options :: ParserInfo (Bool, MaybeIO ())
      options =
        info ((,) <$>
               switch (short 'd' <> long "dry-run" <> help "don't perform any change (dry run)") <*>
               subparser (command "check" (info (pure (checkAttachments cfg bib)) (progDesc "check that attachment exist")) <>
                          command "rename" (info (pure (renameAttachments cfg bib)) (progDesc "rename/move atachments to where they belong")) <>
                          command "merge" (info (mergeIn cfg bib <$> (argument str (metavar "FILE"))) (progDesc "merge a bibfile into the database")) <>
                          command "harvest" (info (pure (harvest cfg bib)) (progDesc "harvest attachments (???)")) <>
                          command "dup" (info (pure (checkDup cfg bib)) (progDesc "check for duplicates")) <>
                          command "cleanup" (info (pure (saveBib cfg bib)) (progDesc "cleanup keys etc."))
                        )) (fullDesc <> progDesc "batch handling of bib db")
  (dry,cmd) <- execParser options
  run dry cmd

dryRun :: MaybeIO a -> IO a
dryRun = run True

trueRun :: MaybeIO a -> IO a
trueRun = run False


-- Local Variables:
-- dante-target: "imbibatch"
-- End:
