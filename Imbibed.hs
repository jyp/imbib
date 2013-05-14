{-# LANGUAGE RecordWildCards, TupleSections  #-}

{--
TODO:
if URI is bib, use as such (should be done, sometimes is problematic.)
Search google on title ?
-}

import Control.Applicative hiding ((<|>),many)
import Control.Exception (catch)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T
import Data.Char
import Data.List
import Data.Maybe
import Graphics.UI.Gtk hiding (on, Entry)
import qualified Graphics.UI.Gtk as Gtk 
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.ModelView.ListStore
import Network.Curl.Download.Lazy
import Network.Curl.Opts
import Data.Function
-- import System.Gnome.VFS.Monitor
-- import qualified System.Gnome.VFS.Init as VFS
import System.GIO.File.File
import System.GIO.File.FileMonitor

import System.Glib.GError
import Text.ParserCombinators.Parsec (parse)
-- import System.INotify -- hnotify

import TypedBibData
import BibDB
import BibAttach
import Config

----------------------
-- GUI

main = do
  bib0 <- rightOrDie <$> loadBibliography
 
  -- notify <- initINotify
  initGUI

  win <- windowNew

  model <- listStoreNewDND bib0 
             (Just (DragSourceIface draggable dataGet dataDelete)) 
             (Just (DragDestIface possible recieved))
  onDestroy win $ do
         saveStore model 
         -- killINotify notify
         mainQuit
  
  {-
  -- Between Dec 2012 and May 2013, using hinotify provokes a segmentation fault:
  addWatch notify [Modify] bibfile $ \e -> do
         putStrLn $ "Bibfile changed; reloading"
         mBib <- loadBibliography
         case mBib of
           Left err -> putStr $ show err
           Right bib -> do oldBib <- listStoreToList model
                           when (bib /= oldBib) $ do
                             listStoreClear model
                             forM_ bib (listStoreAppend model)
  -}
  
  let gioBibfile = fileFromURI ("file://" ++ bibfile)
      ex = fileQueryExists gioBibfile Nothing
  putStrLn $ "Bibfile exists? " ++ show ex
  monitor <- fileMonitor gioBibfile [] Nothing
  

  Gtk.on monitor fileMonitorChanged $ \childFile otherFile evType -> do
         -- This may not work. See bug: http://hackage.haskell.org/trac/gtk2hs/ticket/1221
         -- (bug report copied at the end of this file)
         putStrLn $ "Bibfile changed; reloading " ++ show evType
         mBib <- loadBibliography
         case mBib of
           Left err -> putStr $ show err
           Right bib -> do oldBib <- listStoreToList model
                           when (bib /= oldBib) $ do
                             listStoreClear model
                             forM_ bib (listStoreAppend model)

  searchBox <- entryNew

  filt <- treeModelFilterNew model []
  treeModelFilterSetVisibleFunc filt $ \iter -> do
    needle <- get searchBox entryText
    let idx = listStoreIterToIndex iter
    e <- listStoreGetValue model idx
    return $ e `matchSearch` needle


 
  onEditableChanged searchBox $ treeModelFilterRefilter filt

  view <- New.treeViewNewWithModel filt

  New.treeViewSetHeadersVisible view True

  -- columns
  [col0,col1,col2] <- forM ["⎙","Cit","Title"] $ \h -> do 
     c <- New.treeViewColumnNew
     New.treeViewColumnSetTitle c h
     return c
       
  treeViewColumnSetResizable col1 True
  treeViewColumnSetResizable col2 True

  [renderer0,renderer1,renderer2] <- forM [0..2] $ \_ -> New.cellRendererTextNew

  New.cellLayoutPackStart col0 renderer0 True
  New.cellLayoutPackStart col1 renderer1 True
  New.cellLayoutPackStart col2 renderer2 True

  Gtk.set col1 [treeViewColumnMinWidth := 180]

  forM [renderer1,renderer2] $ \r -> set r [ cellTextEllipsizeSet := True , cellTextEllipsize := EllipsizeEnd ]

  New.cellLayoutSetAttributes col0 renderer0 model $ \row -> [ New.cellText := if null $ findFullText row then "" else "✓"]
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := findCite row ]
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := renderTex $ findTitle row  ]

  {-
  Gtk.on renderer2 edited $ \path@(i:_) newText -> do
         (k,v) <- listStoreGetValue model path
         listStoreSetValue model path (k,newText)
  -}

  New.treeViewAppendColumn view col0
  New.treeViewAppendColumn view col1
  New.treeViewAppendColumn view col2 
  
  -- enable interactive search
  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    p0 <- treeModelGetPath filt iter    
    [i] <- treeModelFilterConvertPathToChildPath filt p0
    entry <- listStoreGetValue model i
    return $ entry `matchSearch` str

  -- make the widget a drag source
  tl <- targetListNew
  targetListAddTextTargets tl 0
  treeViewEnableModelDragSource view [Button1] tl [ActionCopy]

  -- make the widget a drag destination
  tl <- targetListNew
  targetListAddUriTargets tl 0
  targetListAddTextTargets tl 1
  treeViewEnableModelDragDest view tl [ActionCopy]

  -- See the discussion on the gtk2hs-users list: 
  -- Drag and drop support in filtered TreeView
  -- (July 2010)
  Gtk.on view dragDrop $ \ context point time -> do
    putStrLn $ "dragDrop"
    signalStopEmission view "drag_drop"
    Just target <- dragDestFindTarget view context (Just tl)
    -- print target
    dragStatus context Nothing time 
    -- not so important as we do not need the data for motion: we never check this status.
    dragGetData view context target time
    return True

  Gtk.on view dragDataReceived $ \ context pt infoId time -> do
    liftIO $ putStrLn $ "dragData"
    liftIO $ signalStopEmission view "drag_data_received"
    pt' <- liftIO $ treeViewConvertWidgetToBinWindowCoords view pt
    mPath <- liftIO $ treeViewGetPathAtPos view pt'
    done <- case mPath of
      Nothing -> return False
      Just (p0,_,_) -> do
         p1 <- liftIO $ treeModelFilterConvertPathToChildPath filt p0
         recieved model p1 -- normally we'd call "possible" upon motion; but it's always possible.
    liftIO $ dragFinish context done False time

  let entryFromPath p0 = do
        [i] <- treeModelFilterConvertPathToChildPath filt p0
        listStoreGetValue model i
  
  -- Handle "return"
  afterRowActivated view $ \path col -> do
    openFullText =<< entryFromPath path
    
  -- Handle clicks
  Gtk.on view buttonPressEvent $ do 
    b <- eventButton
    c <- eventClick
    (x,y) <- eventCoordinates     
    let pt = (round x, round y)
    mPath <- liftIO $ treeViewGetPathAtPos view pt
    case mPath of
      Nothing -> return False
      Just (p0,_,_) -> liftIO $ do
         case (b,c) of
           (LeftButton,DoubleClick) -> do
              openFullText =<< entryFromPath p0
              return True
           (RightButton,DoubleClick) -> do
              -- Open emacs at the adequate line to edit.
              [i] <- treeModelFilterConvertPathToChildPath filt p0
              saveStore model
              before <- take i <$> listStoreToList model
              let lineNumber = length $ lines $ formatBib before
              runEditor lineNumber bibfile 
              return True
           _ -> return False

  layout <- vBoxNew False 0
  scr <- scrolledWindowNew Nothing Nothing         
  containerAdd scr view
  boxPackStart layout searchBox PackNatural 0 
  boxPackStart layout scr PackGrow 0
  containerAdd win layout

  catch (windowSetIconFromFile win iconFile) $ \e -> do
      putStrLn $ "Could not load icon file: " ++ show (e :: GError)
  windowSetDefaultSize win 640 360
  widgetShowAll win
  mainGUI 

openFullText :: Entry -> IO ()
openFullText t = do
  let file = findFullText t
  case file of 
    [] -> putStrLn "no pdf found!"
    (f:_) -> do putStrLn $ "Opening " ++ f
                runViewer f 
                return ()


draggable store [_] = return True
draggable store _ = return False

dataGet store [p] = do
  t <- liftIO $ listStoreGetValue store p
  let k = findNiceKey t
  selectionDataSetText $ "\\cite{"  ++ k ++ "}"

dataDelete _ _ = return True

receivedBib store text = do
  putStrLn $ "Got bib: "
  forM_ (lines text) putStrLn
  let mBib = parse parseBib "dropped text" text
  case mBib of
    Left err -> print err >> return False
    Right bib -> do print bib
                    mapM_ (listStorePrepend store) (bibToForest bib) 
                    -- saveStore store
                    return True

recievedFile i store uri = do
  putStrLn $ "Downloading " ++ uri
  mFile <- openLazyURIWithOpts [CurlFollowLocation True] uri
  case mFile of
    Left err -> putStrLn err >> return False
    Right file -> do 
      putStrLn "Success!"
      let fileType = guessType uri file
      if fileType == "bib" then receivedBib store (T.unpack $ T.decodeUtf8 file) else do
        entry <- listStoreGetValue store i
        putStrLn "Saving..."
        fname <- saveFile (findAttachName entry fileType) file
        putStrLn "Updating store"
        storeModify store i (addFile (fname, fileType))
        -- saveStore store
        return True

recieved store path@(i:_) = do
      liftIO $ putStrLn "recieved!"
      mURI <- selectionDataGetURIs
      case mURI :: Maybe [String] of
        -- Accept URI as download to link to the library
        -- Normally this stuff should be done in the background.
        -- I don't care.
        Just [uri] -> liftIO $ recievedFile i store uri
        _ -> do
          -- Accept drop of text as bibtex
          mText <- selectionDataGetText
          case mText :: Maybe String of
            Just text -> liftIO $ receivedBib store text
            _ -> return False

possible store path = do 
  liftIO $ putStrLn "Possible?"
  return True -- accept all text & URI, a priori.

--------------------------------------
-- Store

storeModify store i f = do
  a <- listStoreGetValue store i
  listStoreSetValue store i (f a)

saveStore store = do
  putStrLn "Syncing on disk"
  bib <- listStoreToList store
  saveBibliography bib


{-

In the following code, file monitoring stops working when the size of the list store is increased to a suitably big value.

import System.GIO.File.File
import System.GIO.File.FileMonitor
import qualified Graphics.UI.Gtk as Gtk


main = do
 Gtk.initGUI
 let gfile = fileFromURI "file:///some file to monitor"
 monitor <- fileMonitorFile gfile [] Nothing
 Gtk.on monitor fileMonitorChanged $ \childFile otherFile evType -> do
        putStrLn $ "Changed: " ++ show evType

 model <- Gtk.listStoreNew [0..1000::Int]
 --- file monitoring works works with 100, fails with 1000
 filt <- Gtk.treeModelFilterNew model []
 view <- Gtk.treeViewNewWithModel filt

 Gtk.mainGUI

-}