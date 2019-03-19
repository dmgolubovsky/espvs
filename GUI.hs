{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO
import Data.List
import Data.Text (pack, unpack)
import Data.Maybe
import System.Directory

import WithCli

import VoiceFile
import CalbSynth

data Options = Options {
  score :: Maybe FilePath
 ,backing :: Maybe FilePath
}  deriving (Show, Generic, HasArguments)

setElem :: VoiceLine -> [Builder -> IO String]

setElem (VoiceName vn) = [setLabel "VoiceName" vn]
setElem (VoiceLang ln) = [setLabel "VoiceLang" ln]
setElem (Pitch p _) = [setSpinBtn "PitchSet" p]
setElem (Speed f) = [setSpinBtn "Speed" f]
setElem (Flutter f) = [setSpinBtn "Flutter" f]
setElem (Roughness r) = [setSpinBtn "Roughness" r]
setElem (Intonation i) = [setSpinBtn "Intonation" i]
setElem (Voicing v) = [setSpinBtn "Voicing" v]
setElem (Consonants u v) = zipWith setSpinBtn ["Consu", "Consv"] [u, v]
setElem (Tone p1 p2 p3 p4) = concat $ zipWith x [p1, p2, p3, p4] [1..] where
  x (f, a) n = zipWith setSpinBtn (map (++ show n) ["Freq", "Amp"]) [f, a] 
setElem (Breath bs) = setList "Breath" bs
setElem (Breathw bs) = setList "Breathw" bs
setElem (StressLength bs) = setList "Stresslg" bs
setElem (StressAdd bs) = setList "Stressadd" bs
setElem (StressAmp bs) = setList "Stressamp" bs
setElem (Formant n f s w a) = zipWith setSpinBtn flblst [n, f, s, w, a] where
  flblst = map (++ show n) ["FN", "Frq", "Str", "Wid", "Frqad"]
setElem _ = [const $ return ""]

setList stem vs = zipWith setSpinBtn lbls vs where
  lbls = map ((stem ++) . show) [1..]

ctch x = x `catch` (\(e :: ErrorCall) -> putStrLn (show e) >> return "")

setLabel :: String -> String -> Builder -> IO String

setLabel lid str bld = ctch $
  builderGetObject bld castToLabel lid >>= flip labelSetText str >> return ""

getLabel :: String -> Builder -> IO String

getLabel lid bld = ctch $
  builderGetObject bld castToLabel lid >>= labelGetText

setSpinBtn :: String -> Int -> Builder -> IO String

setSpinBtn bid val bld = ctch $ do
  btn <- builderGetObject bld castToSpinButton bid 
  adj <- adjustmentNew (fromIntegral val) (-1.0e6) 1.0e6 1.0 1.0 1000.0
  spinButtonConfigure btn adj 1.0 0
  return bid

getCBox :: String -> Builder -> IO String

getCBox cbid bld = ctch $ do
  cbx <- builderGetObject bld castToComboBox cbid
  mbtx <- comboBoxGetActiveText cbx
  return $ unpack $ fromMaybe (pack "") mbtx

putElem :: VoiceLine -> (Handle -> Builder -> IO ())

putElem vl@(Flutter f) = readSpinBtn vl ["Flutter"]
putElem vl@(Speed f) = readSpinBtn vl ["Speed"]
putElem vl@(Roughness f) = readSpinBtn vl ["Roughness"]
putElem vl@(Intonation f) = readSpinBtn vl ["Intonation"]
putElem vl@(Voicing f) = readSpinBtn vl ["Voicing"]
putElem vl@(Pitch p x) = readSpinBtn vl ["PitchSet", "PitchSet"]
putElem vl@(Consonants u v) = readSpinBtn vl ["Consu", "Consv"]
putElem vl@(Breath bs) = readSpinBtn vl $ map (("Breath" ++) . show) [1 .. length bs]
putElem vl@(Breathw bs) = readSpinBtn vl $ map (("Breathw" ++) . show) [1 .. length bs]
putElem vl@(StressLength bs) = readSpinBtn vl $ map (("Stresslg" ++) . show) [1 .. length bs]
putElem vl@(StressAdd bs) = readSpinBtn vl $ map (("Stressadd" ++) . show) [1 .. length bs]
putElem vl@(StressAmp bs) = readSpinBtn vl $ map (("Stressamp" ++) . show) [1 .. length bs]
putElem vl@(Formant n f s w a) = readSpinBtn vl $ 
  map (++ show n) ["FN", "Frq", "Str", "Wid", "Frqad"]
putElem vl@(Tone (f1, a1) (f2, a2) (f3, a3) (f4, a4)) = readSpinBtn vl lst where
  cp = [(n, s) | n <- [1 .. 4], s <- ["Freq", "Amp"]]
  lst = map (uncurry (flip (++) . show)) cp

putElem vl = putElemDef vl

putElemDef vl = \h b -> hPutStrLn h (wrt vl) >> hFlush h

readSpinBtn :: VoiceLine -> [String] -> Handle -> Builder -> IO ()

readSpinBtn vl bids h builder = do
  vals <- forM bids $ \bid -> ctch $ do
    ed <- builderGetObject builder castToEditable bid
    editableGetChars ed 0 (-1)
  case filter ((> 0) . length) vals of
    [] -> putElemDef vl h builder
    vls -> do
      let kw = head $ (words $ wrt vl) ++ ["//"]
      hPutStrLn h $ kw ++ " " ++ intercalate " " vls

getSpinBtn :: String -> Builder -> IO String

getSpinBtn bid bld = ctch $ do
  ed <- builderGetObject bld castToEditable bid
  editableGetChars ed 0 (-1)
  

apm :: [(a -> b)] -> a -> [b]

apm [] _ = []
apm (f:fs) a = (f a):(apm fs a)

mainWithGUI fp opts = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "espvs.ui"
  wmain <- builderGetObject builder castToWindow "window1"
  on wmain objectDestroy mainQuit
  after wmain realize $ loadVoice fp builder
  let fnl = map (("FN" ++) . show) [0 .. 8]
  widgetShowAll wmain
  forM fnl $ (widgetHide =<<) . builderGetObject builder castToWidget
  if isJust $ score opts
    then do
      let scorepath = fromJust $ score opts
      setLabel "ScorePath" scorepath builder
      pcbx <- builderGetObject builder castToComboBox "ScorePart"
      comboBoxSetModelText pcbx
      parts <- scoreParts "lyrvoc" scorepath
      mapM ((comboBoxAppendText pcbx) . pack) parts
      comboBoxSetActive pcbx 0
      setSpinBtn "Detune" 0 builder
      setPlay fp builder
      return ()
    else return ()
  if isJust $ backing opts
    then do
      let backpath = fromJust $ backing opts
      e <- doesFileExist backpath
      if e
        then do
          setLabel "BackingPath" backpath builder
          bl <- trackLength backpath >>= return . round
          setSpinBtn "BackLength" bl builder
          return ()
        else return ()
    else return ()
  setIdle builder
  mainGUI

setIdle :: Builder -> IO ()

setIdle builder = do
  setLabel "Status" "Idle" builder
  return ()

setBusy :: Builder -> IO () -> IO ()

setBusy builder act = do
  st <- builderGetObject builder castToLabel "Status"
  txt <- labelGetText st
  case txt of
    "Busy" -> return ()
    "Idle" -> labelSetText st "Busy" >> act
    _ -> return ()

loadVoice :: FilePath -> Builder -> IO ()

loadVoice fp builder = do
  vf <- parseVoiceFile fp
  lbls <- (sequence $ apm (setElem =<< vf) builder) >>= return . filter ((> 0) . length)
  es <- mapM (builderGetObject builder castToEditable) lbls
  forM es $ \e -> after e editableChanged (dumpVoice e fp vf builder)
  forkIO $ do
    threadDelay 1000
    postGUISync $ setIdle builder
  return ()

dumpVoice :: Editable -> FilePath -> VoiceFile -> Builder -> IO ()

dumpVoice ed fp vf builder = setBusy builder $ do
  h <- openFile fp WriteMode
  sequence $ apm (map (uncurry . putElem) vf) (h, builder)
  hClose h
  loadVoice fp builder
  return ()

setPlay :: FilePath -> Builder -> IO ()

setPlay fp builder = do
  [play, stop, loop] <- mapM (builderGetObject builder castToButton) ["Play", "Stop", "Loop"]
  widgetSetSensitive (castToWidget stop) False
  after play buttonActivated $ doplay False
  after loop buttonActivated $ doplay True
  return ()
  where 
    doplay rpt = do
      [play, stop, loop] <- mapM (builderGetObject builder castToButton) ["Play", "Stop", "Loop"]
      sclb <- getLabel "ScorePath" builder
      scpt <- getCBox "ScorePart" builder
      detu <- getSpinBtn "Detune" builder
      cfrq <- getSpinBtn "PitchSet" builder
      trim <- getSpinBtn "BackLength" builder
      setLabel "VocalPath" "..." builder
      r <- genVocal "lyrvoc" "espeak-ng" fp sclb scpt detu (Just cfrq)
      case r of
        Left err -> do
          setLabel "VocalPath" ("Error: " ++ err) builder
          return ()
        Right wfp -> do
          setLabel "VocalPath" wfp builder
          widgetSetSensitive (castToWidget play) False
          widgetSetSensitive (castToWidget loop) False
          bfp <- getLabel "BackingPath" builder
          mp <- mixVoc wfp bfp (Just trim)
          widgetSetSensitive (castToWidget stop) True
          after stop buttonActivated $ stopVoc mp
          forkIO $ do 
            b <- waitVoc mp (progress builder)
            postGUIAsync $ do
              after stop buttonActivated $ return ()
              widgetSetSensitive (castToWidget stop) False
              if (b && rpt)
                then doplay rpt
                else do
                       widgetSetSensitive (castToWidget play) True
                       widgetSetSensitive (castToWidget loop) True
                       return ()
            return ()
          return ()
      return ()

progress :: Builder -> String -> IO ()

progress str bld = return ()

