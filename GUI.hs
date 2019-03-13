{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO
import Data.List

import VoiceFile
import CalbSynth

setElem :: VoiceLine -> [Builder -> IO String]

setElem (VoiceName vn) = [setLabel "VoiceName" vn]
setElem (VoiceLang ln) = [setLabel "VoiceLang" ln]
setElem (Pitch p _) = [setSpinBtn "PitchSet" p]
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

setSpinBtn :: String -> Int -> Builder -> IO String

setSpinBtn bid val bld = ctch $ do
  btn <- builderGetObject bld castToSpinButton bid 
  adj <- adjustmentNew (fromIntegral val) (-1.0e6) 1.0e6 1.0 1.0 1000.0
  spinButtonConfigure btn adj 1.0 0
  return bid

putElem :: VoiceLine -> (Handle -> Builder -> IO ())

putElem vl@(Flutter f) = getSpinBtn vl ["Flutter"]
putElem vl@(Roughness f) = getSpinBtn vl ["Roughness"]
putElem vl@(Intonation f) = getSpinBtn vl ["Intonation"]
putElem vl@(Voicing f) = getSpinBtn vl ["Voicing"]
putElem vl@(Pitch p x) = getSpinBtn vl ["PitchSet", "PitchSet"]
putElem vl@(Consonants u v) = getSpinBtn vl ["Consu", "Consv"]
putElem vl@(Breath bs) = getSpinBtn vl $ map (("Breath" ++) . show) [1 .. length bs]
putElem vl@(Breathw bs) = getSpinBtn vl $ map (("Breathw" ++) . show) [1 .. length bs]
putElem vl@(StressLength bs) = getSpinBtn vl $ map (("Stresslg" ++) . show) [1 .. length bs]
putElem vl@(StressAdd bs) = getSpinBtn vl $ map (("Stressadd" ++) . show) [1 .. length bs]
putElem vl@(StressAmp bs) = getSpinBtn vl $ map (("Stressamp" ++) . show) [1 .. length bs]
putElem vl@(Formant n f s w a) = getSpinBtn vl $ 
  map (++ show n) ["FN", "Frq", "Str", "Wid", "Frqad"]
putElem vl@(Tone (f1, a1) (f2, a2) (f3, a3) (f4, a4)) = getSpinBtn vl lst where
  cp = [(n, s) | n <- [1 .. 4], s <- ["Freq", "Amp"]]
  lst = map (uncurry (flip (++) . show)) cp

putElem vl = putElemDef vl

putElemDef vl = \h b -> hPutStrLn h (wrt vl) >> hFlush h

getSpinBtn :: VoiceLine -> [String] -> Handle -> Builder -> IO ()

getSpinBtn vl bids h builder = do
  vals <- forM bids $ \bid -> ctch $ do
    ed <- builderGetObject builder castToEditable bid
    editableGetChars ed 0 (-1)
  case filter ((> 0) . length) vals of
    [] -> putElemDef vl h builder
    vls -> do
      let kw = head $ (words $ wrt vl) ++ ["//"]
      hPutStrLn h $ kw ++ " " ++ intercalate " " vls
          
  
apm :: [(a -> b)] -> a -> [b]

apm [] _ = []
apm (f:fs) a = (f a):(apm fs a)

mainWithGUI fp = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "espvs.ui"
  wmain <- builderGetObject builder castToWindow "window1"
  on wmain objectDestroy mainQuit
  after wmain realize $ loadVoice fp builder
  let fnl = map (("FN" ++) . show) [0 .. 8]
  widgetShowAll wmain
  forM fnl $ (widgetHide =<<) . builderGetObject builder castToWidget
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
    frq <- calbSynth "lyrvoc" "espeak-ng" fp "ee"
    postGUISync $ setLabel "PitchMeasured" frq builder
    threadDelay 2000
    postGUISync $ setIdle builder
  return ()

dumpVoice :: Editable -> FilePath -> VoiceFile -> Builder -> IO ()

dumpVoice ed fp vf builder = setBusy builder $ do
  h <- openFile fp WriteMode
  sequence $ apm (map (uncurry . putElem) vf) (h, builder)
  hClose h
  loadVoice fp builder
  return ()

