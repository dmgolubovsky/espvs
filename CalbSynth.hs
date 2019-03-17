{-# LANGUAGE ScopedTypeVariables #-}
module CalbSynth where

import System.IO
import System.Exit
import System.Process
import System.FilePath
import System.Environment
import Control.Concurrent.Thread
import qualified System.IO.Strict as SIO

genVocal :: String -> String -> String -> FilePath -> String -> String 
         -> IO (Either String FilePath)

genVocal execp synthp voice score part detune = do
  let synth = proc execp [ "-v", voice,
                           "-p", part,
                           "-D", detune,
                           "-S",
                           score
                         ]
  (_, mbhout, mbherr, p) <- createProcess synth {std_out = CreatePipe, std_err = CreatePipe}
  (t, x) <- forkIO $ do
    case mbherr of
      Nothing -> return ""
      Just herr -> SIO.hGetContents herr
  e <- result =<< x
  case mbhout of
    Nothing -> return $ Left "no output"
    Just hout -> do
      s <- SIO.hGetContents hout
      c <- waitForProcess p
      case c of
        ExitSuccess -> do
          let s' = head $ lines $ s ++ "\n\n"
          return (Right $ s' `addExtension` "wav")
        _ -> do
          let e' = head $ lines $ e ++ "\n\n"
          return (Left e')
 
calbSynth :: String -> String -> FilePath -> String -> IO String

calbSynth execp synthp voice ctxt = do
  let synth = proc execp ["-v", voice, "-C", "-c", ctxt, "-x", synthp, "/dev/null"] 
  (_, mbhout, _, p) <- createProcess synth {std_out = CreatePipe}
  case mbhout of
    Nothing -> return "no output"
    Just hout -> do
      s <- SIO.hGetContents hout
      waitForProcess p
      let s' = head $ lines $ s ++ "\n\n"
      return s'
 
scoreParts :: String -> FilePath -> IO [String]

scoreParts execp scpath = do
  let synth = proc execp ["-L", scpath]
  (_, mbhout, _, p) <- createProcess synth {std_out = CreatePipe}
  case mbhout of
    Nothing -> return ["score has no parts"]
    Just hout -> do
      s <- SIO.hGetContents hout
      waitForProcess p
      return $ lines s

trackLength :: FilePath -> IO Double

trackLength fp = do
  let soxi = proc "soxi" ["-D", fp]
  (_, mbhout, _, p) <- createProcess soxi {std_out = CreatePipe}
  case mbhout of
    Nothing -> return (-1.0)
    Just hout -> do
      s <- SIO.hGetContents hout
      waitForProcess p
      let s' = head $ lines $ s ++ "\n\n"
      let d = read s'
      return d

