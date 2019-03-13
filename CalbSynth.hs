{-# LANGUAGE ScopedTypeVariables #-}
module CalbSynth where

import System.IO
import System.Process
import System.Environment
import qualified System.IO.Strict as SIO

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
  
