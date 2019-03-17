{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import WithCli

import VoiceFile
import GUI

main :: IO ()

main = withCliModified mods main'

main' :: MVCE -> Options -> IO ()

main' (MVCE vpath') opts = do
  mainWithGUI vpath' opts

data MVCE = MVCE FilePath

instance Argument MVCE where
  argumentType Proxy = "path-to-the-voice-file"
  parseArgument f = Just (MVCE f)

instance HasArguments MVCE where
  argumentsParser = atomicArgumentsParser

mods :: [Modifier]

mods = [
  AddShortOption "score" 's'
 ,AddOptionHelp  "score" "Set score from this musicxml file"
 ,AddShortOption "backing" 'b'
 ,AddOptionHelp  "backing" "Set path to the backing track"
       ]


