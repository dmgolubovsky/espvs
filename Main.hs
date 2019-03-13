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
  mainWithGUI vpath'

data MVCE = MVCE FilePath

instance Argument MVCE where
  argumentType Proxy = "path-to-the-voice-file"
  parseArgument f = Just (MVCE f)

instance HasArguments MVCE where
  argumentsParser = atomicArgumentsParser

data Options = Options {
}  deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
       ]


