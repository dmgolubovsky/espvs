module VoiceFile where

import System.IO
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

data VoiceLine = 
   Comment String
 | VoiceName String
 | VoiceLang String
 | Pitch Int Int
 | Formant Int Int Int Int Int
 | Tone (Int, Int) (Int, Int) (Int, Int) (Int, Int)
 | Flutter Int
 | Roughness Int
 | Voicing Int
 | Consonants Int Int
 | Breath [Int]
 | Breathw [Int]
 | Speed Int
 | StressLength [Int]
 | StressAdd [Int]
 | StressAmp [Int]
 | Intonation Int
 | Other String
   deriving (Show)

wrtl s ss = s ++ " " ++ intercalate " " (map show ss)

wrt :: VoiceLine -> String

wrt (VoiceName s) = "name " ++ s
wrt (VoiceLang s) = "language " ++ s
wrt (Pitch p x) = "pitch " ++ show p ++ " " ++ show p
wrt (Formant n f s w a) = wrtl "formant" [n, f, s, w, a]
wrt (Tone (f1, a1) (f2, a2) (f3, a3) (f4, a4)) = wrtl "tone" [f1, a1, f2, a2, f3, a3, f4, a4]
wrt (Flutter f) = "flutter " ++ show f
wrt (Speed s) = "speed " ++ show s
wrt (Intonation i) = "intonation " ++ show i
wrt (Roughness r) = "roughness " ++ show r
wrt (Voicing v) = "voicing " ++ show v
wrt (Consonants v u) = "consonants " ++ show v ++ " " ++ show u
wrt (Breath bs) = wrtl "breath" bs
wrt (Breathw bws) = wrtl "breatw" bws
wrt (StressLength sls) = wrtl "stressLength" sls
wrt (StressAdd sas) = wrtl "stressAdd" sas
wrt (StressAmp sas) = wrtl "stressAmp" sas
wrt (Other s) = s
wrt x = error $ "wrt not impl for " ++ show x


type VoiceFile = [VoiceLine]

parseLine :: String -> VoiceLine

parseLine l = case (parse vl "" l) of
  Right p -> p
  Left _ -> Other l
  where
    oneparm n f c = try $ do
                          string n
                          spaces
                          many anyChar >>= return . c . f
    twoparm n c = try $ do
                          string n
                          spaces
                          p1 <- many1 digit
                          spaces
                          p2 <- many1 digit
                          return $ c (read p1) (read p2)
    listparm n c k d = try $ do
                          string n
                          spaces
                          ds <- (many1 digit) `endBy` spaces
                          let l = take k $ (map read ds) ++ repeat d
                          return $ c l
    vl = oneparm "name" id VoiceName
         <|>
         oneparm "language" id VoiceLang
         <|>
         oneparm "flutter" read Flutter
         <|>
         oneparm "roughness" read Roughness
         <|>
         oneparm "voicing" read Voicing
         <|>
         oneparm "intonation" read Intonation
         <|>
         oneparm "speed" read Speed
         <|> 
         twoparm "pitch" Pitch
         <|>
         twoparm "consonants" Consonants
         <|>
         (try $ do
                  string "formant"
                  spaces
                  ds <- (many1 digit) `endBy1` spaces
                  let [n, f, s, w, a] = take 5 $ (map read ds) ++ repeat 0
                  return $ Formant n f s w a
         )
         <|>
         listparm "breath" Breath 8 0
         <|> 
         listparm "breathw" Breathw 8 0
         <|>
         listparm "stressadd" StressAdd 8 0
         <|>
         listparm "stressamp" StressAmp 8 0
         <|>
         listparm "stresslength" StressLength 8 0
         <|>
         (try $ do
                  (Breath [f1, a1, f2, a2, f3, a3, f4, a4]) <- listparm "tone" Breath 8 0
                  return $ Tone (f1, a1) (f2, a2) (f3, a3) (f4, a4)
         )

parseVoiceFile :: String -> IO VoiceFile

parseVoiceFile path = do
  h <- openFile path ReadMode
  vce <- hGetContents h
  let lvce = lines $ map toLower vce
  let prss = map parseLine lvce
  return prss


