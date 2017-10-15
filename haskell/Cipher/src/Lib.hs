module Cipher(
  caesar,
  unCaesar,
  vigenere) where

import Data.Char
import Text.Read

integerOrd = toInteger.ord
integerChr = chr.fromIntegral

lowerBase = integerOrd 'a'
upperBase = integerOrd 'A'

shift :: Integer -> Char -> Char
shift n c
  | isLower c = shiftWithBase c n lowerBase
  | isUpper c = shiftWithBase c n upperBase
  | otherwise = c
  where
    shiftWithBase c n base = integerChr $ (mod (integerOrd c - base + n) 26) + base

caesar :: Integer -> String -> String
caesar n = map $ shift n

unCaesar :: Integer -> String -> String
unCaesar n = map $ shift (-n)

vigenere :: String -> String -> String
vigenere key "" = ""
vigenere key phrase = go key phrase 0
  where
    go _ "" _ = ""
    go key (c:cs) step
      | isLower c || isUpper c = shift (getShiftFromChar key step) c : go key cs (step + 1)
      | otherwise = c : go key cs step
    getShiftFromChar key step = getShift $ getChar key step
    getShift c
      | isLower c = (integerOrd c) - lowerBase
      | isUpper c = (integerOrd c) - upperBase
      | otherwise = 0
    getChar key step = key !! ((fromInteger step) `mod` (length key))

doCaesar :: IO()
doCaesar = do
  putStrLn "Introduce the phrase to encrypt"
  phrase <- getLine
  putStrLn "Introduce the number of steps to shift"
  stepsLine <- getLine
  case readMaybe stepsLine of
    Nothing -> putStrLn "Invalid number!"
    Just steps -> putStrLn ("Result: " ++ caesar steps phrase)

doVigenere :: IO()
doVigenere = do
  putStrLn "Introduce the phrase to encrypt"
  phrase <- getLine
  putStrLn "Introduce the key"
  key <- getLine
  putStrLn ("Result: " ++ vigenere key phrase)
