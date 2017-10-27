module Cipher(
  caesar,
  unCaesar,
  vigenere,
  doCaesar,
  doVigenere) where

import Data.Char
import Text.Read(readMaybe)

integerOrd :: Char -> Integer
integerOrd = toInteger.ord
integerChr :: Integer -> Char
integerChr = chr.fromIntegral

lowerBase :: Integer
lowerBase = integerOrd 'a'
upperBase :: Integer
upperBase = integerOrd 'A'

shift :: Integer -> Char -> Char
shift n c
  | isLower c = shiftWithBase lowerBase
  | isUpper c = shiftWithBase upperBase
  | otherwise = c
  where
    shiftWithBase base = integerChr $ (mod (integerOrd c - base + n) 26) + base

caesar :: Integer -> String -> String
caesar n = map $ shift n

unCaesar :: Integer -> String -> String
unCaesar n = map $ shift (-n)

vigenere :: String -> String -> String
vigenere _ "" = ""
vigenere key phrase = go phrase 0
  where
    go "" _ = ""
    go (c:cs) step
      | isLower c || isUpper c = shift (getShiftFromChar step) c : go cs (step + 1)
      | otherwise = c : go cs step
    getShiftFromChar = getShift.getCharFromShift
    getShift c
      | isLower c = (integerOrd c) - lowerBase
      | isUpper c = (integerOrd c) - upperBase
      | otherwise = 0
    getCharFromShift step = key !! ((fromInteger step) `mod` (length key))

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
