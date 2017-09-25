module Cipher(
  caesar,
  unCaesar) where

import Data.Char

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
    shiftWithBase :: Char -> Integer -> Integer -> Char
    shiftWithBase c n base = integerChr $ (mod (integerOrd c - base + n) 26) + base

caesar :: Integer -> String -> String
caesar n = map $ shift n

unCaesar :: Integer -> String -> String
unCaesar n = map $ shift (-n)
