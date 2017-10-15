module Phone(
  reverseTaps,
  cellPhonesDead) where

import Data.Char
import Data.Function (on)
import Data.List
import Data.Map
import Data.Maybe

data Key = Key Char [Char]
newtype DaPhone = DaPhone [Key]

type Digit = Char
type Presses = Int

findLetter :: DaPhone -> Char -> Key
findLetter (DaPhone keys) c = fromJust $ find (hasChar c) keys
  where
    hasChar :: Char -> Key -> Bool
    hasChar c (Key _ cs) = elem c cs

charToTaps :: Key -> Char -> (Digit, Presses)
charToTaps key@(Key n cs) c = (n, 1 + (fromJust $ elemIndex c cs))

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c
  | isUpper c = [charToTaps (findLetter phone '^') '^', charToTaps (findLetter phone $ toLower c) c]
  | otherwise = [charToTaps (findLetter phone c) c]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = concat $ fmap (reverseTaps phone) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps ts = sum $ fmap snd ts

weights :: Ord a => [a] -> [(a, Int)]
weights l = toList $ fromListWith (+) [(c, 1) | c <- l]

mostPopular :: Ord a => [a] -> a
mostPopular s = fst $ maximumBy (compare `on` snd) (weights s)

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular

coolestLtr :: [String] -> Char
coolestLtr words = mostPopularLetter $ concat words

coolestWord :: [String] -> String
coolestWord = mostPopular
