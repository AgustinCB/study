{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import System.Environment

import Log

parseInt :: String -> Int
parseInt s = (read s)::Int

isValid :: [String] -> Bool
isValid l
  | length l < 3 || length l > 4 = False
  | not (any (\level -> level == (head l)) ["W", "I", "E"]) = False
  | (head l) == "E" && length l == 3 = False
  | otherwise = True

getMessage :: [String] -> LogMessage
getMessage l
  | not(isValid l) = Unknown(unwords l)
  | otherwise = case (head l) of
      "E" -> (LogMessage (Error (parseInt (l !! 1))) (parseInt (l !! 2)) (l !! 3))
      "W" -> (LogMessage Warning (parseInt (l !! 1)) (l !! 2))
      "I" -> (LogMessage Info (parseInt (l !! 1)) (l !! 2))
      _ -> Unknown("Never happens")

parseMessage :: String -> LogMessage
parseMessage s = getMessage (words s)

main :: IO () 
main = do x <- getArgs;
          print (parseMessage (head x))
