module Hlox(SourceCodeLocation, mkSourceCodeLocation) where

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int }

mkSourceCodeLocation :: Maybe String -> Int -> SourceCodeLocation
mkSourceCodeLocation s i = SourceCodeLocation s i

instance Show SourceCodeLocation where
  show (SourceCodeLocation (Just file) line) = "[line " ++ (show line) ++ "] Error in " ++ file ++ ": "
  show (SourceCodeLocation Nothing line) = "[line " ++ (show line) ++ "] Error: "
