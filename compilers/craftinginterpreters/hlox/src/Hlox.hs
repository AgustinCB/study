module Hlox where

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int }
data ProgramOutcome = Error { location :: SourceCodeLocation, msg :: String } | Success String

instance Show SourceCodeLocation where
  show (SourceCodeLocation (Just file) line) = "[line " ++ (show line) ++ "] Error in " ++ file ++ ": "
  show (SourceCodeLocation Nothing line) = "[line " ++ (show line) ++ "] Error: "

instance Show ProgramOutcome where
  show (Error location msg) = (show location) ++ msg
  show (Success result) = result
