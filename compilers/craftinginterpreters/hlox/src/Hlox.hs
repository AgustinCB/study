module Hlox(SourceCodeLocation) where

data SourceCodeLocation = SourceCodeLocation { file :: String, line :: Int }
