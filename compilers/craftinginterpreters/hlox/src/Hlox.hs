module Hlox where

-- Program results

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int }
data ProgramOutcome = Error { location :: SourceCodeLocation, msg :: String } | Success String

instance Show SourceCodeLocation where
  show (SourceCodeLocation (Just file) line) = "[line " ++ (show line) ++ "] Error in " ++ file ++ ": "
  show (SourceCodeLocation Nothing line) = "[line " ++ (show line) ++ "] Error: "

instance Show ProgramOutcome where
  show (Error location msg) = (show location) ++ msg
  show (Success result) = result

-- Language specs

data TokenKeywordType =
  LeftParen |
  RightParen |
  LeftBrace |
  RightBrace |
  Comma |
  Dot |
  Minus |
  Plus |
  Semicolon |
  Slash |
  Star |
  Bang |
  BangEqual |
  Equal |
  EqualEqual |
  Grater |
  GreaterEqual |
  Less |
  LessEqual |
  And |
  Class |
  Else |
  FalseKeyword |
  Fun |
  For |
  If |
  Nil |
  Or |
  Print |
  Return |
  Super |
  This |
  TrueKeyword |
  Var |
  While
data TokenLiteralType a =
  Identifier String |
  StringLiteral String |
  Number a
data TokenType a = TokenKeywordType | TokenLiteralType a
data Token = Token { tokenType :: TokenType, lexeme :: String, location :: SourceCodeLocation }
