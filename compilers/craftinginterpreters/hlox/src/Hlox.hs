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
data Token a = Token { tokenType :: TokenType a, lexeme :: String, tokenLocation :: SourceCodeLocation }

type TokenResult a = (Token a, String, Int)

createToken :: (Num a) => Char -> String -> Int -> TokenResult a
createToken nextChar rest line
  | nextChar == '('   = (Token LeftParen (nextChar : []) (SourceCodeLocation Nothing line), rest, line)
  | nextChar == '\n'  = createToken rest (line + 1)

scanToken :: (Num a) => String -> Int -> TokenResult a
scanToken s l = createToken (head s) (tail s) l

scanTokens :: (Num a) => String -> [Token a]
scanTokens s = scanTokens' s 1
  where
    scanTokens' :: (Num a) => String -> Int -> [Token a]
    scanTokens' "" _ = []
    scanTokens' "" l =
      let (token, rest, line) = scanToken s l
      in token : scanTokens' rest line
