{-# LANGUAGE ScopedTypeVariables #-}
module Hlox where

-- Language specs

data TokenType a =
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
  While |
  Identifier String |
  StringLiteral String |
  Number a
data Token a = Token { tokenType :: TokenType a, lexeme :: String, tokenLocation :: SourceCodeLocation } deriving Show

type TokenResult a = (Token a, String, Int)

-- Program results

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int }
data ParseOutcome = Error { location :: SourceCodeLocation, msg :: String } | Success [Token]

instance Show SourceCodeLocation where
  show (SourceCodeLocation (Just file) line) = "[line " ++ (show line) ++ "] Error in " ++ file ++ ": "
  show (SourceCodeLocation Nothing line) = "[line " ++ (show line) ++ "] Error: "

instance Show ParseOutcome where
  show (Error location msg) = (show location) ++ msg
  show (Success []) = ""
  show (Success t:rest) = (show Token) ++ " " ++ (show (Success rest))

createToken :: (Num a) => Char -> String -> Int -> TokenResult a
createToken nextChar rest line
  | nextChar == '('   = oneCharToken LeftParen
  | nextChar == ')'   = oneCharToken RightParen
  | nextChar == '{'   = oneCharToken LeftBrace
  | nextChar == '}'   = oneCharToken RightBrace
  | nextChar == ','   = oneCharToken Comma
  | nextChar == '.'   = oneCharToken Dot
  | nextChar == '-'   = oneCharToken Minus
  | nextChar == '+'   = oneCharToken Plus
  | nextChar == ';'   = oneCharToken Semicolon
  | nextChar == '*'   = oneCharToken Star
  | nextChar == '\n'  = createToken (head rest) (tail rest) (line + 1)
  where
    oneCharToken :: TokenType a -> TokenResult a
    oneCharToken t = (Token t (nextChar : []) (SourceCodeLocation Nothing line), rest, line)

scanToken :: (Num a) => String -> Int -> TokenResult a
scanToken s l = createToken (head s) (tail s) l

scanTokens :: (Num a) => String -> [Token a]
scanTokens s = scanTokens' s 1
  where
    scanTokens' :: (Num a) => String -> Int -> [Token a]
    scanTokens' "" _ = []
    scanTokens' s l =
      let (token, rest, line) = scanToken s l
      in token : scanTokens' rest line
