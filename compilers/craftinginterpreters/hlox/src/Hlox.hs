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
  Number a deriving Show
data Token a = Token {
    tokenType :: TokenType a
  , lexeme :: String
  , tokenLocation :: SourceCodeLocation } deriving Show

type TokenResult a = (Token a, String, Int)

-- Program results

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int } deriving Show
data ParseOutcome a = Error { location :: SourceCodeLocation, msg :: String } | Success [Token a]


instance Show a => Show (ParseOutcome a) where
  show (Error (SourceCodeLocation (Just file) line)) = "[line " ++ (show line) ++ "] Error in " ++ file ++ ": "
  show (Error (SourceCodeLocation Nothing line)) = "[line " ++ (show line) ++ "] Error: "
  show (Success []) = ""
  show (Success t:rest) = (show Token) ++ " " ++ (show (Success rest))

createToken :: (Num a) => Char -> String -> Int -> Either SourceCodeLocation (TokenResult a)
createToken nextChar rest line
  | nextChar == '('   = oneCharToken $ Right LeftParen
  | nextChar == ')'   = oneCharToken $ Right RightParen
  | nextChar == '{'   = oneCharToken $ Right LeftBrace
  | nextChar == '}'   = oneCharToken $ Right RightBrace
  | nextChar == ','   = oneCharToken $ Right Comma
  | nextChar == '.'   = oneCharToken $ Right Dot
  | nextChar == '-'   = oneCharToken $ Right Minus
  | nextChar == '+'   = oneCharToken $ Right Plus
  | nextChar == ';'   = oneCharToken $ Right Semicolon
  | nextChar == '*'   = oneCharToken $ Right Star
  | nextChar == '\n'  = createToken (head rest) (tail rest) (line + 1)
  | otherwise         = Left $ SourceCodeLocation Nothing line
  where
    oneCharToken :: TokenType a -> TokenResult a
    oneCharToken t = (Token t (nextChar : []) (SourceCodeLocation Nothing line), rest, line)

scanToken :: (Num a) => String -> Int -> Either SourceCodeLocation (TokenResult a)
scanToken s l = createToken (head s) (tail s) l

scanTokens :: (Num a) => String -> ParseOutcome a
scanTokens s = scanTokens' s 1
  where
    scanTokens' :: (Num a) => String -> Int -> ParseOutcome a
    scanTokens' "" _ = Success []
    scanTokens' s l = case r of Left s@(SourceCodeLocation _ _) -> Error s "Unexpected character"
                                Right (token, rest, line) -> token : scanTokens' rest line
      where r = scanToken s l
