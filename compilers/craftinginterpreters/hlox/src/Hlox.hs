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
data ProgramError = ProgramError { location :: SourceCodeLocation, msg :: String }
type ParseOutcome a = Either ProgramError [Token a]

instance Show ProgramError where
  show (ProgramError (SourceCodeLocation (Just file) line) msg) =
    "[line " ++ (show line) ++ "] Error in " ++ file ++ ": " ++ msg
  show (ProgramError (SourceCodeLocation Nothing line) msg) =
    "[line " ++ (show line) ++ "] Error: " ++ msg

createToken :: (Num a) => Char -> String -> Int -> Either SourceCodeLocation (TokenResult a)
createToken nextChar rest line
  | nextChar == '('   = Right $ oneCharToken LeftParen
  | nextChar == ')'   = Right $ oneCharToken RightParen
  | nextChar == '{'   = Right $ oneCharToken LeftBrace
  | nextChar == '}'   = Right $ oneCharToken RightBrace
  | nextChar == ','   = Right $ oneCharToken Comma
  | nextChar == '.'   = Right $ oneCharToken Dot
  | nextChar == '-'   = Right $ oneCharToken Minus
  | nextChar == '+'   = Right $ oneCharToken Plus
  | nextChar == ';'   = Right $ oneCharToken Semicolon
  | nextChar == '*'   = Right $ oneCharToken Star
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
    scanTokens' "" _ = Right []
    scanTokens' s l = case r of Left s -> Left $ locationToError s
                                Right (token, rest, line) -> transformError $ (fmap fst' r) >>= const (scanTokens' rest line)
      where r = scanToken s l
            fst' :: (a, b, c) -> a
            fst' (a, _, _) = a
            locationToError :: SourceCodeLocation -> ProgramError
            locationToError s = ProgramError s "Unexpected character."
            transformError :: Either SourceCodeLocation a -> Either ProgramError a
            transformError e = first locationToError transformError
