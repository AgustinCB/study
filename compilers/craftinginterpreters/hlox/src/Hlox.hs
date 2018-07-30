{-# LANGUAGE ScopedTypeVariables #-}
module Hlox where

import Control.Arrow

-- Language specs

data TokenType =
  LeftParen | RightParen |
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
  Greater |
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
  Number Double deriving Show
data Token = Token {
    tokenType :: TokenType
  , lexeme :: String
  , tokenLocation :: SourceCodeLocation } deriving Show

type TokenResult = (Token, String, Int)

-- Program results

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int } deriving Show
data ProgramError = ProgramError { location :: SourceCodeLocation, msg :: String }
type ParseOutcome = Either ProgramError [Token]

instance Show ProgramError where
  show (ProgramError (SourceCodeLocation (Just file) line) msg) =
    "[line " ++ (show line) ++ "] Error in " ++ file ++ ": " ++ msg
  show (ProgramError (SourceCodeLocation Nothing line) msg) =
    "[line " ++ (show line) ++ "] Error: " ++ msg

createToken :: Char -> String -> Int -> Either SourceCodeLocation TokenResult
createToken nextChar rest line
  | nextChar == '('                       = Right $ oneCharToken LeftParen
  | nextChar == ')'                       = Right $ oneCharToken RightParen
  | nextChar == '{'                       = Right $ oneCharToken LeftBrace
  | nextChar == '}'                       = Right $ oneCharToken RightBrace
  | nextChar == ','                       = Right $ oneCharToken Comma
  | nextChar == '.'                       = Right $ oneCharToken Dot
  | nextChar == '-'                       = Right $ oneCharToken Minus
  | nextChar == '+'                       = Right $ oneCharToken Plus
  | nextChar == ';'                       = Right $ oneCharToken Semicolon
  | nextChar == '*'                       = Right $ oneCharToken Star
  | nextChar == '!' && (head rest) == '=' = Right $ oneCharToken BangEqual
  | nextChar == '!'                       = Right $ oneCharToken Bang
  | nextChar == '=' && (head rest) == '=' = Right $ oneCharToken EqualEqual
  | nextChar == '='                       = Right $ oneCharToken Equal
  | nextChar == '<' && (head rest) == '=' = Right $ oneCharToken LessEqual
  | nextChar == '<'                       = Right $ oneCharToken Less
  | nextChar == '>' && (head rest) == '=' = Right $ oneCharToken GreaterEqual
  | nextChar == '>'                       = Right $ oneCharToken Greater
  | nextChar == '\n'  = createToken (head rest) (tail rest) (line + 1)
  | otherwise         = Left $ SourceCodeLocation Nothing line
  where
    oneCharToken :: TokenType -> TokenResult
    oneCharToken t = (Token t (nextChar : []) (SourceCodeLocation Nothing line), rest, line)

scanToken :: String -> Int -> Either SourceCodeLocation TokenResult
scanToken s l = createToken (head s) (tail s) l

scanTokens :: String -> ParseOutcome
scanTokens s = scanTokens' s 1
  where
    scanTokens' :: String -> Int -> ParseOutcome
    scanTokens' "" _ = Right []
    scanTokens' s l = case r of Left s -> Left $ locationToError s
                                Right (token, rest, line) -> (transformError (fmap fst' r)) >>= addToParseOutcome rest line
      where r = scanToken s l
            fst' :: (a, b, c) -> a
            fst' (a, _, _) = a
            first :: (b -> c) -> Either b d -> Either c d
            first = left
            locationToError :: SourceCodeLocation -> ProgramError
            locationToError s = ProgramError s "Unexpected character."
            transformError :: Either SourceCodeLocation a -> Either ProgramError a
            transformError e = first locationToError e
            addToParseOutcome :: String -> Int -> Token -> ParseOutcome
            addToParseOutcome rest line token = fmap ((:) token) $ scanTokens' rest line
