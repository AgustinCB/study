{-# LANGUAGE ScopedTypeVariables #-}
module Hlox where

import Control.Arrow
import Data.List.Split (splitOn)

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
  Comment |
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
  | nextChar == '('                       = Right $ oneCharTokenWithoutRest LeftParen
  | nextChar == ')'                       = Right $ oneCharTokenWithoutRest RightParen
  | nextChar == '{'                       = Right $ oneCharTokenWithoutRest LeftBrace
  | nextChar == '}'                       = Right $ oneCharTokenWithoutRest RightBrace
  | nextChar == ','                       = Right $ oneCharTokenWithoutRest Comma
  | nextChar == '.'                       = Right $ oneCharTokenWithoutRest Dot
  | nextChar == '-'                       = Right $ oneCharTokenWithoutRest Minus
  | nextChar == '+'                       = Right $ oneCharTokenWithoutRest Plus
  | nextChar == ';'                       = Right $ oneCharTokenWithoutRest Semicolon
  | nextChar == '*'                       = Right $ oneCharTokenWithoutRest Star
  | nextChar == '!' && (head rest) == '=' = Right $ twoCharTokenWithoutRest BangEqual
  | nextChar == '!'                       = Right $ oneCharTokenWithoutRest Bang
  | nextChar == '=' && (head rest) == '=' = Right $ twoCharTokenWithoutRest EqualEqual
  | nextChar == '='                       = Right $ oneCharTokenWithoutRest Equal
  | nextChar == '<' && (head rest) == '=' = Right $ twoCharTokenWithoutRest LessEqual
  | nextChar == '<'                       = Right $ oneCharTokenWithoutRest Less
  | nextChar == '>' && (head rest) == '=' = Right $ twoCharTokenWithoutRest GreaterEqual
  | nextChar == '>'                       = Right $ oneCharTokenWithoutRest Greater
  | nextChar == '/' && (head rest) == '/' = Right $ tokenWithRestAndLiteral Comment secondPartition firstPartition
  | nextChar == '/'                       = Right $ oneCharTokenWithoutRest Slash
  | nextChar == '\n'                      = createToken (head rest) (tail rest) (line + 1)
  | elem nextChar [' ', '\r', '\t']       = createToken (head rest) (tail rest) (line + 1)
  | otherwise                             = Left $ SourceCodeLocation Nothing line
  where
    secondPartition = getSecondElement partitions
    firstPartition = getFirstElement partitions
    partitions = (splitOn ('\n':[]) rest)
    getSecondElement :: [String] -> String
    getSecondElement (s:(s1:_)) = s
    getSecondElement _ = ""
    getFirstElement :: [String] -> String
    getFirstElement [] = ""
    getFirstElement (s:[]) = s
    tokenWithRestAndLiteral :: TokenType -> String -> String -> TokenResult
    tokenWithRestAndLiteral t r l = (createToken' t l, r, line + 1)
    oneCharTokenWithoutRest :: TokenType -> TokenResult
    oneCharTokenWithoutRest t = (createToken' t (nextChar : []), rest, line)
    twoCharTokenWithoutRest :: TokenType -> TokenResult
    twoCharTokenWithoutRest t = (createToken' t (nextChar : (head rest) : []), rest, line)
    createToken' :: TokenType -> String -> Token
    createToken' t s = Token t s $ SourceCodeLocation Nothing line

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
