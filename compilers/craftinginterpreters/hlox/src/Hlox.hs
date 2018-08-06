{-# LANGUAGE ScopedTypeVariables #-}
module Hlox where

import Data.List (intercalate)
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

createToken :: Char -> String -> Int -> Either ProgramError TokenResult
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
  | nextChar == '"'                       = createStringToken (tail rest) line
  | otherwise                             = Left $ ProgramError (SourceCodeLocation Nothing line) "Unexpected character."
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

createStringToken :: String -> Int -> Either ProgramError TokenResult
createStringToken s line
  | elem '"' s  = Right $ (Token (StringLiteral string) (stringLiteral string) (SourceCodeLocation Nothing line), newRest, line + (breaklines string))
  | otherwise   = Left $ ProgramError (SourceCodeLocation Nothing (line+(breaklines s))) "Unterminated string."
  where string = head partitions
        newRest = intercalate ['"'] (tail partitions)
        partitions = splitOn ('"':[]) s
        breaklines s = length $ filter (== '\n') s
        stringLiteral s = ('"':s) ++ "\""


scanToken :: String -> Int -> Either ProgramError TokenResult
scanToken s l = createToken (head s) (tail s) l

scanTokens :: String -> ParseOutcome
scanTokens s = scanTokens' s 1
  where
    scanTokens' :: String -> Int -> ParseOutcome
    scanTokens' "" _ = Right []
    scanTokens' s l = scanToken s l >>= tokenResultToParseOutcome
    tokenResultToParseOutcome :: TokenResult -> ParseOutcome
    tokenResultToParseOutcome (token, rest, line) = fmap ((:) token) $ scanTokens' rest line
