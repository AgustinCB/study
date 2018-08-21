{-# LANGUAGE ScopedTypeVariables #-}
module Hlox where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- Language specs

data Literal = StringLiteral String |
    NumberLiteral Double

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
    NumberLiteral Double deriving Show
data Token = Token {
      tokenType :: TokenType
    , lexeme :: String
    , tokenLocation :: SourceCodeLocation } deriving Show

data Expression = Binary { right :: Expression, operator :: TokenType, left :: Expression } |
    Unary { operator :: TokenType, operand :: Expression } |
    Grouping { expression :: Expression } |
    Literal { value :: TokenType }

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
    | nextChar == '/' && (head rest) == '*' = Right $ tokenWithRestAndLiteral Comment rest comment
    | nextChar == '/'                       = Right $ oneCharTokenWithoutRest Slash
    | nextChar == '\n'                      = createToken (head rest) (tail rest) (line + 1)
    | elem nextChar [' ', '\r', '\t']       = createToken (head rest) (tail rest) (line + 1)
    | nextChar == '"'                       = createStringToken (tail rest) line
    | isDigit nextChar                      = createNumberToken nextChar rest line
    | isAlpha nextChar || nextChar == '_'   = Right $ createIdentifierOrKeywordToken nextChar rest line
    | otherwise                             = Left $ ProgramError (SourceCodeLocation Nothing line) "Unexpected character."
    where
        secondPartition = getSecondElement partitions
        firstPartition = getFirstElement partitions
        partitions = splitOn ('\n':[]) rest
        (comment, rest) = getCommentAndRest (tail rest)
        getCommentAndRest :: String -> (String, String)
        getCommentAndRest content = (getFirstElement partition, getSecondElement partition)
            where partition = splitOn "*/" content
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

createNumberToken :: Char -> String -> Int -> Either ProgramError TokenResult
createNumberToken firstChar rest line = createNumberToken' [firstChar] rest
    where createNumberToken' :: String -> String -> Either ProgramError TokenResult
          createNumberToken' acc (head:r) = if (isDigit head || head == '.') then createNumberToken' (acc ++ [head]) r else maybeFinish acc (head:r)
          error = Left $ ProgramError (SourceCodeLocation Nothing line) "Invalid number."
          maybeFinish :: String -> String -> Either ProgramError TokenResult
          maybeFinish acc rest
              | last acc == '.'                                         = error
              | (length $ filter (== '.') acc) > 1    = error
              | otherwise                                                     = Right $ (Token (NumberLiteral (read acc)) acc (SourceCodeLocation Nothing line), [], line)

createStringToken :: String -> Int -> Either ProgramError TokenResult
createStringToken s line
    | elem '"' s    = Right $ (Token (StringLiteral string) (stringLiteral string) (SourceCodeLocation Nothing line), newRest, line + (breaklines string))
    | otherwise     = Left $ ProgramError (SourceCodeLocation Nothing (line+(breaklines s))) "Unterminated string."
    where string = head partitions
          newRest = intercalate ['"'] (tail partitions)
          partitions = splitOn ('"':[]) s
          breaklines s = length $ filter (== '\n') s
          stringLiteral s = ('"':s) ++ "\""

createIdentifierOrKeywordToken :: Char -> String -> Int -> TokenResult
createIdentifierOrKeywordToken nextChar input line = identifierOrKeywordToken word rest line
    where (word, rest) = extractWord (nextChar:input) []
          extractWord :: String -> String -> (String, String)
          extractWord (c:rest) acc
              | isIdentifier c    = extractWord rest (acc ++ [c])
              | otherwise             = (acc, c:rest)
          isIdentifier :: Char -> Bool
          isIdentifier c = isDigit c || isAlpha c || '_' == c

identifierOrKeywordToken :: String -> String -> Int -> TokenResult
identifierOrKeywordToken word rest line
    | word == "and"     = createKeywordToken And word rest line
    | word == "class" = createKeywordToken Class word rest line
    | word == "else"    = createKeywordToken Else word rest line
    | word == "false" = createKeywordToken FalseKeyword word rest line
    | word == "for"     = createKeywordToken For word rest line
    | word == "fun"     = createKeywordToken Fun word rest line
    | word == "if"        = createKeywordToken If word rest line
    | word == "nil"     = createKeywordToken Nil word rest line
    | word == "or"        = createKeywordToken Or word rest line
    | word == "print" = createKeywordToken Print word rest line
    | word == "return"= createKeywordToken Return word rest line
    | word == "super" = createKeywordToken Super word rest line
    | word == "this"    = createKeywordToken This word rest line
    | word == "true"    = createKeywordToken TrueKeyword word rest line
    | word == "var"     = createKeywordToken Var word rest line
    | word == "while" = createKeywordToken While word rest line
    | otherwise             = createIdentifierToken word rest line

createKeywordToken :: TokenType -> String -> String -> Int -> TokenResult
createKeywordToken tokenType value rest line = (Token tokenType value (SourceCodeLocation Nothing line), rest, line)

createIdentifierToken :: String -> String -> Int -> TokenResult
createIdentifierToken value rest line = 
    (Token (Identifier value) value (SourceCodeLocation Nothing line), rest, line)

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
