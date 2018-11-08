module Lexer where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Types

createToken :: Char -> String -> Int -> Either (ProgramError Char) TokenResult
createToken nextChar rest line
    | nextChar == '('                       = Right $ oneCharTokenWithoutRest LeftParen
    | nextChar == ')'                       = Right $ oneCharTokenWithoutRest RightParen
    | nextChar == '{'                       = Right $ oneCharTokenWithoutRest LeftBrace
    | nextChar == '}'                       = Right $ oneCharTokenWithoutRest RightBrace
    | nextChar == ':'                       = Right $ oneCharTokenWithoutRest Colon
    | nextChar == ','                       = Right $ oneCharTokenWithoutRest Comma
    | nextChar == '.'                       = Right $ oneCharTokenWithoutRest Dot
    | nextChar == '-'                       = Right $ oneCharTokenWithoutRest Minus
    | nextChar == '+'                       = Right $ oneCharTokenWithoutRest Plus
    | nextChar == ';'                       = Right $ oneCharTokenWithoutRest Semicolon
    | nextChar == '*'                       = Right $ oneCharTokenWithoutRest Star
    | nextChar == '!' && headRest == '='    = Right $ twoCharTokenWithoutRest BangEqual
    | nextChar == '!'                       = Right $ oneCharTokenWithoutRest Bang
    | nextChar == '=' && headRest== '='     = Right $ twoCharTokenWithoutRest EqualEqual
    | nextChar == '='                       = Right $ oneCharTokenWithoutRest Equal
    | nextChar == '<' && headRest == '='    = Right $ twoCharTokenWithoutRest LessEqual
    | nextChar == '<'                       = Right $ oneCharTokenWithoutRest Less
    | nextChar == '>' && headRest == '='    = Right $ twoCharTokenWithoutRest GreaterEqual
    | nextChar == '>'                       = Right $ oneCharTokenWithoutRest Greater
    | nextChar == '/' && headRest == '/'    = Right $ tokenWithRestAndLiteral Comment secondPartition firstPartition
    | nextChar == '/' && headRest == '*'    = Right $ tokenWithRestAndLiteral Comment commentRest comment
    | nextChar == '/'                       = Right $ oneCharTokenWithoutRest Slash
    | nextChar == '?'                       = Right $ oneCharTokenWithoutRest Question
    | nextChar == '\n'                      = createToken headRest tailRest (line + 1)
    | elem nextChar [' ', '\r', '\t']       = createToken headRest tailRest line
    | nextChar == '"'                       = createStringToken rest line
    | isDigit nextChar                      = createNumberToken nextChar rest line
    | isAlpha nextChar || nextChar == '_'   = Right $ createIdentifierOrKeywordToken nextChar rest line
    | otherwise                             = Left $ ProgramError (SourceCodeLocation Nothing line) "Unexpected character." rest
    where
        headRest = maybe '\0' id (listToMaybe rest)
        tailRest = if rest == [] then [] else tail rest
        secondPartition = getSecondElement partitions
        firstPartition = getFirstElement partitions
        partitions = splitOn ('\n':[]) rest
        (comment, commentRest) = getCommentAndRest tailRest
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
        twoCharTokenWithoutRest t = (createToken' t (nextChar : headRest : []), tailRest, line)
        createToken' :: TokenType -> String -> Token
        createToken' t s = Token t s $ SourceCodeLocation Nothing line

createNumberToken :: Char -> String -> Int -> Either (ProgramError Char) TokenResult
createNumberToken firstChar rest line = createNumberToken' [firstChar] rest
    where createNumberToken' :: String -> String -> Either (ProgramError Char) TokenResult
          createNumberToken' acc [] = maybeFinish acc []
          createNumberToken' acc (head:r) = if (isDigit head || head == '.') then createNumberToken' (acc ++ [head]) r else maybeFinish acc (head:r)
          numberToken :: Double -> TokenType
          numberToken n = TokenLiteral $ NumberLiteral n
          maybeFinish :: String -> String -> Either (ProgramError Char) TokenResult
          maybeFinish acc rest
              | last acc == '.'                       =
                    Left $ ProgramError (SourceCodeLocation Nothing line) "Invalid number." rest
              | (length $ filter (== '.') acc) > 1    =
                    Left $ ProgramError (SourceCodeLocation Nothing line) "Invalid number." rest
              | otherwise                             =
                    Right $ (Token (numberToken (read acc)) acc (SourceCodeLocation Nothing line), rest, line)

createStringToken :: String -> Int -> Either (ProgramError Char) TokenResult
createStringToken s line
    | elem '"' s    = Right $ (Token (stringToken string) (stringLiteral string) (SourceCodeLocation Nothing line), newRest, line + (breaklines string))
    | otherwise     = Left $ ProgramError (SourceCodeLocation Nothing (line+(breaklines s))) "Unterminated string." []
    where string = head partitions
          newRest = intercalate ['"'] (tail partitions)
          partitions = splitOn ('"':[]) s
          breaklines s = length $ filter (== '\n') s
          stringLiteral s = ('"':s) ++ "\""
          stringToken :: String -> TokenType
          stringToken s = TokenLiteral $ StringLiteral s

createIdentifierOrKeywordToken :: Char -> String -> Int -> TokenResult
createIdentifierOrKeywordToken nextChar input line = identifierOrKeywordToken word rest line
    where (word, rest) = extractWord (nextChar:input) []
          extractWord :: String -> String -> (String, String)
          extractWord [] acc = (acc, [])
          extractWord (c:rest) acc
              | isIdentifier c    = extractWord rest (acc ++ [c])
              | otherwise         = (acc, c:rest)
          isIdentifier :: Char -> Bool
          isIdentifier c = isDigit c || isAlpha c || '_' == c

identifierOrKeywordToken :: String -> String -> Int -> TokenResult
identifierOrKeywordToken word rest line
    | word == "and"   = createKeywordToken And word rest line
    | word == "class" = createKeywordToken Class word rest line
    | word == "else"  = createKeywordToken Else word rest line
    | word == "false" = createKeywordToken (TokenLiteral $ KeywordLiteral FalseKeyword) word rest line
    | word == "for"   = createKeywordToken For word rest line
    | word == "fun"   = createKeywordToken Fun word rest line
    | word == "if"    = createKeywordToken If word rest line
    | word == "nil"   = createKeywordToken (TokenLiteral $ KeywordLiteral NilKeyword) word rest line
    | word == "or"    = createKeywordToken Or word rest line
    | word == "print" = createKeywordToken Print word rest line
    | word == "return"= createKeywordToken Return word rest line
    | word == "super" = createKeywordToken Super word rest line
    | word == "this"  = createKeywordToken This word rest line
    | word == "true"  = createKeywordToken (TokenLiteral $ KeywordLiteral TrueKeyword) word rest line
    | word == "var"   = createKeywordToken Var word rest line
    | word == "while" = createKeywordToken While word rest line
    | otherwise       = createIdentifierToken word rest line

createKeywordToken :: TokenType -> String -> String -> Int -> TokenResult
createKeywordToken tokenType value rest line = (Token tokenType value (SourceCodeLocation Nothing line), rest, line)

createIdentifierToken :: String -> String -> Int -> TokenResult
createIdentifierToken value rest line =
    (Token (Identifier value) value (SourceCodeLocation Nothing line), rest, line)

scanTokens :: String -> ScanningResult
scanTokens s = scanTokens' s 1
    where
        scanTokens' :: String -> Int -> ScanningResult
        scanTokens' "" _ = Right []
        scanTokens' (h:t) l = (createToken h t l) >>= tokenResultToParseOutcome
        tokenResultToParseOutcome :: TokenResult -> ScanningResult
        tokenResultToParseOutcome (token, rest, line) = fmap ((:) token) $ scanTokens' rest line