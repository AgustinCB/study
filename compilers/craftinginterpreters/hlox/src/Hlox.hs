{-# LANGUAGE ScopedTypeVariables #-}
module Hlox (scanTokens, evaluate, parseExpression,
                LoxValue, ScanningResult, ParsingResult, Token, ParsingStep, ProgramError) where

import Control.Monad (liftM2)
import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- Language specs

data DataKeyword = TrueKeyword | FalseKeyword | NilKeyword deriving (Show, Eq)

data Literal = StringLiteral String |
    KeywordLiteral DataKeyword |
    NumberLiteral Double deriving (Show, Eq)

data TokenType =
    LeftParen |
    RightParen |
    LeftBrace |
    RightBrace |
    Colon |
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
    Fun |
    For |
    If |
    Or |
    Print |
    Question |
    Return |
    Super |
    This |
    Var |
    While |
    Comment |
    Identifier String |
    TokenLiteral Literal deriving (Show, Eq)
data Token = Token {
      tokenType :: TokenType
    , lexeme :: String
    , tokenLocation :: SourceCodeLocation } deriving Show

data Expression = Conditional { condition :: Expression, thenBranch :: Expression, elseBranch :: Expression, expressionLocation :: SourceCodeLocation } |
    Binary { right :: Expression, operator :: TokenType, left :: Expression, expressionLocation :: SourceCodeLocation } |
    Unary { operator :: TokenType, operand :: Expression, expressionLocation :: SourceCodeLocation } |
    Grouping { expression :: Expression, expressionLocation :: SourceCodeLocation } |
    ExpressionLiteral { value :: Literal, expressionLocation :: SourceCodeLocation } deriving Show

type TokenResult = (Token, String, Int)

-- Program results

data SourceCodeLocation = SourceCodeLocation { file :: Maybe String, line :: Int } deriving Show
data ProgramError a = ProgramError { location :: SourceCodeLocation, msg :: String, rest :: [a] }
type ScanningResult = Either (ProgramError Char) [Token]

instance Show (ProgramError a) where
    show (ProgramError (SourceCodeLocation (Just file) line) msg _) =
        "[line " ++ (show line) ++ "] Error in " ++ file ++ ": " ++ msg
    show (ProgramError (SourceCodeLocation Nothing line) msg _) =
        "[line " ++ (show line) ++ "] Error: " ++ msg

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
    | nextChar == '!' && (head rest) == '=' = Right $ twoCharTokenWithoutRest BangEqual
    | nextChar == '!'                       = Right $ oneCharTokenWithoutRest Bang
    | nextChar == '=' && (head rest) == '=' = Right $ twoCharTokenWithoutRest EqualEqual
    | nextChar == '='                       = Right $ oneCharTokenWithoutRest Equal
    | nextChar == '<' && (head rest) == '=' = Right $ twoCharTokenWithoutRest LessEqual
    | nextChar == '<'                       = Right $ oneCharTokenWithoutRest Less
    | nextChar == '>' && (head rest) == '=' = Right $ twoCharTokenWithoutRest GreaterEqual
    | nextChar == '>'                       = Right $ oneCharTokenWithoutRest Greater
    | nextChar == '/' && (head rest) == '/' = Right $ tokenWithRestAndLiteral Comment secondPartition firstPartition
    | nextChar == '/' && (head rest) == '*' = Right $ tokenWithRestAndLiteral Comment commentRest comment
    | nextChar == '/'                       = Right $ oneCharTokenWithoutRest Slash
    | nextChar == '?'                       = Right $ oneCharTokenWithoutRest Question
    | nextChar == '\n'                      = createToken (head rest) (tail rest) (line + 1)
    | elem nextChar [' ', '\r', '\t']       = createToken (head rest) (tail rest) (line + 1)
    | nextChar == '"'                       = createStringToken (tail rest) line
    | isDigit nextChar                      = createNumberToken nextChar rest line
    | isAlpha nextChar || nextChar == '_'   = Right $ createIdentifierOrKeywordToken nextChar rest line
    | otherwise                             = Left $ ProgramError (SourceCodeLocation Nothing line) "Unexpected character." rest
    where
        secondPartition = getSecondElement partitions
        firstPartition = getFirstElement partitions
        partitions = splitOn ('\n':[]) rest
        (comment, commentRest) = getCommentAndRest (tail rest)
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
                    Right $ (Token (numberToken (read acc)) acc (SourceCodeLocation Nothing line), [], line)

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
          extractWord (c:rest) acc
              | isIdentifier c    = extractWord rest (acc ++ [c])
              | otherwise             = (acc, c:rest)
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
    | word == "true"  = createKeywordToken (TokenLiteral $ KeywordLiteral FalseKeyword) word rest line
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

type ParsingStep = (Expression, [Token])
type ParsingResult = Either (ProgramError Token) ParsingStep
type Parser = [Token] -> ParsingResult

parseExpression :: [Token] -> ParsingResult
parseExpression [] = Left $ ProgramError (SourceCodeLocation Nothing 1) "No input!" []
parseExpression list = parseComma list

createBinaryResult :: TokenType -> SourceCodeLocation -> Expression -> ParsingResult -> ParsingResult
createBinaryResult tokenType location expr = fmap (\r -> (Binary (fst r) tokenType expr location, snd r))

concatenate :: [TokenType] -> Parser -> Expression -> [Token] -> ParsingResult
concatenate _ _ expr [] = Right $ (expr, [])
concatenate tokens parser expr (head:rest)
  | elem (tokenType head) tokens = createBinaryResult (tokenType head) (tokenLocation head) expr (parser rest)
  | otherwise                    = Right $ (expr, head:rest)

parseComma :: Parser
parseComma list = (parseTernary list) >>= uncurry (concatenate [Comma] parseComma)

consume :: TokenType -> String -> [Token] -> Either (ProgramError Token) [Token]
consume needle error [] = Left $ ProgramError (SourceCodeLocation Nothing 1) error []
consume needle error (head:tail)
  | needle == (tokenType head)  = Right $ tail
  | otherwise                   = Left $ ProgramError (tokenLocation head) error tail

parseTernary :: Parser
parseTernary tokens = parseEquality tokens >>= parseTernaryOperator
    where parseTernaryOperator :: ParsingStep -> ParsingResult
          parseTernaryOperator (expr, [])   = Right $ (expr, [])
          parseTernaryOperator (expr, tokens@(head:rest))
            | (tokenType head) == Question  = createTernaryOperator expr rest
            | otherwise                     = Right $ (expr, tokens)
          createTernaryOperator :: Expression -> [Token] -> ParsingResult
          createTernaryOperator equality tokens@(head:_) = do
            (thenBranch, rest) <- parseExpression tokens
            rest <- consume Colon "Expect ':' after then branch of conditional expression." rest
            (elseBranch, rest) <- parseTernary rest
            Right $ (Conditional equality thenBranch elseBranch (tokenLocation head), rest)

parseEquality :: Parser
parseEquality list = (parseComparison list) >>= uncurry (concatenate [BangEqual, EqualEqual] parseEquality)

parseComparison :: Parser
parseComparison list = (parseAddition list) >>=
                            uncurry (concatenate [Greater, GreaterEqual, Less, LessEqual] parseComparison)

parseAddition :: Parser
parseAddition list = (parseMultiplication list) >>= uncurry (concatenate [Minus, Plus] parseAddition)

parseMultiplication :: Parser
parseMultiplication list = (parseUnary list) >>= uncurry (concatenate [Slash, Star] parseMultiplication)

parseUnary :: Parser
parseUnary (head:rest)
  | elem (tokenType head) [Bang, Minus] = fmap (\r -> (Unary (tokenType head) (fst r) (tokenLocation head), snd r)) result
  | otherwise                           = parsePrimary (head:rest)
    where result = parseUnary rest

parsePrimary :: Parser
parsePrimary ((Token (TokenLiteral literal) _ location):rest) = Right (ExpressionLiteral literal location, rest)
parsePrimary ((Token LeftParen _ location):rest) = let partition = partitionByToken RightParen rest
                                                       newExpr = fst partition
                                                       rest = snd partition
                                                   in case rest of
                                                       [] -> Left $ ProgramError (tokenLocation (head rest)) "Expecting right parenthesis" []
                                                       h:r -> fmap (\p -> (Grouping (fst p) location, r)) (parseExpression newExpr)
parsePrimary (head@(Token headType _ headLocation):r)
  | elem headType [EqualEqual, BangEqual]                   = fastForward parseEquality r >>=
                                                                Left . ProgramError headLocation "Equality without left side"
  | elem headType [Greater, GreaterEqual, Less, LessEqual]  = fastForward parseComparison r >>=
                                                                Left . ProgramError headLocation "Comparision without left side"
  | headType == Plus                                        = fastForward parseAddition r >>=
                                                                Left . ProgramError headLocation "Addition without left side"
  | elem headType [Slash, Star]                             = fastForward parseMultiplication r >>=
                                                                Left . ProgramError headLocation "Multiplication without left side"
  | otherwise                                               = Left $ ProgramError headLocation "Expecting a literal!" r

fastForward :: Parser -> [Token] -> Either (ProgramError Token) [Token]
fastForward p ts = fmap snd $ p ts

discardTillStatement :: [Token] -> [Token]
discardTillStatement [] = []
discardTillStatement (head@(Token headType _ _):tail)
  | headType == Semicolon                                           = tail
  | elem headType [Class, Fun, Var, For, If, While, Print, Return]  = (head:tail)
  | otherwise                                                       = discardTillStatement tail

partitionByToken :: TokenType -> [Token] -> ([Token], [Token])
partitionByToken t = span ((/= t) .tokenType)

-- Evaluation
data LoxValue = NilValue
              | BooleanValue { boolean :: Bool }
              | NumberValue { number :: Double }
              | StringValue { string :: String } deriving (Eq, Show)
type EvaluationResult = Either (ProgramError Expression) LoxValue

isTruthy :: LoxValue -> LoxValue
isTruthy NilValue = BooleanValue False
isTruthy (BooleanValue False) = BooleanValue False
isTruthy _ = BooleanValue True

negateTruthy :: LoxValue -> LoxValue
negateTruthy e = BooleanValue $ not . boolean $ isTruthy e

negateDouble :: SourceCodeLocation -> LoxValue -> EvaluationResult
negateDouble _ (NumberValue v) = Right $ NumberValue (-v)
negateDouble location _ = Left $ ProgramError location "Can only negate numbers" []

expectNumber :: SourceCodeLocation -> LoxValue -> EvaluationResult
expectNumber _ n@(NumberValue v ) = Right $ n
expectNumber location _ = Left $ ProgramError location "Type error! Expecting a double!" []

expectString :: SourceCodeLocation -> LoxValue -> EvaluationResult
expectString _ n@(StringValue v ) = Right $ n
expectString location _ = Left $ ProgramError location "Type error! Expecting a string!" []

type MathOperation = Double -> Double -> Double
mathOperation :: SourceCodeLocation -> Expression -> MathOperation -> Expression -> EvaluationResult
mathOperation location left op right = do
  rightOp <- evaluate right >>= (expectNumber location)
  leftOp <- evaluate right >>= (expectNumber location)
  return $ NumberValue ((number leftOp) `op` (number rightOp))

type BooleanOperation = Double -> Double -> Bool
comparisonOperation :: SourceCodeLocation -> Expression -> BooleanOperation -> Expression -> EvaluationResult
comparisonOperation location left op right = do
  rightOp <- evaluate right >>= (expectNumber location)
  leftOp <- evaluate right >>= (expectNumber location)
  return $ BooleanValue ((number leftOp) `op` (number rightOp))

isEquals :: Expression -> Expression -> EvaluationResult
isEquals left right = do
  rightOp <- evaluate right
  leftOp <- evaluate right
  return $ BooleanValue (leftOp == rightOp)

concatenateValues :: SourceCodeLocation -> Expression -> Expression -> EvaluationResult
concatenateValues location left right = do
  rightOp <- evaluate right >>= (expectString location)
  leftOp <- evaluate right >>= (expectString location)
  return $ StringValue ((string leftOp) ++ (string rightOp))

evaluate :: Expression -> EvaluationResult
evaluate (ExpressionLiteral (KeywordLiteral NilKeyword) _) = Right $ NilValue
evaluate (ExpressionLiteral (KeywordLiteral TrueKeyword) _) = Right $ BooleanValue True
evaluate (ExpressionLiteral (KeywordLiteral FalseKeyword) _) = Right $ BooleanValue False
evaluate (ExpressionLiteral (NumberLiteral v) _) = Right $ NumberValue v
evaluate (ExpressionLiteral (StringLiteral s) _) = Right $ StringValue s
evaluate (Grouping expr _) = evaluate expr
evaluate (Unary Bang expr _) = fmap negateTruthy $ evaluate expr
evaluate (Unary Minus expr location) = evaluate expr >>= (negateDouble location)
evaluate (Binary left Minus right location) = mathOperation location left (-) right
evaluate (Binary left Star right location) = mathOperation location left (*) right
evaluate (Binary left Slash right location) =
  let div = mathOperation location left (/) right
      inf = 1/0
  in case div of (Right (NumberValue inf)) -> Left $ ProgramError location "Division by zero!" []
                 r -> r
evaluate (Binary left Plus right location) =
  let sum = mathOperation location left (+) right
  in case sum of r@(Right _) -> r
                 (Left _) -> concatenateValues location left right
evaluate (Binary left Greater right location) = comparisonOperation location left (>) right
evaluate (Binary left GreaterEqual right location) = comparisonOperation location left (>=) right
evaluate (Binary left Less right location) = comparisonOperation location left (<) right
evaluate (Binary left LessEqual right location) = comparisonOperation location left (<=) right
evaluate (Binary left EqualEqual right _) = isEquals left right
evaluate (Binary left BangEqual right _) = fmap negateTruthy $ isEquals left right
evaluate (Binary left Comma right _) = liftM2 seq (evaluate left) (evaluate right)
evaluate (Conditional condition thenBranch elseBranch location) = do
  isTruth <- fmap isTruthy (evaluate condition)
  if (boolean isTruth) then evaluate thenBranch
                       else evaluate elseBranch