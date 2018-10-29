{-# LANGUAGE ScopedTypeVariables #-}
module Hlox (scanTokens, evaluateStatement, parseStatement, Statement, ScanningResult, LoxState,
             ParsingExpressionResult, Token, ParsingExpressionStep, ProgramError, zeroState) where

import Control.Monad (liftM2)
import Data.Char (isDigit, isAlpha)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)

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
    ExpressionLiteral { value :: Literal, expressionLocation :: SourceCodeLocation } |
    VariableLiteral { identifier :: String, expressionLocation :: SourceCodeLocation } |
    VariableAssignment { identifier :: String, expression :: Expression, expressionLocation :: SourceCodeLocation }
    deriving Show

data Statement = StatementExpression SourceCodeLocation Expression |
    PrintStatement SourceCodeLocation Expression |
    VariableDeclaration SourceCodeLocation String (Maybe Expression) |
    BlockStatement SourceCodeLocation [Statement] deriving Show

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

type ParsingStatementStep = ([Token], Statement)
type ParsingStatementResult = Either (ProgramError Token) ParsingStatementStep
type StatementParser = [Token] -> ParsingStatementResult

createStatementFromExpression :: (Expression -> Statement) -> [Token] -> ParsingStatementResult
createStatementFromExpression f list = do
  (rest, expression) <- parseExpression list
  newRest <- consume Semicolon "Expected semicolon" rest
  return (newRest, f expression)

parseStatement :: StatementParser
parseStatement [] = Left $ ProgramError (SourceCodeLocation Nothing 1) "No input!" []
parseStatement ((Token Print _ l):list) = createStatementFromExpression (PrintStatement l) list
parseStatement ((Token Var _ l):(Token (Identifier ident) _ _):(Token Semicolon _ _):list) =
  Right $ (list, VariableDeclaration l ident Nothing)
parseStatement ((Token Var _ l):(Token (Identifier ident) _ _):(Token Equal _ _):list) =
  createStatementFromExpression ((VariableDeclaration l ident) . Just) list
parseStatement ((Token Var _ location):list) = Left $ ProgramError location "Invalid variable declaration!" []
parseStatement ((Token LeftBrace _ l):list) = do
  newRest <- consume RightBrace "Expected '}' after block" list
  return (newRest, BlockStatement l [])
parseStatement list@((Token _ _ l):_) = createStatementFromExpression (StatementExpression l) list

type ParsingExpressionStep = ([Token], Expression)
type ParsingExpressionResult = Either (ProgramError Token) ParsingExpressionStep
type ExpressionParser = [Token] -> ParsingExpressionResult

parseExpression :: ExpressionParser
parseExpression [] = Left $ ProgramError (SourceCodeLocation Nothing 1) "No input!" []
parseExpression list = parseAssignment list

createBinaryResult :: TokenType -> SourceCodeLocation -> Expression -> ParsingExpressionResult -> ParsingExpressionResult
createBinaryResult tokenType location expr = fmap (\r -> (fst r, Binary expr tokenType (snd r) location))

concatenate :: [TokenType] -> ExpressionParser -> [Token] -> Expression -> ParsingExpressionResult
concatenate _ _ [] expr = Right $ ([], expr)
concatenate tokens parser (head:rest) expr
  | elem (tokenType head) tokens = createBinaryResult (tokenType head) (tokenLocation head) expr (parser rest)
  | otherwise                    = Right $ (head:rest, expr)

consume :: TokenType -> String -> [Token] -> Either (ProgramError Token) [Token]
consume needle error [] = Left $ ProgramError (SourceCodeLocation Nothing 1) error []
consume needle error (head:tail)
  | needle == (tokenType head)  = Right $ tail
  | otherwise                   = Left $ ProgramError (tokenLocation head) error tail

parseAssignment :: ExpressionParser
parseAssignment tokens = do
  (rest, variable) <- parseComma tokens
  case rest of (Token Equal _ l):[] -> Left $ ProgramError l "No right side in assignment" []
               (Token Equal _ _):r -> do
                 (newRest, value) <- parseAssignment r
                 case variable of (VariableLiteral ident l) -> Right (newRest, (VariableAssignment ident value l))
                                  a -> Left $ ProgramError (expressionLocation a) "Invalid assignment target" newRest
               _ -> Right (rest, variable)

parseComma :: ExpressionParser
parseComma list = (parseTernary list) >>= uncurry (concatenate [Comma] parseComma)

parseTernary :: ExpressionParser
parseTernary tokens = parseEquality tokens >>= parseTernaryOperator
    where parseTernaryOperator :: ParsingExpressionStep -> ParsingExpressionResult
          parseTernaryOperator ([], expr)   = Right $ ([], expr)
          parseTernaryOperator (tokens@(head:rest), expr)
            | (tokenType head) == Question  = createTernaryOperator expr rest
            | otherwise                     = Right $ (tokens, expr)
          createTernaryOperator :: Expression -> [Token] -> ParsingExpressionResult
          createTernaryOperator equality tokens@(head:_) = do
            (rest, thenBranch) <- parseExpression tokens
            rest <- consume Colon "Expect ':' after then branch of conditional expression." rest
            (rest, elseBranch) <- parseTernary rest
            Right $ (rest, Conditional equality thenBranch elseBranch (tokenLocation head))

parseEquality :: ExpressionParser
parseEquality list = (parseComparison list) >>= uncurry (concatenate [BangEqual, EqualEqual] parseEquality)

parseComparison :: ExpressionParser
parseComparison list = (parseAddition list) >>=
                            uncurry (concatenate [Greater, GreaterEqual, Less, LessEqual] parseComparison)

parseAddition :: ExpressionParser
parseAddition list = (parseMultiplication list) >>= uncurry (concatenate [Minus, Plus] parseAddition)

parseMultiplication :: ExpressionParser
parseMultiplication list = (parseUnary list) >>= uncurry (concatenate [Slash, Star] parseMultiplication)

parseUnary :: ExpressionParser
parseUnary (head:rest)
  | elem (tokenType head) [Bang, Minus] = fmap (\r -> (fst r, Unary (tokenType head) (snd r) (tokenLocation head))) result
  | otherwise                           = parsePrimary (head:rest)
    where result = parseUnary rest

parsePrimary :: ExpressionParser
parsePrimary ((Token (TokenLiteral literal) _ location):rest) = Right (rest, ExpressionLiteral literal location)
parsePrimary ((Token (Identifier ident) _ location):rest) = Right (rest, VariableLiteral ident location)
parsePrimary ((Token LeftParen _ location):rest) = let partition = partitionByToken RightParen rest
                                                       newExpr = fst partition
                                                       newRest = snd partition
                                                   in case newRest of
                                                       [] -> Left $ ProgramError (tokenLocation (head rest)) "Expecting right parenthesis" []
                                                       h:r -> fmap (\p -> (r, Grouping (snd p) location)) (parseExpression newExpr)
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

fastForward :: ExpressionParser -> [Token] -> Either (ProgramError Token) [Token]
fastForward p ts = fmap fst $ p ts

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
              | StringValue { string :: String } deriving (Eq)
type EvaluationExpressionResult = Either (LoxState, (ProgramError Expression)) (LoxState, LoxValue)
type EvaluationResult = Either (LoxState, (ProgramError Expression)) LoxState
type LoxState = Map.Map String LoxValue

instance Show LoxValue where
  show NilValue = "nil"
  show (BooleanValue b) = show b
  show (NumberValue n) = show n
  show (StringValue s) = s

isTruthy :: LoxValue -> LoxValue
isTruthy NilValue = BooleanValue False
isTruthy (BooleanValue False) = BooleanValue False
isTruthy _ = BooleanValue True

negateTruthy :: LoxValue -> LoxValue
negateTruthy e = BooleanValue $ not . boolean $ isTruthy e

negateDouble :: SourceCodeLocation -> LoxState -> LoxValue -> EvaluationExpressionResult
negateDouble _ s (NumberValue v) = Right $ (s, NumberValue (-v))
negateDouble location s _ = Left (s, ProgramError location "Can only negate numbers" [])

expectNumber :: SourceCodeLocation -> LoxState -> LoxValue -> EvaluationExpressionResult
expectNumber _ s n@(NumberValue v) = Right $ (s, n)
expectNumber location s _ = Left (s, ProgramError location "Type error! Expecting a double!" [])

expectString :: SourceCodeLocation -> LoxState -> LoxValue -> EvaluationExpressionResult
expectString _ s n@(StringValue v ) = Right $ (s, n)
expectString location s _ = Left (s, ProgramError location "Type error! Expecting a string!" [])

type MathOperation = Double -> Double -> Double
mathOperation :: SourceCodeLocation -> Expression -> MathOperation -> Expression -> LoxState -> EvaluationExpressionResult
mathOperation location left op right s = do
  (_, rightOp) <- evaluateExpression s right >>= uncurry (expectNumber location)
  (_, leftOp) <- evaluateExpression s left >>= uncurry (expectNumber location)
  return $ (s, NumberValue ((number leftOp) `op` (number rightOp)))

type BooleanOperation = Double -> Double -> Bool
comparisonOperation :: SourceCodeLocation -> Expression -> BooleanOperation -> Expression -> LoxState -> EvaluationExpressionResult
comparisonOperation location left op right s = do
  (_, rightOp) <- evaluateExpression s right >>= uncurry (expectNumber location)
  (_, leftOp) <- evaluateExpression s left >>= uncurry (expectNumber location)
  return $ (s, BooleanValue ((number leftOp) `op` (number rightOp)))

isEquals :: Expression -> Expression -> LoxState -> EvaluationExpressionResult
isEquals left right s = do
  (_, rightOp) <- evaluateExpression s right
  (_, leftOp) <- evaluateExpression s left
  return $ (s, BooleanValue (leftOp == rightOp))

concatenateValues :: SourceCodeLocation -> Expression -> Expression -> LoxState -> EvaluationExpressionResult
concatenateValues location left right s = do
  (_, rightOp) <- evaluateExpression s right >>= uncurry (expectString location)
  (_, leftOp) <- evaluateExpression s left >>= uncurry (expectString location)
  return $ (s, StringValue ((string leftOp) ++ (string rightOp)))

maybeToEvaluationExpressionResult :: ProgramError Expression -> LoxState -> Maybe LoxValue -> EvaluationExpressionResult
maybeToEvaluationExpressionResult _ s (Just v) = Right (s, v)
maybeToEvaluationExpressionResult e s Nothing = Left (s, e)

evaluateExpression :: LoxState -> Expression -> EvaluationExpressionResult
evaluateExpression s (ExpressionLiteral (KeywordLiteral NilKeyword) _) = Right $ (s, NilValue)
evaluateExpression s (ExpressionLiteral (KeywordLiteral TrueKeyword) _) = Right $ (s, BooleanValue True)
evaluateExpression s (ExpressionLiteral (KeywordLiteral FalseKeyword) _) = Right $ (s, BooleanValue False)
evaluateExpression s (ExpressionLiteral (NumberLiteral v) _) = Right $ (s, NumberValue v)
evaluateExpression s (ExpressionLiteral (StringLiteral string) _) = Right $ (s, StringValue string)
evaluateExpression state (VariableLiteral ident l) = maybeToEvaluationExpressionResult (ProgramError l "Variable not found!" []) state
                                                        $ Map.lookup ident state
evaluateExpression s (Grouping expr _) = evaluateExpression s expr
evaluateExpression s (Unary Bang expr _) = do
  (ns, value) <- evaluateExpression s expr
  return $ (ns, negateTruthy value)
evaluateExpression s (Unary Minus expr location) = evaluateExpression s expr >>= uncurry (negateDouble location)
evaluateExpression s (Binary left Minus right location) = mathOperation location left (-) right s
evaluateExpression s (Binary left Star right location) = mathOperation location left (*) right s
evaluateExpression s (Binary left Slash right location) =
  let div = mathOperation location left (/) right s
      inf = 1/0
  in case div of r@(Right (s, (NumberValue inf))) -> Left (s, ProgramError location "Division by zero!" [])
                 r -> r
evaluateExpression s (Binary left Plus right location) =
  let sum = mathOperation location left (+) right s
  in case sum of r@(Right _) -> r
                 Left (s, ProgramError _ "Type error! Expecting a double!" _) -> concatenateValues location left right s
                 e@(Left _) -> e
evaluateExpression s (Binary left Greater right location) = comparisonOperation location left (>) right s
evaluateExpression s (Binary left GreaterEqual right location) = comparisonOperation location left (>=) right s
evaluateExpression s (Binary left Less right location) = comparisonOperation location left (<) right s
evaluateExpression s (Binary left LessEqual right location) = comparisonOperation location left (<=) right s
evaluateExpression s (Binary left EqualEqual right _) = isEquals left right s
evaluateExpression s (Binary left BangEqual right _) = do
  (ns, value) <- isEquals left right s
  return $ (ns, negateTruthy value)
evaluateExpression s (Binary left Comma right _) = liftM2 seq (evaluateExpression s left) (evaluateExpression s right)
evaluateExpression s (Conditional condition thenBranch elseBranch location) = do
  (ns, value) <- evaluateExpression s condition
  isTruth <- fmap isTruthy $ Right value
  if (boolean isTruth) then evaluateExpression s thenBranch
                       else evaluateExpression s elseBranch
evaluateExpression s (VariableAssignment ident expression location)
  | Map.member ident s  = do
    (rest, value) <- evaluateExpression s expression
    Right (Map.insert ident value s, value)
  | otherwise           = Left (s, ProgramError location "Variable not found!" [])

zeroState :: LoxState
zeroState = Map.empty

evaluateStatement :: LoxState -> Statement -> IO EvaluationResult
evaluateStatement state (PrintStatement _ expression) =
  case (evaluateExpression state expression) of Left e -> return $ Left e
                                                Right (s, v) -> putStrLn (show v) >> (return $ Right s)
evaluateStatement state (StatementExpression _ expression) =
  return $ evaluateStatementExpression (evaluateExpression state expression)
evaluateStatement state (VariableDeclaration _ ident Nothing) = return $ Right (Map.insert ident NilValue state)
evaluateStatement state (VariableDeclaration _ ident (Just expression)) =
  return $ fmap (uncurry (evaluateVariableDeclaration ident)) (evaluateExpression state expression)
evaluateStatement state (BlockStatement _ statements) = foldr processNext (return $ Right state) statements
  where processNext :: Statement -> IO EvaluationResult -> IO EvaluationResult
        processNext s e = evaluateStatement state s

evaluateStatementExpression :: EvaluationExpressionResult -> EvaluationResult
evaluateStatementExpression o = fmap (uncurry (flip seq)) o

evaluateVariableDeclaration :: String -> LoxState -> LoxValue -> LoxState
evaluateVariableDeclaration ident state value = value `seq` (Map.insert ident value state)