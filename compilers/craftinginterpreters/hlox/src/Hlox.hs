{-# LANGUAGE ScopedTypeVariables #-}
module Hlox (scanTokens, evaluateStatement, parseStatement, Statement, ScanningResult, LoxState,
             ParsingExpressionResult, Token, ParsingExpressionStep, ProgramError, zeroState) where

import Control.Monad (liftM2)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Lexer
import Types

-- Language specs

isToken :: TokenType -> Token -> Bool
isToken needle token = (tokenType token) == needle

createStatementFromExpression :: (Expression -> Statement) -> [Token] -> ParsingStatementResult
createStatementFromExpression f list = do
  (rest, expression) <- parseExpression list
  newRest <- consume Semicolon "Expected semicolon" rest
  return (newRest, f expression)

blockStatements :: [Token] -> [ParsingStatementResult]
blockStatements [] = []
blockStatements ((Token RightBrace _ _):r) = []
blockStatements ts = let h = parseStatement ts
                     in case h of Right p -> ((Right p) : (blockStatements (fst p)))
                                  Left e -> (Left e):(blockStatements (rest e))

parseStatement :: StatementParser
parseStatement [] = Left $ ProgramError (SourceCodeLocation Nothing 1) "No input!" []
parseStatement ((Token Print _ l):list) = createStatementFromExpression (PrintStatement l) list
parseStatement ((Token Var _ l):(Token (Identifier ident) _ _):(Token Semicolon _ _):list) =
  Right $ (list, VariableDeclaration l ident Nothing)
parseStatement ((Token Var _ l):(Token (Identifier ident) _ _):(Token Equal _ _):list) =
  createStatementFromExpression ((VariableDeclaration l ident) . Just) list
parseStatement ((Token Var _ l):list) = Left $ ProgramError l "Invalid variable declaration!" []
parseStatement ((Token LeftBrace _ l):[]) = Left $ ProgramError l "Expected '}' after block" []
parseStatement ((Token LeftBrace _ l):(Token RightBrace _ _):r) = Right $ (r, BlockStatement l [])
parseStatement ((Token LeftBrace _ l):list) = do
  parseResults <- sequence $ blockStatements list
  let (rest, statements) = case parseResults of [] -> (list, [])
                                                l -> (fst $ (last l), map snd l)
  newRest <- consume RightBrace "Expected '}' after block" rest
  return (newRest, BlockStatement l statements)
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
              | Uninitialized
              | BooleanValue { boolean :: Bool }
              | NumberValue { number :: Double }
              | StringValue { string :: String } deriving (Eq)
type EvaluationExpressionResult = Either (LoxState, (ProgramError Expression)) (LoxState, LoxValue)
type EvaluationResult = Either (LoxState, (ProgramError Expression)) LoxState
data LoxState = LoxState { enclosing :: Maybe LoxState, values :: Map.Map String LoxValue }

zeroState :: LoxState
zeroState = LoxState Nothing Map.empty

stateLookup :: String -> LoxState -> Maybe LoxValue
stateLookup ident (LoxState Nothing state) = Map.lookup ident state
stateLookup ident (LoxState (Just parent) state) = case (Map.lookup ident state) of Just v -> Just v
                                                                                    Nothing -> stateLookup ident parent

stateMember :: String -> LoxState -> Bool
stateMember ident (LoxState Nothing state) = Map.member ident state
stateMember ident (LoxState (Just parent) state)
  | Map.member ident state = True
  | otherwise              = stateMember ident parent

stateInsert :: String -> LoxValue -> LoxState -> LoxState
stateInsert ident value (LoxState parent state) = LoxState parent $ Map.insert ident value state

addScope :: LoxState -> LoxState
addScope state = LoxState (Just state) Map.empty

popScope :: LoxState -> LoxState
popScope (LoxState maybeParent _) = foldl (const id) zeroState maybeParent

instance Show LoxValue where
  show NilValue = "nil"
  show Uninitialized = "uninitialized"
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

maybeToEvaluationExpressionResult :: SourceCodeLocation -> String -> LoxState -> Maybe LoxValue -> EvaluationExpressionResult
maybeToEvaluationExpressionResult l _ s (Just Uninitialized) = Left (s, ProgramError l "Variable not initialized!" [])
maybeToEvaluationExpressionResult _ _ s (Just v) = Right (s, v)
maybeToEvaluationExpressionResult l e s Nothing = Left (s, ProgramError l e [])

evaluateExpression :: LoxState -> Expression -> EvaluationExpressionResult
evaluateExpression s (ExpressionLiteral (KeywordLiteral NilKeyword) _) = Right $ (s, NilValue)
evaluateExpression s (ExpressionLiteral (KeywordLiteral TrueKeyword) _) = Right $ (s, BooleanValue True)
evaluateExpression s (ExpressionLiteral (KeywordLiteral FalseKeyword) _) = Right $ (s, BooleanValue False)
evaluateExpression s (ExpressionLiteral (NumberLiteral v) _) = Right $ (s, NumberValue v)
evaluateExpression s (ExpressionLiteral (StringLiteral string) _) = Right $ (s, StringValue string)
evaluateExpression state (VariableLiteral ident l) = maybeToEvaluationExpressionResult l "Variable not found!" state
                                                        $ stateLookup ident state
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
  | stateMember ident s  = do
    (rest, value) <- evaluateExpression s expression
    Right (stateInsert ident value s, value)
  | otherwise           = Left (s, ProgramError location "Variable not found!" [])

evaluateStatement :: LoxState -> Statement -> IO EvaluationResult
evaluateStatement state (PrintStatement _ expression) =
  case (evaluateExpression state expression) of Left e -> return $ Left e
                                                Right (s, v) -> putStrLn (show v) >> (return $ Right s)
evaluateStatement state (StatementExpression _ expression) =
  return $ evaluateStatementExpression (evaluateExpression state expression)
evaluateStatement state (VariableDeclaration _ ident Nothing) = return $ Right (stateInsert ident Uninitialized state)
evaluateStatement state (VariableDeclaration _ ident (Just expression)) =
  return $ fmap (uncurry (evaluateVariableDeclaration ident)) (evaluateExpression state expression)
evaluateStatement state (BlockStatement _ statements) =
  fmap (\r -> fmap popScope r) (foldl processNext (return $ Right $ addScope state) statements)
  where processNext :: IO EvaluationResult -> Statement -> IO EvaluationResult
        processNext e s = e >>= \r -> case r of Right state -> evaluateStatement state s
                                                e -> return $ e

evaluateStatementExpression :: EvaluationExpressionResult -> EvaluationResult
evaluateStatementExpression o = fmap (uncurry (flip seq)) o

evaluateVariableDeclaration :: String -> LoxState -> LoxValue -> LoxState
evaluateVariableDeclaration ident state value = value `seq` (stateInsert ident value state)