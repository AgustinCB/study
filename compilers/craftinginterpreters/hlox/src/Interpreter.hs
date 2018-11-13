module Interpreter where

import Control.Monad (liftM2)
import qualified Data.Map as Map
import Types

import Debug.Trace (trace)

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

mathOperation :: SourceCodeLocation -> Expression -> MathOperation -> Expression -> LoxState -> EvaluationExpressionResult
mathOperation location left op right s = do
  (_, rightOp) <- evaluateExpression s right >>= uncurry (expectNumber location)
  (_, leftOp) <- evaluateExpression s left >>= uncurry (expectNumber location)
  return $ (s, NumberValue ((number leftOp) `op` (number rightOp)))

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
evaluateExpression s (Binary left And right _) = do
  (ns, value) <- evaluateExpression s left
  isTruth <- fmap isTruthy $ Right value
  if (boolean isTruth) then evaluateExpression s right
                       else return $ (ns, value)
evaluateExpression s (Binary left Or right _) = do
  (ns, value) <- evaluateExpression s left
  isTruth <- fmap isTruthy $ Right value
  if (boolean isTruth) then return $ (ns, value)
                       else evaluateExpression s right
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
evaluateStatement state (IfStatement _ condition thenBranch (Just elseBranch)) =
  evaluateStatementAfterExpression state condition (\s -> \v -> if boolean $ (isTruthy v) then
                                                                  evaluateStatement s thenBranch
                                                                else
                                                                  evaluateStatement s elseBranch)
evaluateStatement state (IfStatement _ condition thenBranch Nothing) =
  evaluateStatementAfterExpression state condition (\s -> \v -> if boolean $ (isTruthy v) then
                                                                  evaluateStatement s thenBranch
                                                                else
                                                                  return $ Right s)
evaluateStatement state w@(WhileStatement _ condition body) =
  evaluateStatementAfterExpression state condition (\s -> \v -> if boolean $ (isTruthy v) then
                                                                  evaluateStatement s body >>= (\r ->
                                                                    case r of Right ns -> evaluateStatement ns w
                                                                              e -> return $ e)
                                                                else
                                                                  return $ Right s)
evaluateStatement state (BlockStatement _ statements) =
  fmap (\r -> fmap popScope r) (foldl processNext (return $ Right $ addScope state) statements)
  where processNext :: IO EvaluationResult -> Statement -> IO EvaluationResult
        processNext e s = e >>= \r -> case r of Right state -> evaluateStatement state s
                                                e -> return $ e

evaluateStatementExpression :: EvaluationExpressionResult -> EvaluationResult
evaluateStatementExpression o = fmap (uncurry (flip seq)) o

evaluateVariableDeclaration :: String -> LoxState -> LoxValue -> LoxState
evaluateVariableDeclaration ident state value = value `seq` (stateInsert ident value state)

evaluateStatementAfterExpression :: LoxState -> Expression -> (LoxState -> LoxValue -> IO EvaluationResult) -> IO EvaluationResult
evaluateStatementAfterExpression s e f = case (evaluateExpression s e) of Right (newState, value) -> f newState value
                                                                          Left e -> return $ Left e