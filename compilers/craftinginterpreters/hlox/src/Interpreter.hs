module Interpreter where

import Control.Monad (liftM2)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Types

import Debug.Trace (trace)

clock = FunctionValue 0 (\s -> \args -> (Right . (((,) s)) . NumberValue . fromIntegral . round . (* 1000)) <$> getPOSIXTime)

zeroState :: LoxState
zeroState = LoxState False Nothing $ Map.fromList [("clock", clock)]

stateLookup :: String -> LoxState -> Maybe LoxValue
stateLookup ident (LoxState _ Nothing state) = Map.lookup ident state
stateLookup ident (LoxState _ (Just parent) state) = case (Map.lookup ident state) of Just v -> Just v
                                                                                      Nothing -> stateLookup ident parent

stateMember :: String -> LoxState -> Bool
stateMember ident (LoxState _ Nothing state) = Map.member ident state
stateMember ident (LoxState _ (Just parent) state)
  | Map.member ident state = True
  | otherwise              = stateMember ident parent

stateInsert :: String -> LoxValue -> LoxState -> LoxState
stateInsert ident value (LoxState l parent state) = LoxState l parent $ Map.insert ident value state

stateReplace :: String -> LoxValue -> LoxState -> LoxState
stateReplace ident value (LoxState l Nothing state) = LoxState l Nothing $ Map.insert ident value state
stateReplace ident value (LoxState l (Just parent) state) =
  case (Map.lookup ident state) of Just v -> LoxState l (Just parent) $ Map.insert ident value state
                                   Nothing -> (LoxState l (Just (stateInsert ident value parent)) state)

addScope :: LoxState -> LoxState
addScope state = LoxState (brokeLoop state) (Just state) Map.empty

popScope :: LoxState -> LoxState
popScope (LoxState brokeLoop maybeParent _) = withBrokeLoop brokeLoop $ foldl (const id) zeroState maybeParent

withBrokeLoop :: Bool -> LoxState -> LoxState
withBrokeLoop b (LoxState _ p s) = LoxState b p s

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

mathOperation :: SourceCodeLocation -> Expression -> MathOperation -> Expression -> LoxState -> IO EvaluationExpressionResult
mathOperation location left op right s = liftM2 mathOperation' l r
  where mathOperation' :: EvaluationExpressionResult -> EvaluationExpressionResult -> EvaluationExpressionResult
        mathOperation' left right = do
          (_, rightOp) <- right
          (_, leftOp) <- left
          Right (s, NumberValue ((number leftOp) `op` (number rightOp)))
        l = fmap (\r -> r >>= uncurry (expectNumber location)) $ evaluateExpression s right
        r = fmap (\r -> r >>= uncurry (expectNumber location)) $ evaluateExpression s left

comparisonOperation :: SourceCodeLocation -> Expression -> BooleanOperation -> Expression -> LoxState -> IO EvaluationExpressionResult
comparisonOperation location left op right s = liftM2 comparisonOperation' l r
  where comparisonOperation' :: EvaluationExpressionResult -> EvaluationExpressionResult -> EvaluationExpressionResult
        comparisonOperation' left right = do
          (_, rightOp) <- right
          (_, leftOp) <- left
          Right (s, BooleanValue ((number leftOp) `op` (number rightOp)))
        r = fmap (\r -> r >>= uncurry (expectNumber location)) $ evaluateExpression s right
        l = fmap (\r -> r >>= uncurry (expectNumber location)) $ evaluateExpression s left

isEquals :: Expression -> Expression -> LoxState -> IO EvaluationExpressionResult
isEquals left right s = liftM2 (\l -> \r -> do
  (_, rightOp) <- r
  (_, leftOp) <- l
  return $ (s, BooleanValue (leftOp == rightOp))) (evaluateExpression s left) (evaluateExpression s right)

concatenateValues :: SourceCodeLocation -> Expression -> Expression -> LoxState -> IO EvaluationExpressionResult
concatenateValues location left right s = liftM2 concatenateValues' l r
  where concatenateValues' :: EvaluationExpressionResult -> EvaluationExpressionResult -> EvaluationExpressionResult
        concatenateValues' left right = do
          (_, rightOp) <- right
          (_, leftOp) <- left
          return $ (s, StringValue ((string leftOp) ++ (string rightOp)))
        r = fmap (\r -> r >>= uncurry (expectString location)) $ evaluateExpression s right
        l = fmap (\r -> r >>= uncurry (expectString location)) $ evaluateExpression s left

maybeToEvaluationExpressionResult :: SourceCodeLocation -> String -> LoxState -> Maybe LoxValue -> EvaluationExpressionResult
maybeToEvaluationExpressionResult l _ s (Just Uninitialized) = Left (s, ProgramError l "Variable not initialized!" [])
maybeToEvaluationExpressionResult _ _ s (Just v) = Right (s, v)
maybeToEvaluationExpressionResult l e s Nothing = Left (s, ProgramError l e [])

evaluateExpression :: LoxState -> Expression -> IO EvaluationExpressionResult
evaluateExpression s (ExpressionLiteral (KeywordLiteral NilKeyword) _) = return $ Right $ (s, NilValue)
evaluateExpression s (ExpressionLiteral (KeywordLiteral TrueKeyword) _) = return $ Right $ (s, BooleanValue True)
evaluateExpression s (ExpressionLiteral (KeywordLiteral FalseKeyword) _) = return $ Right $ (s, BooleanValue False)
evaluateExpression s (ExpressionLiteral (NumberLiteral v) _) = return $ Right $ (s, NumberValue v)
evaluateExpression s (ExpressionLiteral (StringLiteral string) _) = return $ Right $ (s, StringValue string)
evaluateExpression state (VariableLiteral ident l) = return $ maybeToEvaluationExpressionResult l "Variable not found!" state
                                                        $ stateLookup ident state
evaluateExpression s (Grouping expr _) = evaluateExpression s expr
evaluateExpression s (Unary Bang expr _) = fmap (\p -> do
    (ns, value) <- p
    return $ (ns, negateTruthy value)) $ evaluateExpression s expr
evaluateExpression s (Unary Minus expr location) = fmap (\r -> r >>= uncurry (negateDouble location)) $ evaluateExpression s expr
evaluateExpression s (Binary left Minus right location) = mathOperation location left (-) right s
evaluateExpression s (Binary left Star right location) = mathOperation location left (*) right s
evaluateExpression s (Binary left Slash right location) = do
  div <- mathOperation location left (/) right s
  let inf = 1/0
  case div of r@(Right (s, (NumberValue inf))) -> return $ Left (s, ProgramError location "Division by zero!" [])
              r -> return $ r
evaluateExpression s (Binary left Plus right location) = do
  sum <- mathOperation location left (+) right s
  case sum of r@(Right _) -> return r
              Left (s, ProgramError _ "Type error! Expecting a double!" _) -> concatenateValues location left right s
              e@(Left _) -> return e
evaluateExpression s (Binary left Greater right location) = comparisonOperation location left (>) right s
evaluateExpression s (Binary left GreaterEqual right location) = comparisonOperation location left (>=) right s
evaluateExpression s (Binary left Less right location) = comparisonOperation location left (<) right s
evaluateExpression s (Binary left LessEqual right location) = comparisonOperation location left (<=) right s
evaluateExpression s (Binary left EqualEqual right _) = isEquals left right s
evaluateExpression s (Binary left BangEqual right _) = fmap (\r -> do
  (ns, value) <- r
  return $ (ns, negateTruthy value)) $ isEquals left right s
evaluateExpression s (Binary left Comma right _) = liftM2 seq (evaluateExpression s left) (evaluateExpression s right)
evaluateExpression s (Binary left And right _) = (evaluateExpression s left) >>= (\l ->
  case l of Right (ns, value) -> if (boolean $ isTruthy value) then evaluateExpression ns right else return $ Right (ns, value)
            Left e -> return $ Left e)
evaluateExpression s (Binary left Or right _) = (evaluateExpression s left) >>= (\l ->
  case l of Right (ns, value) -> if (boolean $ isTruthy value) then return $ Right (ns, value) else evaluateExpression ns right
            Left e -> return $ Left e)
evaluateExpression s (Conditional condition thenBranch elseBranch location) = (evaluateExpression s condition) >>= (\l ->
  case l of Right (ns, value) -> if (boolean $ isTruthy value) then evaluateExpression ns thenBranch else evaluateExpression ns elseBranch
            Left e -> return $ Left e)
evaluateExpression s (VariableAssignment ident expression location)
  | stateMember ident s  = (evaluateExpression s expression) >>= (\v ->
    case v of Right (rest, value) -> return $ Right (stateReplace ident value s, value)
              Left e -> return $ Left e)
  | otherwise           = return $ Left (s, ProgramError location "Variable not found!" [])
evaluateExpression initialState (Call calleeExpression argumentExpressions l) =
  (evaluateExpression initialState calleeExpression) >>= (\r ->
    case r of e@(Right (s, callee)) -> fmap ((,) e) (evaluateMultipleExpressionsInSequence s argumentExpressions)) >>=
    (\p -> return $ liftM2 (,) (fst p) (snd p)) >>= (\p ->
    case p of Right ((s, callee), arguments) -> if length arguments == 0 then call l s callee (fmap snd arguments)
                                                else call l (fst (last arguments)) callee (fmap snd arguments)
              Left e -> return $ Left e)

call :: SourceCodeLocation -> LoxState -> LoxValue -> [LoxValue] -> IO EvaluationExpressionResult
call l s (FunctionValue arity f) args
  | arity /= length args    = return $ Left (s, ProgramError l ("Wrong number of arguments! Expected: " ++ (show arity) ++ " Got: " ++ (show $ length args)) [])
  | otherwise               = f s args
call l s _ _ = return $ Left (s, ProgramError l "Only functions or classes can be called!" [])

evaluateMultipleExpressionsInSequence :: LoxState -> [Expression] -> IO (Either (LoxState, (ProgramError Expression)) [(LoxState, LoxValue)])
evaluateMultipleExpressionsInSequence state [] = return $ Right []
evaluateMultipleExpressionsInSequence state (h:t) = (evaluateExpression state h) >>= (\r ->
  case r of Right (s,v) -> fmap (\r2 -> fmap ((:) (s,v)) r2) (evaluateMultipleExpressionsInSequence s t)
            Left e -> return $ Left e)

evaluateStatement :: LoxState -> Statement -> IO EvaluationResult
evaluateStatement state (PrintStatement _ expression) = ((evaluateExpression state expression) ) >>= (\r ->
  case r of Left e -> return $ Left e
            Right (s, v) -> putStrLn (show v) >> (return $ Right s))
evaluateStatement state (StatementExpression _ expression) =
  fmap evaluateStatementExpression (evaluateExpression state expression)
evaluateStatement state (VariableDeclaration _ ident Nothing) = return $ Right (stateInsert ident Uninitialized state)
evaluateStatement state (VariableDeclaration _ ident (Just expression)) =
  fmap (fmap (uncurry (evaluateVariableDeclaration ident))) (evaluateExpression state expression)
evaluateStatement state (FunctionDeclaration l (Token (Identifier ident) _ _) names body) =
  return $ Right (stateInsert ident (FunctionValue (length names) loxFunction) state)
  where loxFunction :: LoxState -> [LoxValue] -> IO EvaluationExpressionResult
        loxFunction initialState params =
          let functionScope = foldr (\p -> \s -> stateInsert (name (tokenType (snd p))) (fst p) s) initialState (zip params names)
          in fmap (\v -> fmap (\s -> (s, NilValue)) v) (evaluateStatement functionScope (BlockStatement l body))
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
evaluateStatement state (ReturnStatement l _) = return $ Left (state, ProgramError l "Unexpected return statement!" [])
evaluateStatement (LoxState _ p s) (BreakStatement _) = return $ Right (LoxState True p s)
evaluateStatement state w@(WhileStatement _ condition body) =
  evaluateStatementAfterExpression state condition evaluateWhileBody
  where evaluateWhileBody :: LoxState -> LoxValue -> IO EvaluationResult
        evaluateWhileBody s v = if boolean $ (isTruthy v)
                                    then (evaluateStatement s body) >>= (\r ->
                                        case r of Right (LoxState True p ns) -> return $ Right (LoxState False p ns)
                                                  Right ns -> evaluateStatement ns w
                                                  e -> return $ e)
                                else return $ Right s
evaluateStatement state (BlockStatement _ statements) =
  fmap (\r -> fmap popScope r) $ foldlTill processNext notLeftNorBrokeLoop (return $ Right $ addScope state) statements
  where notLeftNorBrokeLoop :: EvaluationResult -> Bool
        notLeftNorBrokeLoop (Left _) = False
        notLeftNorBrokeLoop (Right (LoxState brokeLoop _ _)) = not brokeLoop
        processNext :: IO EvaluationResult -> Statement -> IO EvaluationResult
        processNext e s = e >>= \r -> case r of Right state -> evaluateStatement state s
                                                e -> return $ e

foldlTill :: (Monad m, Show b) => (m b -> a -> m b) -> (b -> Bool) -> m b -> [a] -> m b
foldlTill f pred zero foldable = lgo zero foldable
  where lgo z []     =  z
        lgo z (x:xs) = do
          res <- f z x
          if pred res then lgo (return res) xs
                      else return res

evaluateStatementExpression :: EvaluationExpressionResult -> EvaluationResult
evaluateStatementExpression o = fmap (uncurry (flip seq)) o

evaluateVariableDeclaration :: String -> LoxState -> LoxValue -> LoxState
evaluateVariableDeclaration ident state value = value `seq` (stateInsert ident value state)

evaluateStatementAfterExpression :: LoxState -> Expression -> (LoxState -> LoxValue -> IO EvaluationResult) -> IO EvaluationResult
evaluateStatementAfterExpression s e f = (evaluateExpression s e) >>= (\r ->
  case r of Right (newState, value) -> f newState value
            Left e -> return $ Left e)