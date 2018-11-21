module Parser where

import Control.Monad (liftM4)
import Types

import Debug.Trace (trace)

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
                     in case h of Right p -> ((Right p):(blockStatements (fst p)))
                                  Left e -> (Left e):(blockStatements (rest e))

createForBlock :: Statement -> Statement -> Statement -> Statement -> Statement
createForBlock init (StatementExpression _ e) incr body = BlockStatement l [init, whileStatement]
  where l = statementLocation init
        whileStatement = WhileStatement l e blockStatement
        blockStatement = BlockStatement (statementLocation body) [body, incr]

parseStatement :: StatementParser
parseStatement [] = Left $ ProgramError (SourceCodeLocation Nothing 1) "No input!" []
parseStatement ((Token If _ l):list) = do
  rest1 <- consume LeftParen "Expected '(' after if token" list
  (rest2, condition) <- parseExpression rest1
  rest3 <- consume RightParen "Expected ')' after if condition" rest2
  (rest4, thenStatement) <- parseStatement rest3
  case rest4 of ((Token Else _ _):rest5) -> do
                                              (rest6, elseStatement) <- parseStatement rest5
                                              return $ (rest6, IfStatement l condition thenStatement (Just elseStatement))
                rest -> return $ (rest, IfStatement l condition thenStatement Nothing)
parseStatement ((Token For _ l):list) =
  let rest1 = consume LeftParen "Expected '(' after if token" list
      tempInit = case rest1 of Right ((Token Semicolon _ l):r) ->
                                 Right (r, StatementExpression l $ ExpressionLiteral (KeywordLiteral NilKeyword) l)
                               Right r -> parseStatement r
                               Left e -> Left e
      init = case tempInit of Right (r, s@(VariableDeclaration _ _ _)) -> Right $ (r, s)
                              Right (r, s@(StatementExpression _ _)) -> Right $ (r, s)
                              Right _ -> Left $ ProgramError l "Invalid statement for initialization!" []
                              Left e -> Left e
      cond = readStatementExpressionFollowedByToken Semicolon init
      incr = readStatementExpressionFollowedByToken RightParen cond
      body = incr >>= \p -> parseStatement (fst p)
      forInfo = liftM4 (\a -> \b -> \c -> \d -> (snd a, snd b, snd c, snd d, fst d)) init cond incr body
  in case forInfo of Right (initialize, condition, increment, body, rest) ->
                        Right (rest, createForBlock initialize condition increment body)
                     Left e -> Left e
  where readStatementExpressionFollowedByToken :: TokenType -> ParsingStatementResult -> ParsingStatementResult
        readStatementExpressionFollowedByToken _ (Left e) = Left e
        readStatementExpressionFollowedByToken expected (Right (all@((Token t _ l):rest), statement))
          | expected == t   = Right (rest, StatementExpression l (ExpressionLiteral (KeywordLiteral NilKeyword) l))
          | otherwise       = parseExpression all
                                >>= \p -> Right (fst p, StatementExpression l (snd p))
                                >>= \p -> (fmap (\r -> (r, (snd p))) (consume expected ("Expected '" ++ (show expected) ++ "'") (fst p)))
parseStatement ((Token While _ l):list) = do
  rest1 <- consume LeftParen "Expected '(' after if token" list
  (rest2, condition) <- parseExpression rest1
  rest3 <- consume RightParen "Expected ')' after if condition" rest2
  (rest4, statement) <- parseStatement rest3
  return $ (rest4, WhileStatement l condition statement)
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
parseTernary tokens = parseOr tokens >>= parseTernaryOperator
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

parseOr :: ExpressionParser
parseOr list = (parseAnd list) >>= uncurry (concatenate [Or] parseOr)

parseAnd :: ExpressionParser
parseAnd list = (parseEquality list) >>= uncurry (concatenate [And] parseAnd)

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
  | otherwise                                               = Left $ ProgramError headLocation
                                                                        ("Expecting a literal, but got " ++ (show head) ++ "!") r

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