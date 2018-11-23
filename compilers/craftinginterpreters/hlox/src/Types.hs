module Types where

import qualified Data.Map as Map

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
    Break |
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
    BlockStatement SourceCodeLocation [Statement] |
    IfStatement SourceCodeLocation Expression Statement (Maybe Statement) |
    WhileStatement SourceCodeLocation Expression Statement |
    BreakStatement SourceCodeLocation deriving Show

statementLocation :: Statement -> SourceCodeLocation
statementLocation (StatementExpression l _) = l
statementLocation (PrintStatement l _) = l
statementLocation (VariableDeclaration l _ _) = l
statementLocation (BlockStatement l _) = l
statementLocation (IfStatement l _ _ _) = l
statementLocation (WhileStatement l _ _) = l
statementLocation (BreakStatement l) = l

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

type ParsingStatementStep = ([Token], Statement)
type ParsingStatementResult = Either (ProgramError Token) ParsingStatementStep
type StatementParser = [Token] -> Bool -> ParsingStatementResult

type ParsingExpressionStep = ([Token], Expression)
type ParsingExpressionResult = Either (ProgramError Token) ParsingExpressionStep
type ExpressionParser = [Token] -> ParsingExpressionResult

data LoxValue = NilValue
              | Uninitialized
              | BooleanValue { boolean :: Bool }
              | NumberValue { number :: Double }
              | StringValue { string :: String } deriving (Eq)
type EvaluationExpressionResult = Either (LoxState, (ProgramError Expression)) (LoxState, LoxValue)
type EvaluationResult = Either (LoxState, (ProgramError Expression)) LoxState
data LoxState = LoxState { enclosing :: Maybe LoxState, values :: Map.Map String LoxValue } deriving Show

type MathOperation = Double -> Double -> Double
type BooleanOperation = Double -> Double -> Bool

instance Show LoxValue where
  show NilValue = "nil"
  show Uninitialized = "uninitialized"
  show (BooleanValue b) = show b
  show (NumberValue n) = show n
  show (StringValue s) = s