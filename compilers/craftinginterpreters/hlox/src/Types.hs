module Types where

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

type ParsingStatementStep = ([Token], Statement)
type ParsingStatementResult = Either (ProgramError Token) ParsingStatementStep
type StatementParser = [Token] -> ParsingStatementResult

type ParsingExpressionStep = ([Token], Expression)
type ParsingExpressionResult = Either (ProgramError Token) ParsingExpressionStep
type ExpressionParser = [Token] -> ParsingExpressionResult