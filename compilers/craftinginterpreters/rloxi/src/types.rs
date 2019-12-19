use crate::interpreter::Evaluable;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{Debug, Display, Error, Formatter};
use std::ops::{Neg, Not};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceCodeLocation {
    pub file: String,
    pub line: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataKeyword {
    True,
    False,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    QuotedString(String),
    Keyword(DataKeyword),
    Number(f32),
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Class,
    Else,
    Fun,
    For,
    Break,
    If,
    Or,
    Print,
    Question,
    Return,
    Super,
    This,
    Var,
    While,
    Comment,
    EOF,
    Identifier { name: String },
    TokenLiteral { value: Literal },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub location: SourceCodeLocation,
}

#[derive(Debug, PartialEq)]
pub struct ProgramError {
    pub location: SourceCodeLocation,
    pub message: String,
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(
            format!(
                "There was an error! [line {}] Error: {}",
                self.location.line + 1,
                self.message
            )
            .as_str(),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub location: SourceCodeLocation,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionType {
    Conditional {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    Binary {
        right: Box<Expression>,
        operator: TokenType,
        left: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
    Unary {
        operator: TokenType,
        operand: Box<Expression>,
    },
    Grouping {
        expression: Box<Expression>,
    },
    ExpressionLiteral {
        value: Literal,
    },
    VariableLiteral {
        identifier: String,
    },
    VariableAssignment {
        identifier: String,
        expression: Box<Expression>,
    },
}

impl Expression {
    pub fn create_program_error(&self, message: &str) -> ProgramError {
        ProgramError {
            location: self.location.clone(),
            message: message.to_owned(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub location: SourceCodeLocation,
    pub statement_type: StatementType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType {
    Expression {
        expression: Expression,
    },
    PrintStatement {
        expression: Expression,
    },
    VariableDeclaration {
        expression: Option<Expression>,
        name: String,
    },
    FunctionDeclaration {
        name: String,
        arguments: Vec<String>,
        body: Vec<Box<Statement>>,
    },
    Block {
        body: Vec<Box<Statement>>,
    },
    If {
        condition: Expression,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        action: Box<Statement>,
    },
    Return {
        value: Option<Expression>,
    },
    Break,
    EOF,
}

pub type EvaluationResult = Result<(State, Value), ProgramError>;

#[derive(Clone, Debug, PartialEq)]
pub struct State {
    pub return_value: Option<Box<Value>>,
    pub broke_loop: bool,
    pub in_loop: bool,
    pub enclosing: Option<Box<State>>,
    pub in_function: bool,
    values: HashMap<String, Value>,
}

impl State {
    pub fn find(&self, identifier: &str) -> Option<&Value> {
        match &self.enclosing {
            Some(parent) => parent.find(identifier),
            None => self.values.get(identifier),
        }
    }

    pub fn insert(&mut self, identifier: String, value: Value) {
        match &mut self.enclosing {
            Some(parent) => parent.insert(identifier, value),
            None => {
                self.values.insert(identifier, value);
            }
        };
    }

    pub fn delete(&mut self, identifier: &str) {
        self.values.remove(identifier);
        if let Some(p) = &mut self.enclosing {
            p.delete(identifier);
        }
    }

    pub fn add_return_value(&mut self, v: Value) {
        self.return_value = Some(Box::new(v));
    }
}

impl Default for State {
    fn default() -> State {
        State {
            return_value: None,
            broke_loop: false,
            in_loop: false,
            enclosing: None,
            in_function: false,
            values: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxFunction {
    pub arguments: Vec<String>,
    pub environment: State,
    pub body: Vec<Statement>,
    pub location: SourceCodeLocation,
}

impl LoxFunction {
    pub fn eval(&self, parent_state: State, values: &[Value]) -> EvaluationResult {
        if self.arguments.len() != values.len() {
            return Err(ProgramError {
                message: format!(
                    "Wrong number of arguments: Received {}, expected {}",
                    values.len(),
                    self.arguments.len()
                ),
                location: self.location.clone(),
            });
        }
        let mut values_map = HashMap::default();
        for (name, value) in self.arguments.iter().cloned().zip(values.iter().cloned()) {
            values_map.insert(name, value);
        }
        let state = State {
            return_value: None,
            broke_loop: false,
            in_loop: false,
            enclosing: Some(Box::new(parent_state)),
            in_function: true,
            values: values_map,
        };
        let mut current_state = state;
        for st in self.body.iter() {
            current_state = st.evaluate(current_state)?.0;
            if let Some(return_value) = &current_state.return_value {
                let value = (**return_value).clone();
                return Ok((*current_state.enclosing.unwrap(), value));
            }
        }
        current_state.in_function = false;
        Ok((*current_state.enclosing.unwrap(), Value::Nil))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Uninitialized,
    Boolean { value: bool },
    Number { value: f32 },
    String { value: String },
    Function(LoxFunction),
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Value::Number { .. } => true,
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Uninitialized => false,
            Value::Boolean { value: false } => false,
            Value::Number { value } if *value == 0f32 => false,
            _ => true,
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Number { value } => Value::Number { value: -value },
            _ => panic!("Only numbers can change sign"),
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean { value } => Value::Boolean { value: !value },
            _ => Value::Boolean {
                value: !self.is_truthy(),
            },
        }
    }
}

pub enum ValueError {
    ExpectingDouble,
    ExpectingString,
}

impl ValueError {
    pub fn into_program_error(self, location: &SourceCodeLocation) -> ProgramError {
        ProgramError {
            location: location.clone(),
            message: self.to_string(),
        }
    }
}

impl ToString for ValueError {
    fn to_string(&self) -> String {
        match self {
            ValueError::ExpectingDouble => "Type error! Expecting a double!".to_owned(),
            ValueError::ExpectingString => "Type error! Expecting a string!".to_owned(),
        }
    }
}

impl TryInto<f32> for Value {
    type Error = ValueError;
    fn try_into(self) -> Result<f32, Self::Error> {
        match self {
            Value::Number { value } => Ok(value),
            _ => Err(ValueError::ExpectingDouble),
        }
    }
}

impl TryInto<String> for Value {
    type Error = ValueError;
    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Value::String { value } => Ok(value),
            _ => Err(ValueError::ExpectingString),
        }
    }
}

impl Into<Value> for &Literal {
    fn into(self) -> Value {
        match self {
            Literal::Number(value) => Value::Number { value: *value },
            Literal::QuotedString(value) => Value::String {
                value: value.clone(),
            },
            Literal::Keyword(DataKeyword::Nil) => Value::Nil,
            Literal::Keyword(DataKeyword::True) => Value::Boolean { value: true },
            Literal::Keyword(DataKeyword::False) => Value::Boolean { value: false },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Value::Number { value } => f.write_str(value.to_string().as_str()),
            Value::String { value } => f.write_str(value.as_str()),
            Value::Boolean { value } => f.write_str(value.to_string().as_str()),
            Value::Uninitialized => f.write_str("Uninitialized"),
            Value::Nil => f.write_str("Nil"),
            Value::Function(lf) => f.write_str(format!("{:?}", *lf).as_str()),
        }
    }
}
