use crate::interpreter::Evaluable;
use crate::state::State;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{Debug, Display, Error, Formatter};
use std::ops::{Neg, Not};
use std::rc::Rc;

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
    id: usize,
}

impl Expression {
    pub fn id(&self) -> usize {
        self.id
    }
}

pub struct ExpressionFactory {
    counter: usize,
}

impl ExpressionFactory {
    pub fn new() -> ExpressionFactory {
        ExpressionFactory { counter: 0 }
    }
    pub fn new_starting(counter: usize) -> ExpressionFactory {
        ExpressionFactory { counter }
    }
    pub fn new_expression(
        &mut self,
        expression_type: ExpressionType,
        location: SourceCodeLocation,
    ) -> Expression {
        let result = Expression {
            expression_type,
            location,
            id: self.counter,
        };
        self.counter += 1;
        result
    }
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
    AnonymousFunction {
        arguments: Vec<String>,
        body: Vec<Statement>,
    },
    Get {
        callee: Box<Expression>,
        property: String,
    },
    Set {
        callee: Box<Expression>,
        property: String,
        value: Box<Expression>,
    }
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
    Class {
        name: String,
        methods: Vec<Box<Statement>>,
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
pub struct LoxClass {
    pub methods: HashMap<String, LoxFunction>,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject {
    properties: Rc<RefCell<HashMap<String, Value>>>,
    class_name: String,
}

impl LoxObject {
    pub fn new(class: LoxClass) -> LoxObject {
        let mut properties = HashMap::default();
        for (name, f) in class.methods {
            properties.insert(name, Value::Function(f));
        }
        LoxObject {
            properties: Rc::new(RefCell::new(properties)),
            class_name: class.name,
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.properties.borrow().get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.properties.borrow_mut().insert(name, value);
    }
}

#[derive(Clone, PartialEq)]
pub struct LoxFunction {
    pub arguments: Vec<String>,
    pub environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    pub body: Vec<Statement>,
    pub location: SourceCodeLocation,
}

impl LoxFunction {
    pub fn eval(
        &self,
        values: &[Value],
        locals: &HashMap<usize, usize>,
    ) -> Result<Value, ProgramError> {
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
        let mut current_state = State::new(&self.environments);
        current_state.push();
        for (name, value) in self.arguments.iter().cloned().zip(values.iter().cloned()) {
            current_state.insert_top(name, value);
        }
        current_state.in_function = true;
        for st in self.body.iter() {
            current_state = st.evaluate(current_state, locals)?.0;
            if let Some(return_value) = &current_state.return_value {
                let value = (**return_value).clone();
                return Ok(value);
            }
        }
        Ok(Value::Nil)
    }
}

impl Debug for LoxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(
            format!(
                "[Function: Arguments {:?} Body {:?} Location {:?} Env Size {:?}",
                self.arguments,
                self.body,
                self.location,
                self.environments.len()
            )
            .as_str(),
        )
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
    Class(LoxClass),
    Object(LoxObject),
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
            Value::Class(c) => f.write_str(format!("{}", c.name).as_str()),
            Value::Object(c) => f.write_str(format!("{} instance", c.class_name).as_str()),
        }
    }
}
