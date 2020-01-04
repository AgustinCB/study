use crate::interpreter::Evaluable;
use crate::state::State;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Error, Formatter};
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
        static_methods: Vec<Box<Statement>>,
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
    methods: HashMap<String, LoxFunction>,
    pub name: String,
    pub static_instance: LoxObject,
}

impl LoxClass {
    pub fn new(name: String, static_method_list: &[&Statement], method_list: &[&Statement], environments: Vec<Rc<RefCell<HashMap<String, Value>>>>) -> LoxClass {
        let mut methods = HashMap::default();
        for ms in method_list {
            match &ms.statement_type {
                StatementType::FunctionDeclaration { arguments, body, name, } => {
                    methods.insert(name.clone(), LoxClass::function_declaration_to_lox_funxtion(arguments, body, &ms.location, &environments));
                }
                _ => panic!("Unexpected method"),
            }
        }
        let mut static_methods = vec![];
        for ms in static_method_list {
            match &ms.statement_type {
                StatementType::FunctionDeclaration { arguments, body, name, } => {
                    static_methods.push((name.clone(), LoxClass::function_declaration_to_lox_funxtion(arguments, body, &ms.location, &environments)));
                }
                _ => panic!("Unexpected method"),
            }
        }
        let static_instance = LoxObject::new_static(name.clone(), &static_methods);
        LoxClass {
            methods,
            name,
            static_instance,
        }
    }

    fn function_declaration_to_lox_funxtion(arguments: &[String], body: &[Box<Statement>], location: &SourceCodeLocation, environments: &[Rc<RefCell<HashMap<String, Value>>>]) -> LoxFunction {
        LoxFunction {
            arguments: arguments.to_vec(),
            body: body.iter().map(|s| (**s).clone()).collect(),
            environments: environments.to_vec(),
            location: location.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject {
    properties: Rc<RefCell<HashMap<String, Value>>>,
    pub class_name: String,
}

impl LoxObject {
    pub fn new(class: LoxClass) -> LoxObject {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        let object = LoxObject {
            properties: properties.clone(),
            class_name: class.name,
        };
        for (name, mut f) in class.methods {
            f.bind(object.clone());
            properties.borrow_mut().insert(name, Value::Function(f));
        }
        object
    }

    fn new_static(class_name: String, methods: &[(String, LoxFunction)]) -> LoxObject {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        for (name, function) in methods {
            properties.borrow_mut().insert(name.clone(), Value::Function(function.clone()));
        }
        LoxObject {
            class_name,
            properties
        }
    }

    pub fn init(
        &self,
        values: &[Value],
        locals: &HashMap<usize, usize>,
        location: &SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        if let Some(Value::Function(f)) = self.properties.borrow().get("init") {
            f.eval(values, locals)?;
            Ok(())
        } else if values.len() != 0 {
            Err(ProgramError {
                message: format!(
                    "Wrong number of arguments: Received {}, expected {}",
                    values.len(),
                    0,
                ),
                location: location.clone(),
            })
        } else {
            Ok(())
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

    fn bind(&mut self, instance: LoxObject) {
        let mut new_scope = HashMap::default();
        new_scope.insert("this".to_owned(), Value::Object(instance));
        self.environments.push(Rc::new(RefCell::new(new_scope)));
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
