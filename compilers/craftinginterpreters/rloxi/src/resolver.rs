use crate::interpreter::Interpreter;
use crate::types::{
    Expression, ExpressionType, ProgramError, SourceCodeLocation, Statement, StatementType,
};
use std::collections::HashMap;

pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'a mut Interpreter,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Resolver {
            interpreter,
            scopes: vec![HashMap::default()],
        }
    }
    fn push_scope(&mut self, scope: HashMap<String, bool>) {
        self.scopes.push(scope);
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, name: &str, location: &SourceCodeLocation) -> Result<(), ProgramError> {
        if let Some(s) = self.scopes.last_mut() {
            if s.contains_key(name) {
                return Err(ProgramError {
                    message: format!("Variable `{}` already declared in this scope!", name),
                    location: location.clone(),
                });
            }
            s.insert(name.to_owned(), false);
        }
        Ok(())
    }
    fn define(&mut self, name: &str) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.to_owned(), true);
        }
    }
    fn resolve_local(&mut self, expression: &Expression, name: &str) {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find(|(_, s)| s.contains_key(name))
        {
            self.interpreter.resolve_variable(expression, i);
        }
    }
    fn resolve_function(
        &mut self,
        arguments: &[String],
        body: &[Box<Statement>],
        location: &SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        self.push_scope(HashMap::default());
        for arg in arguments {
            self.declare(arg, location)?;
            self.define(arg);
        }
        body.iter()
            .map(|s| self.resolve(s))
            .collect::<Result<Vec<()>, ProgramError>>()?;
        self.pop_scope();
        Ok(())
    }
}

pub trait Pass {
    fn run(&mut self) -> Result<(), ProgramError>;
    fn resolve(&mut self, statement: &Statement) -> Result<(), ProgramError>;
    fn resolve_expression(&mut self, expression: &Expression) -> Result<(), ProgramError>;
}

impl<'a> Pass for Resolver<'a> {
    fn run(&mut self) -> Result<(), ProgramError> {
        let ss = self.interpreter.content().to_vec();
        ss.iter()
            .map(|s| self.resolve(&s))
            .collect::<Result<Vec<()>, ProgramError>>()?;
        Ok(())
    }
    fn resolve(&mut self, statement: &Statement) -> Result<(), ProgramError> {
        match &statement.statement_type {
            StatementType::Block { body } => {
                self.push_scope(HashMap::default());
                body.iter()
                    .map(|s| self.resolve(&s))
                    .collect::<Result<Vec<()>, ProgramError>>()?;
                self.pop_scope();
            }
            StatementType::VariableDeclaration { expression, name } => {
                self.declare(&name, &statement.location)?;
                if let Some(e) = expression {
                    self.resolve_expression(e)?;
                }
                self.define(&name);
            }
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            } => {
                self.declare(name, &statement.location)?;
                self.define(name);
                self.resolve_function(arguments, body, &statement.location)?;
            }
            StatementType::Expression { expression } => {
                self.resolve_expression(expression)?;
            }
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                self.resolve_expression(condition)?;
                self.resolve(then)?;
                if let Some(o) = otherwise {
                    self.resolve(o)?;
                }
            }
            StatementType::PrintStatement { expression } => {
                self.resolve_expression(expression)?;
            }
            StatementType::Return { value: Some(e) } => {
                self.resolve_expression(e)?;
            }
            StatementType::Return { .. } => {}
            StatementType::While { condition, action } => {
                self.resolve_expression(condition)?;
                self.resolve(action)?;
            }
            StatementType::Break => {}
            StatementType::EOF => {}
        };
        Ok(())
    }

    fn resolve_expression(&mut self, expression: &Expression) -> Result<(), ProgramError> {
        match &expression.expression_type {
            ExpressionType::VariableLiteral { identifier } => {
                self.resolve_local(expression, identifier);
            }
            ExpressionType::VariableAssignment {
                identifier,
                expression: expression_value,
            } => {
                self.resolve_expression(expression_value)?;
                self.resolve_local(expression, identifier);
            }
            ExpressionType::Binary { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            ExpressionType::Call { callee, arguments } => {
                self.resolve_expression(callee)?;
                arguments
                    .iter()
                    .map(|a| self.resolve_expression(a))
                    .collect::<Result<Vec<()>, ProgramError>>()?;
            }
            ExpressionType::Grouping { expression } => {
                self.resolve_expression(expression)?;
            }
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_expression(then_branch)?;
                self.resolve_expression(else_branch)?;
            }
            ExpressionType::Unary { operand, .. } => {
                self.resolve_expression(operand)?;
            }
            ExpressionType::ExpressionLiteral { .. } => {}
        };
        Ok(())
    }
}
