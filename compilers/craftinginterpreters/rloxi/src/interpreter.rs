use crate::types::{
    EvaluationResult, Expression, ExpressionType, LoxFunction, ProgramError, SourceCodeLocation,
    State, Statement, StatementType, TokenType, Value, ValueError,
};
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Sub};

fn div_expressions(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(s)?;
    match right_value {
        Value::Number { value } if value == 0f32 => {
            Err(right.create_program_error("Division by zero!"))
        }
        _ => Ok(()),
    }?;
    math_operation(left_value, right_value, f32::div)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(&location))
}

fn add_expressions(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    if left_value.is_number() {
        let (final_state, right_value) = right.evaluate(s)?;
        math_operation(left_value, right_value, f32::add)
            .map(|v| (final_state, v))
            .map_err(|e| e.into_program_error(location))
    } else {
        let left_string: String = left_value
            .try_into()
            .map_err(|e: ValueError| e.into_program_error(location))?;
        let (final_state, right_value) = right.evaluate(s)?;
        let right_string: String = right_value
            .try_into()
            .map_err(|e: ValueError| e.into_program_error(location))?;
        Ok((
            final_state,
            Value::String {
                value: format!("{}{}", left_string, right_string),
            },
        ))
    }
}

fn operation<R>(l: Value, r: Value, op: fn(f32, f32) -> R) -> Result<R, ValueError> {
    let l_number = l.try_into()?;
    let r_number = r.try_into()?;
    Ok(op(l_number, r_number))
}

fn math_operation(l: Value, r: Value, op: fn(f32, f32) -> f32) -> Result<Value, ValueError> {
    Ok(Value::Number {
        value: operation(l, r, op)?,
    })
}

fn comparison_operation(l: Value, r: Value, op: fn(f32, f32) -> bool) -> Result<Value, ValueError> {
    Ok(Value::Boolean {
        value: operation(l, r, op)?,
    })
}

fn value_math_operation(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    op: fn(f32, f32) -> f32,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(s)?;
    math_operation(left_value, right_value, op)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(location))
}

fn value_comparison_operation(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    op: fn(f32, f32) -> bool,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(s)?;
    comparison_operation(left_value, right_value, op)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(location))
}

fn eq_expressions(state: State, left: &Expression, right: &Expression) -> EvaluationResult {
    let (next_state, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(next_state)?;
    Ok((
        final_state,
        Value::Boolean {
            value: left_value == right_value,
        },
    ))
}

fn conditional_expression(
    state: State,
    condition: &Expression,
    then_branch: &Expression,
    else_branch: &Expression,
) -> EvaluationResult {
    let (s, condition) = condition.evaluate(state)?;
    if condition.is_truthy() {
        then_branch
    } else {
        else_branch
    }
    .evaluate(s)
}

fn boolean_expression(
    state: State,
    left: &Expression,
    right: &Expression,
    op: fn(Value, Value) -> Value,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(s)?;
    Ok((final_state, op(left_value, right_value)))
}

fn variable_assignment(
    state: State,
    name: &str,
    expression: &Expression,
    location: &SourceCodeLocation,
) -> EvaluationResult {
    match state.find(name) {
        Some(_) => {
            let (mut s, value) = expression.evaluate(state)?;
            s.insert(name.to_owned(), value.clone());
            Ok((s, value))
        }
        None => Err(ProgramError {
            location: location.clone(),
            message: format!("Variable `{}` not found!", name),
        }),
    }
}

fn call_expression(
    state: State,
    callee: &Expression,
    arguments: &[Box<Expression>],
) -> EvaluationResult {
    let (next_state, function_value) = callee.evaluate(state)?;
    match function_value {
        Value::Function(f) if f.arguments.len() != arguments.len() => Err(callee
            .create_program_error(
                format!(
                    "Wrong number of arguments! Expected: {} Got: {}",
                    f.arguments.len(),
                    arguments.len()
                )
                .as_str(),
            )),
        Value::Function(f) => {
            // TODO: Use environment to have closures!!!
            let mut values = vec![];
            let mut current_state = next_state;
            for e in arguments {
                let (value_status, value) = e.evaluate(current_state)?;
                current_state = value_status;
                values.push(value);
            }
            f.eval(current_state, &values)
        }
        _ => Err(callee.create_program_error("Only functions or classes can be called!")),
    }
}

pub struct Interpreter {
    content: Vec<Statement>,
}

impl Interpreter {
    pub fn new(content: Vec<Statement>) -> Interpreter {
        Interpreter { content }
    }

    pub fn run(&self) -> Result<(), ProgramError> {
        let mut current_state = State::default();
        for s in self.content.iter() {
            match s.evaluate(current_state) {
                Ok((next_state, _)) => {
                    current_state = next_state;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
}

pub trait Evaluable {
    fn evaluate(&self, state: State) -> EvaluationResult;
}

impl Evaluable for Expression {
    fn evaluate(&self, state: State) -> EvaluationResult {
        match &self.expression_type {
            ExpressionType::ExpressionLiteral { value } => Ok((state, value.into())),
            ExpressionType::VariableLiteral { identifier } => {
                let value = state
                    .find(identifier)
                    .ok_or_else(|| {
                        self.create_program_error(&format!("Variable `{}` not found!", identifier))
                    })?
                    .clone();
                if value == Value::Uninitialized {
                    Err(self.create_program_error(&format!(
                        "Variable `{}` not initialized!",
                        identifier
                    )))
                } else {
                    Ok((state, value))
                }
            }
            ExpressionType::Grouping { expression } => expression.evaluate(state),
            ExpressionType::Unary {
                operand,
                operator: TokenType::Minus,
            } => {
                let (s, v) = operand.evaluate(state)?;
                if v.is_number() {
                    Ok((s, -v))
                } else {
                    Err(self.create_program_error("Can only negate numbers"))
                }
            }
            ExpressionType::Unary {
                operand,
                operator: TokenType::Bang,
            } => operand.evaluate(state).map(|(s, v)| (s, !v)),
            ExpressionType::Unary { .. } => {
                Err(self.create_program_error("Invalid unary operator"))
            }
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Plus,
            } => add_expressions(state, left, right, &self.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Minus,
            } => value_math_operation(state, left, right, &self.location, f32::sub),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Slash,
            } => div_expressions(state, left, right, &self.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Star,
            } => value_math_operation(state, left, right, &self.location, f32::mul),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Greater,
            } => value_comparison_operation(state, left, right, &self.location, |f1, f2| {
                f32::gt(&f1, &f2)
            }),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::GreaterEqual,
            } => value_comparison_operation(state, left, right, &self.location, |f1, f2| {
                f32::ge(&f1, &f2)
            }),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Less,
            } => value_comparison_operation(state, left, right, &self.location, |f1, f2| {
                f32::lt(&f1, &f2)
            }),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::LessEqual,
            } => value_comparison_operation(state, left, right, &self.location, |f1, f2| {
                f32::le(&f1, &f2)
            }),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::EqualEqual,
            } => eq_expressions(state, left, right),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::BangEqual,
            } => eq_expressions(state, left, right).map(|(s, v)| (s, !v)),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Comma,
            } => left.evaluate(state).and_then(|(s, _)| right.evaluate(s)),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::And,
            } => boolean_expression(state, left, right, |left_value, right_value| {
                if left_value.is_truthy() {
                    right_value
                } else {
                    left_value
                }
            }),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Or,
            } => boolean_expression(state, left, right, |left_value, right_value| {
                if left_value.is_truthy() {
                    left_value
                } else {
                    right_value
                }
            }),
            ExpressionType::Binary { .. } => {
                Err(self.create_program_error("Invalid binary operator"))
            }
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => conditional_expression(state, condition, then_branch, else_branch),
            ExpressionType::VariableAssignment {
                expression,
                identifier,
            } => variable_assignment(state, identifier, expression, &self.location),
            ExpressionType::Call { callee, arguments } => call_expression(state, callee, arguments),
        }
    }
}

impl Evaluable for Statement {
    fn evaluate(&self, mut state: State) -> Result<(State, Value), ProgramError> {
        let state = match &self.statement_type {
            StatementType::EOF => state,
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                let (s, cond_value) = condition.evaluate(state)?;
                if cond_value.is_truthy() {
                    then.evaluate(s)?.0
                } else if let Some(o) = otherwise {
                    o.evaluate(s)?.0
                } else {
                    s
                }
            }
            StatementType::Expression { expression } => expression.evaluate(state)?.0,
            StatementType::Block { body } => {
                let mut current_state = state.push();
                for st in body {
                    let (s, _) = st.evaluate(current_state)?;
                    current_state = s;
                    if current_state.broke_loop {
                        break;
                    }
                }
                let mut final_state = current_state.clone().get_parent().unwrap();
                final_state.broke_loop = current_state.broke_loop;
                final_state
            }
            StatementType::VariableDeclaration { expression, name } => {
                let (mut s, v) = if let Some(e) = expression {
                    e.evaluate(state)?
                } else {
                    (state, Value::Uninitialized)
                };
                s.insert_top(name.clone(), v);
                s
            }
            StatementType::PrintStatement { expression } => {
                let (s, v) = expression.evaluate(state)?;
                println!("{}", v);
                s
            }
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            } => {
                state.insert(
                    name.clone(),
                    Value::Function(LoxFunction {
                        arguments: arguments.clone(),
                        environment: state.clone(),
                        body: body.iter().map(|s| (**s).clone()).collect(),
                        location: self.location.clone(),
                    }),
                );
                state
            }
            StatementType::Return { value } if state.in_function => match value {
                None => state,
                Some(e) => {
                    let (mut s, v) = e.evaluate(state)?;
                    s.add_return_value(v);
                    s
                }
            },
            StatementType::Return { .. } => {
                return Err(ProgramError {
                    location: self.location.clone(),
                    message: "Return outside function".to_owned(),
                })
            }
            StatementType::While { condition, action } => {
                let mut current_state = state;
                current_state.in_loop = true;
                while {
                    let (s, v) = condition.evaluate(current_state)?;
                    current_state = s;
                    current_state.in_loop && v.is_truthy()
                } {
                    let (s, _) = action.evaluate(current_state)?;
                    current_state = s;
                    if current_state.broke_loop {
                        break;
                    }
                }
                current_state.broke_loop = false;
                current_state
            }
            StatementType::Break if state.in_loop => {
                state.in_loop = false;
                state.broke_loop = true;
                state
            }
            StatementType::Break => {
                return Err(ProgramError {
                    location: self.location.clone(),
                    message: "Break outside loop".to_owned(),
                })
            }
        };
        Ok((state, Value::Nil))
    }
}

#[cfg(test)]
mod test_statement {
    use crate::interpreter::Evaluable;
    use crate::types::{
        Expression, ExpressionType, Literal, LoxFunction, ProgramError, SourceCodeLocation, State,
        Statement, StatementType, TokenType, Value,
    };

    #[test]
    fn test_if_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::If {
                condition: create_expression_number(1.0, &location),
                then: Box::new(create_variable_assignment_statement(
                    "identifier",
                    1.0,
                    &location,
                )),
                otherwise: Some(Box::new(create_variable_assignment_statement(
                    "identifier",
                    0.0,
                    &location,
                ))),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 1.0 })
        );
    }

    #[test]
    fn test_if_statement_else() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::If {
                condition: create_expression_number(0.0, &location),
                then: Box::new(create_variable_assignment_statement(
                    "identifier",
                    1.0,
                    &location,
                )),
                otherwise: Some(Box::new(create_variable_assignment_statement(
                    "identifier",
                    0.0,
                    &location,
                ))),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 0.0 })
        );
    }

    #[test]
    fn test_expression_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = create_variable_assignment_statement("identifier", 0.0, &location);
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 0.0 })
        );
    }

    #[test]
    fn test_block_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };

        let statement = Statement {
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(create_variable_assignment_statement(
                        "identifier",
                        0.0,
                        &location,
                    )),
                    Box::new(create_variable_assignment_statement(
                        "identifier1",
                        1.0,
                        &location,
                    )),
                    Box::new(create_variable_assignment_statement(
                        "identifier2",
                        2.0,
                        &location,
                    )),
                ],
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        state.insert("identifier1".to_owned(), Value::Number { value: 2.0 });
        state.insert("identifier2".to_owned(), Value::Number { value: 0.0 });
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 0.0 })
        );
        assert_eq!(
            s.find("identifier1").cloned(),
            Some(Value::Number { value: 1.0 })
        );
        assert_eq!(
            s.find("identifier2").cloned(),
            Some(Value::Number { value: 2.0 })
        );
    }

    #[test]
    fn test_variable_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::VariableDeclaration {
                expression: Some(create_expression_number(1.0, &location)),
                name: "identifier".to_string(),
            },
            location,
        };
        let state = State::default();
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 1.0 })
        );
    }

    #[test]
    fn test_function_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::FunctionDeclaration {
                name: "function".to_string(),
                arguments: vec![],
                body: vec![Box::new(Statement {
                    statement_type: StatementType::EOF,
                    location: location.clone(),
                })],
            },
            location: location.clone(),
        };
        let state = State::default();
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("function").cloned(),
            Some(Value::Function(LoxFunction {
                arguments: vec![],
                environment: Default::default(),
                body: vec![Statement {
                    statement_type: StatementType::EOF,
                    location: location.clone(),
                }],
                location,
            }))
        );
    }

    #[test]
    fn test_return_in_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location,
        };
        let mut state = State::default();
        state.in_function = true;
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(s.return_value, Some(Box::new(Value::Number { value: 1.0 })));
    }

    #[test]
    fn test_return_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location: location.clone(),
        };
        let state = State::default();
        let r = statement.evaluate(state);
        assert_eq!(
            r,
            Err(ProgramError {
                message: "Return outside function".to_owned(),
                location,
            })
        );
    }

    #[test]
    fn test_break_in_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location,
        };
        let mut state = State::default();
        state.in_loop = true;
        let (s, _) = statement.evaluate(state).unwrap();
        assert!(s.broke_loop);
        assert!(!s.in_loop);
    }

    #[test]
    fn test_break_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location: location.clone(),
        };
        let state = State::default();
        let r = statement.evaluate(state);
        assert_eq!(
            r,
            Err(ProgramError {
                message: "Break outside loop".to_owned(),
                location,
            })
        );
    }

    #[test]
    fn test_while_loop() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let identifier_expression = Expression {
            expression_type: ExpressionType::VariableLiteral {
                identifier: "identifier".to_owned(),
            },
            location: location.clone(),
        };
        let statement = Statement {
            statement_type: StatementType::While {
                condition: Expression {
                    expression_type: ExpressionType::Binary {
                        operator: TokenType::Less,
                        left: Box::new(identifier_expression.clone()),
                        right: Box::new(create_expression_number(10f32, &location)),
                    },
                    location: location.clone(),
                },
                action: Box::new(Statement {
                    statement_type: StatementType::Expression {
                        expression: Expression {
                            expression_type: ExpressionType::VariableAssignment {
                                identifier: "identifier".to_owned(),
                                expression: Box::new(Expression {
                                    expression_type: ExpressionType::Binary {
                                        operator: TokenType::Plus,
                                        left: Box::new(identifier_expression.clone()),
                                        right: Box::new(create_expression_number(1.0, &location)),
                                    },
                                    location: location.clone(),
                                }),
                            },
                            location: location.clone(),
                        },
                    },
                    location: location.clone(),
                }),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        let (s, _) = statement.evaluate(state).unwrap();
        assert_eq!(
            s.find("identifier").cloned(),
            Some(Value::Number { value: 10.0 })
        );
    }

    fn create_variable_assignment_statement(
        id: &str,
        value: f32,
        location: &SourceCodeLocation,
    ) -> Statement {
        Statement {
            statement_type: StatementType::Expression {
                expression: Expression {
                    expression_type: ExpressionType::VariableAssignment {
                        identifier: id.to_owned(),
                        expression: Box::new(create_expression_number(value, location)),
                    },
                    location: location.clone(),
                },
            },
            location: location.clone(),
        }
    }

    fn create_expression_number(value: f32, location: &SourceCodeLocation) -> Expression {
        Expression {
            expression_type: ExpressionType::ExpressionLiteral {
                value: Literal::Number(value),
            },
            location: location.clone(),
        }
    }
}

#[cfg(test)]
mod test_expression {
    use crate::interpreter::Evaluable;
    use crate::types::{
        DataKeyword, Expression, ExpressionType, Literal, LoxFunction, State, Statement,
        StatementType, Value,
    };
    use crate::types::{SourceCodeLocation, TokenType};

    #[test]
    fn test_expression_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let state = State::default();
        let (final_state, got) = get_number(1.0, &location).evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_variable_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::VariableLiteral {
                identifier: "variable".to_owned(),
            },
            location,
        };
        let mut state = State::default();
        state.insert("variable".to_owned(), Value::Number { value: 1.0 });
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_group_expression() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Grouping {
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_minus_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Unary {
                operator: TokenType::Minus,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: -1.0 });
    }

    #[test]
    fn test_bang_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Unary {
                operator: TokenType::Bang,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: false });
    }

    #[test]
    fn test_sum() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_sum_strings() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_string("1", &location)),
                right: Box::new(get_string("2", &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(
            got,
            Value::String {
                value: "12".to_string()
            }
        );
    }

    #[test]
    fn test_sub() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Minus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 0.0 });
    }

    #[test]
    fn test_mult() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Star,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_div() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Slash,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Greater,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal_with_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Less,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal_with_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::EqualEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_different() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::BangEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_and() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::And,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(true, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_or() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Or,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(false, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_comma() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Binary {
                operator: TokenType::Comma,
                left: Box::new(Expression {
                    expression_type: ExpressionType::VariableAssignment {
                        expression: Box::new(get_number(1.0, &location)),
                        identifier: "identifier".to_owned(),
                    },
                    location: location.clone(),
                }),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        state.insert("identifier".to_owned(), Value::Number { value: 1.0 });
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_conditional() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Conditional {
                condition: Box::new(get_boolean(true, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_conditional_else_branch() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Conditional {
                condition: Box::new(get_boolean(false, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        };
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_variable_assignment() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::VariableAssignment {
                identifier: "identifier".to_owned(),
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        let (final_state, got) = expression.evaluate(state.clone()).unwrap();
        state.insert("identifier".to_owned(), Value::Number { value: 1.0 });
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_function_call() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = Expression {
            expression_type: ExpressionType::Call {
                callee: Box::new(Expression {
                    expression_type: ExpressionType::VariableLiteral {
                        identifier: "function".to_owned(),
                    },
                    location: location.clone(),
                }),
                arguments: vec![],
            },
            location: location.clone(),
        };
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        state.insert(
            "function".to_owned(),
            Value::Function(LoxFunction {
                arguments: vec![],
                environment: State::default(),
                body: vec![Statement {
                    statement_type: StatementType::VariableDeclaration {
                        expression: Some(Expression {
                            expression_type: ExpressionType::ExpressionLiteral {
                                value: Literal::Number(1.0),
                            },
                            location: location.clone(),
                        }),
                        name: "identifier".to_owned(),
                    },
                    location: location.clone(),
                }],
                location,
            }),
        );
        let (mut final_state, got) = expression.evaluate(state.clone()).unwrap();
        state.delete("function");
        final_state.delete("function");
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Nil);
    }

    fn get_string(s: &str, location: &SourceCodeLocation) -> Expression {
        Expression {
            expression_type: ExpressionType::ExpressionLiteral {
                value: Literal::QuotedString(s.to_owned()),
            },
            location: location.clone(),
        }
    }

    fn get_number(n: f32, location: &SourceCodeLocation) -> Expression {
        Expression {
            expression_type: ExpressionType::ExpressionLiteral {
                value: Literal::Number(n),
            },
            location: location.clone(),
        }
    }

    fn get_boolean(n: bool, location: &SourceCodeLocation) -> Expression {
        let keyword = if n {
            DataKeyword::True
        } else {
            DataKeyword::False
        };
        Expression {
            expression_type: ExpressionType::ExpressionLiteral {
                value: Literal::Keyword(keyword),
            },
            location: location.clone(),
        }
    }
}
