use crate::types::{
    EvaluationResult, Expression, ExpressionType, ProgramError, SourceCodeLocation, State,
    TokenType, Value, ValueError,
};
use std::convert::TryInto;
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Sub};

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
    op: fn(bool, bool) -> bool,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state)?;
    let (final_state, right_value) = right.evaluate(s)?;
    Ok((
        final_state,
        Value::Boolean {
            value: op(left_value.is_truthy(), right_value.is_truthy()),
        },
    ))
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
            message: format!("Variable {} not found!", name),
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
        Value::Function { arity, .. } if arity != arguments.len() as u8 => Err(callee
            .create_program_error(
                format!(
                    "Wrong number of arguments! Expected: {} Got: {}",
                    arity,
                    arguments.len()
                )
                .as_str(),
            )),
        Value::Function {
            environment,
            function,
            ..
        } => {
            // TODO: Use environment to have closures!!!
            let mut values = vec![];
            let mut current_state = next_state;
            for e in arguments {
                let (value_status, value) = e.evaluate(current_state)?;
                current_state = value_status;
                values.push(value);
            }
            function(current_state, &values)
        }
        _ => Err(callee.create_program_error("Only functions or classes can be called!")),
    }
}

trait Evaluable {
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
                        self.create_program_error(&format!("Variable `{}` not found", identifier))
                    })?
                    .clone();
                Ok((state, value))
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
            } => boolean_expression(state, left, right, bool::bitand),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Or,
            } => boolean_expression(state, left, right, bool::bitor),
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
