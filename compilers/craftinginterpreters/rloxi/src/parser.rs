use crate::types::TokenType::TokenLiteral;
use crate::types::{
    Expression, ExpressionType, Literal, ProgramError, SourceCodeLocation, Token, TokenType,
};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<I: Iterator<Item = Token>> {
    content: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(content: Peekable<I>) -> Parser<I> {
        Parser { content }
    }

    pub(crate) fn parse_expression(&mut self) -> Result<Expression, ProgramError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, ProgramError> {
        let variable = self.parse_comma()?;
        match self.content.peek().cloned() {
            Some(Token {
                token_type: TokenType::Equal,
                location,
                lexeme: _,
            }) => {
                self.content.next();
                if self.content.peek().is_some() {
                    let expression = self.parse_assignment()?;
                    match variable {
                        Expression {
                            expression_type: ExpressionType::VariableLiteral { identifier },
                            location,
                        } => Ok(Expression {
                            expression_type: ExpressionType::VariableAssignment {
                                identifier,
                                expression: Box::new(expression),
                            },
                            location,
                        }),
                        _ => Err(ProgramError {
                            location,
                            message: "Invalid assignment target".to_owned(),
                        }),
                    }
                } else {
                    Err(ProgramError {
                        location,
                        message: "No right side in assignment".to_owned(),
                    })
                }
            }
            _ => Ok(variable),
        }
    }

    fn parse_comma(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_ternary,
            Parser::parse_comma,
            &vec![TokenType::Comma],
        )
    }

    fn parse_ternary(&mut self) -> Result<Expression, ProgramError> {
        let condition = self.parse_or()?;
        if self
            .content
            .peek()
            .map(|t| t.token_type == TokenType::Question)
            .unwrap_or(false)
        {
            let then_branch = self.parse_expression()?;
            self.consume(
                TokenType::Colon,
                "Expected ':' after then branch of conditional expression",
                then_branch.location.clone(),
            )?;
            let else_branch = self.parse_ternary()?;
            Ok(Expression {
                location: condition.location.clone(),
                expression_type: ExpressionType::Conditional {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
            })
        } else {
            Ok(condition)
        }
    }

    fn parse_or(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(Parser::parse_and, Parser::parse_or, &vec![TokenType::Or])
    }

    fn parse_and(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_equality,
            Parser::parse_and,
            &vec![TokenType::And],
        )
    }

    fn parse_equality(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_comparison,
            Parser::parse_equality,
            &vec![TokenType::And],
        )
    }

    fn parse_comparison(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_addition,
            Parser::parse_comparison,
            &vec![
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
        )
    }

    fn parse_addition(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_multiplication,
            Parser::parse_addition,
            &vec![TokenType::Minus, TokenType::Plus],
        )
    }

    fn parse_multiplication(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_unary,
            Parser::parse_multiplication,
            &vec![TokenType::Star, TokenType::Slash],
        )
    }

    fn parse_unary(&mut self) -> Result<Expression, ProgramError> {
        if self
            .content
            .peek()
            .map(|t| [TokenType::Minus, TokenType::Plus].contains(&t.token_type))
            .unwrap_or(false)
        {
            let t = self.content.next().unwrap();
            let value = self.parse_unary()?;
            Ok(Expression {
                expression_type: ExpressionType::Unary {
                    operator: t.token_type,
                    operand: Box::new(value),
                },
                location: t.location,
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expression, ProgramError> {
        let callee = self.parse_primary()?;
        if self
            .content
            .peek()
            .map(|t| t.token_type == TokenType::LeftParen)
            .unwrap_or(false)
        {
            self.content.next();
            let mut args = vec![];
            loop {
                let arg = self.parse_ternary()?;
                args.push(Box::new(arg));
                match self.content.peek() {
                    None
                    | Some(Token {
                        token_type: TokenType::RightParen,
                        ..
                    }) => break,
                    _ => self.consume(
                        TokenType::Comma,
                        "Expected ',' after call argument",
                        callee.location.clone(),
                    )?,
                }
            }
            if self.content.peek().is_some() {
                self.content.next();
                Ok(Expression {
                    location: callee.location.clone(),
                    expression_type: ExpressionType::Call {
                        callee: Box::new(callee),
                        arguments: args,
                    },
                })
            } else {
                Err(ProgramError {
                    location: callee.location.clone(),
                    message: "Expecting right parenthesis".to_owned(),
                })
            }
        } else {
            Ok(callee)
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ProgramError> {
        match self.content.next() {
            Some(Token {
                token_type: TokenType::TokenLiteral { value },
                location,
                lexeme: _,
            }) => Ok(Expression {
                expression_type: ExpressionType::ExpressionLiteral { value },
                location,
            }),
            Some(Token {
                token_type: TokenType::Identifier { name },
                location,
                lexeme: _,
            }) => Ok(Expression {
                expression_type: ExpressionType::VariableLiteral { identifier: name },
                location,
            }),
            Some(Token {
                token_type: TokenType::LeftParen,
                location,
                lexeme: _,
            }) => self.parse_group(location),
            None => Err(ProgramError {
                message: "Unexpected end of file! Expecting primary".to_owned(),
                location: SourceCodeLocation {
                    file: "".to_owned(),
                    line: 0,
                },
            }),
            Some(Token {
                token_type,
                location,
                lexeme,
            }) => self.parse_left_side_missing(token_type, location, lexeme),
        }
    }

    fn parse_left_side_missing(
        &mut self,
        token_type: TokenType,
        location: SourceCodeLocation,
        lexeme: String,
    ) -> Result<Expression, ProgramError> {
        match token_type {
            TokenType::EqualEqual | TokenType::BangEqual => {
                self.parse_equality()?;
                Err(ProgramError {
                    location,
                    message: "Equality without left side".to_owned(),
                })
            }
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                self.parse_comparison()?;
                Err(ProgramError {
                    location,
                    message: "Comparision without left side".to_owned(),
                })
            }
            TokenType::Plus => {
                self.parse_addition()?;
                Err(ProgramError {
                    location,
                    message: "Addition without left side".to_owned(),
                })
            }
            TokenType::Slash | TokenType::Star => {
                self.parse_multiplication()?;
                Err(ProgramError {
                    location,
                    message: "Multiplication without left side".to_owned(),
                })
            }
            _ => Err(ProgramError {
                location,
                message: format!("Expecting a literal, but got {}!", lexeme),
            }),
        }
    }

    fn parse_group(&mut self, location: SourceCodeLocation) -> Result<Expression, ProgramError> {
        let content = self.take_while(|t| t.token_type != TokenType::RightParen);
        let mut parser = Parser::new(content.peekable());
        let next = self.content.next();
        if let Some(Token {
            token_type: TokenType::RightParen,
            location,
            lexeme: _,
        }) = next
        {
            parser.parse_expression()
        } else {
            Err(ProgramError {
                location,
                message: "Missing `)`.".to_owned(),
            })
        }
    }

    fn take_while<F: Fn(&Token) -> bool>(&mut self, f: F) -> Peekable<IntoIter<Token>> {
        let mut buffer = vec![];
        while let Some(t) = self.content.peek().cloned() {
            if f(&t) {
                buffer.push(t);
                self.content.next();
            } else {
                break;
            }
        }
        buffer.into_iter().peekable()
    }

    fn parse_binary<
        L: Fn(&mut Parser<I>) -> Result<Expression, ProgramError>,
        R: Fn(&mut Parser<I>) -> Result<Expression, ProgramError>,
    >(
        &mut self,
        parse_left: L,
        parse_right: R,
        operators: &[TokenType],
    ) -> Result<Expression, ProgramError> {
        let left = parse_left(self)?;
        if self
            .content
            .peek()
            .cloned()
            .map(|t| operators.contains(&t.token_type))
            .unwrap_or(false)
        {
            let t = self.content.next().unwrap();
            let right = parse_right(self)?;
            Ok(Expression {
                location: left.location.clone(),
                expression_type: ExpressionType::Binary {
                    left: Box::new(left),
                    operator: t.token_type,
                    right: Box::new(right),
                },
            })
        } else {
            Ok(left)
        }
    }

    fn consume(
        &mut self,
        token: TokenType,
        message: &str,
        location: SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        let n = self.content.next();
        match n {
            Some(t) if t.token_type == token => Ok(()),
            _ => Err(ProgramError {
                location,
                message: message.to_owned(),
            }),
        }
    }
}
