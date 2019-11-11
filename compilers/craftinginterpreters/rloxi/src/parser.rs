use crate::types::{
    DataKeyword, Expression, ExpressionType, Literal, ProgramError, SourceCodeLocation, Statement,
    StatementType, Token, TokenType,
};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<I: Iterator<Item = Token>> {
    block_stack: u8,
    content: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(content: Peekable<I>) -> Parser<I> {
        Parser {
            block_stack: 0,
            content,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ProgramError>> {
        let mut output_vec = vec![];
        let mut error_vec = vec![];

        while self.content.peek().is_some() {
            match self.parse_statement() {
                Ok(s) => output_vec.push(s),
                Err(e) => error_vec.push(e),
            }
        }

        if error_vec.is_empty() {
            Ok(output_vec)
        } else {
            Err(error_vec)
        }
    }

    pub(crate) fn parse_statement(&mut self) -> Result<Statement, ProgramError> {
        match self.content.peek().cloned() {
            Some(Token {
                location,
                token_type: TokenType::If,
                ..
            }) => self.parse_if_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::Return,
                ..
            }) => self.parse_return_statement(location),
            Some(Token {
                location,
                token_type: TokenType::Var,
                ..
            }) => self.parse_var_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::LeftBrace,
                ..
            }) => self.parse_block_statement(location),
            Some(Token {
                location,
                token_type: TokenType::Fun,
                ..
            }) => self.parse_fun_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::While,
                ..
            }) => self.parse_while_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::For,
                ..
            }) => self.parse_for_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::EOF,
                ..
            }) => Ok(Statement {
                location,
                statement_type: StatementType::EOF,
            }),
            Some(Token {
                location,
                token_type: TokenType::Print,
                ..
            }) => {
                let expression = self.parse_expression()?;
                Ok(Statement {
                    location,
                    statement_type: StatementType::PrintStatement { expression },
                })
            }
            Some(Token {
                location,
                token_type: TokenType::Break,
                ..
            }) if self.block_stack > 0 => {
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after break statement",
                    &location,
                )?;
                Ok(Statement {
                    location,
                    statement_type: StatementType::Break,
                })
            }
            Some(Token {
                location,
                token_type: TokenType::Break,
                ..
            }) => Err(ProgramError {
                message: "Break statement can't go here".to_owned(),
                location,
            }),
            None => Err(ProgramError {
                message: "Unexpected end of file".to_owned(),
                location: SourceCodeLocation {
                    line: 0,
                    file: "".to_owned(),
                },
            }),
            _ => {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon",
                    &expression.location,
                )?;
                Ok(Statement {
                    location: expression.location.clone(),
                    statement_type: StatementType::Expression { expression },
                })
            }
        }
    }

    fn parse_if_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after if token",
            location,
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after if token",
            location,
        )?;
        let then = Box::new(self.parse_statement()?);
        let otherwise = if self
            .content
            .peek()
            .map(|t| t.token_type == TokenType::Else)
            .unwrap_or(false)
        {
            self.content.next();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::If {
                condition,
                then,
                otherwise,
            },
        })
    }

    fn parse_return_statement(
        &mut self,
        location: SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        if self
            .content
            .peek()
            .map(|t| t.token_type == TokenType::Semicolon)
            .unwrap_or(false)
        {
            Ok(Statement {
                location,
                statement_type: StatementType::Return { value: None },
            })
        } else {
            let value = self.parse_expression()?;
            Ok(Statement {
                location,
                statement_type: StatementType::Return { value: Some(value) },
            })
        }
    }

    fn parse_var_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        if let Some(TokenType::Identifier { name }) = self.content.next().map(|t| t.token_type) {
            match self.content.next() {
                Some(Token {
                    token_type: TokenType::Semicolon,
                    ..
                }) => Ok(Statement {
                    location: location.clone(),
                    statement_type: StatementType::VariableDeclaration {
                        name,
                        expression: None,
                    },
                }),
                Some(Token {
                    token_type: TokenType::Equal,
                    ..
                }) => {
                    let expression = Some(self.parse_expression()?);
                    self.consume(
                        TokenType::Semicolon,
                        "Expected semicolon at end of statement",
                        location,
                    )?;
                    Ok(Statement {
                        location: location.clone(),
                        statement_type: StatementType::VariableDeclaration { name, expression },
                    })
                }
                _ => Err(ProgramError {
                    location: location.clone(),
                    message: "Invalid variable declaration!".to_owned(),
                }),
            }
        } else {
            Err(ProgramError {
                location: location.clone(),
                message: "Invalid variable declaration!".to_owned(),
            })
        }
    }

    fn parse_block_statement(
        &mut self,
        mut location: SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.consume(TokenType::LeftBrace, "Expected left brace", &location)?;
        let mut statements = vec![];
        while self
            .content
            .peek()
            .map(|t| t.token_type != TokenType::RightBrace)
            .unwrap_or(false)
        {
            let statement = self.parse_statement()?;
            location = statement.location.clone();
            statements.push(Box::new(statement));
        }
        self.consume(TokenType::RightBrace, "Expected '{' after block", &location)?;
        Ok(Statement {
            location,
            statement_type: StatementType::Block { body: statements },
        })
    }

    fn parse_fun_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
            ..
        }) = self.content.next()
        {
            self.consume(
                TokenType::LeftParen,
                "Expected a parenthesis after name!",
                &location,
            )?;
            let arguments = self.parse_parameters(&location, Parser::parse_identifier)?;
            self.consume(
                TokenType::RightParen,
                "Expected a parenthesis after parameters!",
                &location,
            )?;
            let body = if let StatementType::Block { body } =
                self.parse_block_statement(location.clone())?.statement_type
            {
                body
            } else {
                panic!("Can't happen")
            };
            Ok(Statement {
                statement_type: StatementType::FunctionDeclaration {
                    name,
                    arguments,
                    body,
                },
                location: location.clone(),
            })
        } else {
            Err(ProgramError {
                location: location.clone(),
                message: "Expected a function name!".to_owned(),
            })
        }
    }

    fn parse_while_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after while token",
            location,
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after while condition",
            &condition.location,
        )?;
        self.block_stack += 1;
        let body = self.parse_statement()?;
        self.block_stack -= 1;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::While {
                condition,
                action: Box::new(body),
            },
        })
    }

    fn parse_for_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after while token",
            location,
        )?;
        let temp_init = match self.content.peek().cloned() {
            Some(Token {
                location,
                token_type: TokenType::Semicolon,
                ..
            }) => {
                self.content.next();
                Ok(Statement {
                    location: location.clone(),
                    statement_type: StatementType::Expression {
                        expression: Expression {
                            expression_type: ExpressionType::ExpressionLiteral {
                                value: Literal::Keyword(DataKeyword::Nil),
                            },
                            location,
                        },
                    },
                })
            }
            Some(_) => self.parse_statement(),
            _ => Err(ProgramError {
                message: "Unexpected end of file".to_owned(),
                location: SourceCodeLocation {
                    line: 0,
                    file: "".to_owned(),
                },
            }),
        }?;
        let init = match &temp_init.statement_type {
            StatementType::VariableDeclaration { .. } | StatementType::Expression { .. } => {
                Ok(temp_init)
            }
            _ => Err(ProgramError {
                location: temp_init.location,
                message: "Invalid statement for initialization!".to_owned(),
            }),
        }?;
        let condition = {
            if !self.peek(TokenType::Semicolon) {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::Semicolon,
                    "Expecting ';' after expression.",
                    &expression.location,
                )?;
                expression
            } else {
                Expression {
                    location: location.clone(),
                    expression_type: ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                }
            }
        };
        let incr = {
            let expression = if !self.peek(TokenType::RightParen) {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::RightParen,
                    "Expecting ')' after expression.",
                    &expression.location,
                )?;
                expression
            } else {
                Expression {
                    location: location.clone(),
                    expression_type: ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                }
            };
            Statement {
                location: expression.location.clone(),
                statement_type: StatementType::Expression { expression },
            }
        };
        let body = self.parse_statement()?;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(init),
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::While {
                            condition,
                            action: Box::new(Statement {
                                location: location.clone(),
                                statement_type: StatementType::Block {
                                    body: vec![Box::new(body), Box::new(incr)],
                                },
                            }),
                        },
                    }),
                ],
            },
        })
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
                ..
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
            &[TokenType::Comma],
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
            self.content.next();
            let then_branch = self.parse_expression()?;
            self.consume(
                TokenType::Colon,
                "Expected ':' after then branch of conditional expression",
                &then_branch.location,
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
            &vec![TokenType::EqualEqual, TokenType::BangEqual],
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
            let args = self.parse_parameters(&callee.location, Parser::parse_ternary)?;
            if self.content.peek().is_some() {
                self.content.next();
                Ok(Expression {
                    location: callee.location.clone(),
                    expression_type: ExpressionType::Call {
                        callee: Box::new(callee),
                        arguments: args.into_iter().map(Box::new).collect(),
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

    fn parse_parameters<R, F: Fn(&mut Parser<I>) -> Result<R, ProgramError>>(
        &mut self,
        location: &SourceCodeLocation,
        parser: F,
    ) -> Result<Vec<R>, ProgramError> {
        let mut args = vec![];
        loop {
            match self.content.peek() {
                None
                | Some(Token {
                    token_type: TokenType::RightParen,
                    ..
                }) => break,
                _ => {
                    let arg = parser(self)?;
                    args.push(arg);
                    if self
                        .content
                        .peek()
                        .map(|t| t.token_type != TokenType::RightParen)
                        .unwrap_or(false)
                    {
                        self.consume(
                            TokenType::Comma,
                            "Expected ',' after call argument",
                            location,
                        )?;
                    }
                }
            }
        }
        Ok(args)
    }

    fn parse_identifier(&mut self) -> Result<String, ProgramError> {
        match self.content.next() {
            Some(Token {
                token_type: TokenType::Identifier { name },
                ..
            }) => Ok(name),
            Some(Token { location, .. }) => Err(ProgramError {
                location,
                message: "Expected identifier!".to_owned(),
            }),
            None => panic!("Can't happen"),
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ProgramError> {
        match self.content.next() {
            Some(Token {
                token_type: TokenType::TokenLiteral { value },
                location,
                ..
            }) => Ok(Expression {
                expression_type: ExpressionType::ExpressionLiteral { value },
                location,
            }),
            Some(Token {
                token_type: TokenType::Identifier { name },
                location,
                ..
            }) => Ok(Expression {
                expression_type: ExpressionType::VariableLiteral { identifier: name },
                location,
            }),
            Some(Token {
                token_type: TokenType::LeftParen,
                location,
                ..
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
            ..
        }) = next
        {
            let expression = Box::new(parser.parse_expression()?);
            Ok(Expression {
                location,
                expression_type: ExpressionType::Grouping { expression },
            })
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

    fn peek(&mut self, token: TokenType) -> bool {
        if let Some(t) = self.content.peek() {
            token == t.token_type
        } else {
            false
        }
    }

    fn consume(
        &mut self,
        token: TokenType,
        message: &str,
        location: &SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        let n = self.content.next();
        match n {
            Some(t) if t.token_type == token => Ok(()),
            Some(t) => Err(ProgramError {
                location: t.location,
                message: message.to_owned(),
            }),
            _ => Err(ProgramError {
                location: location.clone(),
                message: message.to_owned(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::types::ExpressionType::ExpressionLiteral;
    use crate::types::StatementType::VariableDeclaration;
    use crate::types::{
        Expression, ExpressionType, Literal, SourceCodeLocation, Statement, StatementType, Token,
        TokenType,
    };

    #[test]
    fn parse_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![Token {
            location: location.clone(),
            token_type: TokenType::TokenLiteral {
                value: Literal::Number(1.0),
            },
            lexeme: "1.0".to_string(),
        }];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                location,
                expression_type: ExpressionType::ExpressionLiteral {
                    value: Literal::Number(1.0),
                },
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_identifier() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![Token {
            location: location.clone(),
            token_type: TokenType::Identifier {
                name: "identifier".to_string(),
            },
            lexeme: "1.0".to_string(),
        }];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                location,
                expression_type: ExpressionType::VariableLiteral {
                    identifier: "identifier".to_owned(),
                },
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_identifier_group() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                location: location.clone(),
                expression_type: ExpressionType::Grouping {
                    expression: Box::new(Expression {
                        location,
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                    }),
                },
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_identifier_group_without_right_paren() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression();
        assert!(result.is_err());
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_identifier_group_without_right_paren_and_more_content() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression();
        assert!(result.is_err());
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_call_with_no_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                expression_type: ExpressionType::Call {
                    callee: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location: location.clone(),
                    }),
                    arguments: vec![],
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_call_with_one_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                expression_type: ExpressionType::Call {
                    callee: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location: location.clone(),
                    }),
                    arguments: vec![Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location: location.clone(),
                    })],
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_call_with_multiple_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Comma,
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                expression_type: ExpressionType::Call {
                    callee: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location: location.clone(),
                    }),
                    arguments: vec![
                        Box::new(Expression {
                            expression_type: ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location: location.clone(),
                        }),
                        Box::new(Expression {
                            expression_type: ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location: location.clone(),
                        }),
                    ],
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_unary_with_minus() {
        test_unary(TokenType::Minus);
    }

    #[test]
    fn parse_unary_with_plus() {
        test_unary(TokenType::Plus)
    }

    #[test]
    fn parse_multiplication_with_star() {
        test_binary(TokenType::Star);
    }

    #[test]
    fn parse_multiplication_with_slash() {
        test_binary(TokenType::Star);
    }

    #[test]
    fn parse_addition_with_minus() {
        test_binary(TokenType::Minus);
    }

    #[test]
    fn parse_comparison_with_less_equal() {
        test_binary(TokenType::LessEqual);
    }

    #[test]
    fn parse_comparison_with_less() {
        test_binary(TokenType::Less);
    }

    #[test]
    fn parse_comparison_with_greater_equal() {
        test_binary(TokenType::GreaterEqual);
    }

    #[test]
    fn parse_comparison_with_greater() {
        test_binary(TokenType::Greater);
    }

    #[test]
    fn parse_addition_with_plus() {
        test_binary(TokenType::Plus);
    }

    #[test]
    fn parse_equality_with_equal_equal() {
        test_binary(TokenType::EqualEqual);
    }

    #[test]
    fn parse_equality_with_bang_equal() {
        test_binary(TokenType::BangEqual);
    }

    #[test]
    fn parse_and() {
        test_binary(TokenType::And);
    }

    #[test]
    fn parse_or() {
        test_binary(TokenType::Or);
    }

    #[test]
    fn parse_ternary() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "identifier".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Question,
                lexeme: "?".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier1".to_string(),
                },
                lexeme: "identifier1".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Colon,
                lexeme: ":".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier2".to_string(),
                },
                lexeme: "1.0".to_string(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                expression_type: ExpressionType::Conditional {
                    condition: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location: location.clone(),
                    }),
                    then_branch: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier1".to_owned(),
                        },
                        location: location.clone(),
                    }),
                    else_branch: Box::new(Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "identifier2".to_owned(),
                        },
                        location: location.clone(),
                    }),
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_comma() {
        test_binary(TokenType::Comma);
    }

    #[test]
    fn parse_assignment() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
                lexeme: "identifier".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                expression_type: ExpressionType::VariableAssignment {
                    identifier: "identifier".to_string(),
                    expression: Box::new(Expression {
                        expression_type: ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    })
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_if() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::If,
                lexeme: "if".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::If {
                    condition: Expression {
                        expression_type: ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    },
                    then: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: Expression {
                                expression_type: ExpressionType::ExpressionLiteral {
                                    value: Literal::Number(1.0),
                                },
                                location: location.clone(),
                            }
                        },
                    }),
                    otherwise: None,
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_if_else() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::If,
                lexeme: "if".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Else,
                lexeme: "else".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
        ];
        let literal_expression = Box::new(Statement {
            location: location.clone(),
            statement_type: StatementType::Expression {
                expression: Expression {
                    expression_type: ExpressionType::ExpressionLiteral {
                        value: Literal::Number(1.0),
                    },
                    location: location.clone(),
                },
            },
        });
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::If {
                    condition: Expression {
                        expression_type: ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    },
                    then: literal_expression.clone(),
                    otherwise: Some(literal_expression),
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_var() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
                lexeme: "var".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::VariableDeclaration {
                    name: "identifier".to_owned(),
                    expression: None,
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_var_with_expression() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
                lexeme: "var".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Equal,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::VariableDeclaration {
                    name: "identifier".to_owned(),
                    expression: Some(Expression {
                        expression_type: ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    }),
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_block() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::Block {
                    body: vec![
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: Expression {
                                    location: location.clone(),
                                    expression_type: ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned(),
                                    }
                                }
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: Expression {
                                    location: location.clone(),
                                    expression_type: ExpressionType::ExpressionLiteral {
                                        value: Literal::Number(1.0),
                                    }
                                }
                            }
                        }),
                    ]
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_fun() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Fun,
                lexeme: "fun".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
                lexeme: "argument".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::FunctionDeclaration {
                    name: "identifier".to_owned(),
                    arguments: vec!["argument".to_owned()],
                    body: vec![
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: Expression {
                                    location: location.clone(),
                                    expression_type: ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned(),
                                    }
                                }
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: Expression {
                                    location: location.clone(),
                                    expression_type: ExpressionType::ExpressionLiteral {
                                        value: Literal::Number(1.0),
                                    }
                                }
                            }
                        }),
                    ]
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_while() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::While,
                lexeme: "while".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
                lexeme: "argument".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::While {
                    condition: Expression {
                        expression_type: ExpressionType::VariableLiteral {
                            identifier: "argument".to_owned(),
                        },
                        location: location.clone(),
                    },
                    action: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Block {
                            body: vec![
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: Expression {
                                            location: location.clone(),
                                            expression_type: ExpressionType::VariableLiteral {
                                                identifier: "identifier".to_owned(),
                                            }
                                        }
                                    }
                                }),
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: Expression {
                                            location: location.clone(),
                                            expression_type: ExpressionType::ExpressionLiteral {
                                                value: Literal::Number(1.0),
                                            }
                                        }
                                    }
                                }),
                            ],
                        }
                    })
                },
                location: location.clone(),
            }
        );
        assert!(parser.content.is_empty());
    }

    #[test]
    fn parse_full_for() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::For,
                lexeme: "for".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
                lexeme: "var".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
                lexeme: "argument".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
                lexeme: "argument".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        let body_statement = Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: Expression {
                                location: location.clone(),
                                expression_type: ExpressionType::VariableLiteral {
                                    identifier: "identifier".to_owned(),
                                },
                            },
                        },
                    }),
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: Expression {
                                location: location.clone(),
                                expression_type: ExpressionType::ExpressionLiteral {
                                    value: Literal::Number(1.0),
                                },
                            },
                        },
                    }),
                ],
            },
        };
        let while_statement = Statement {
            statement_type: StatementType::While {
                condition: Expression {
                    expression_type: ExpressionType::VariableLiteral {
                        identifier: "argument".to_owned(),
                    },
                    location: location.clone(),
                },
                action: Box::new(Statement {
                    location: location.clone(),
                    statement_type: StatementType::Block {
                        body: vec![
                            Box::new(body_statement),
                            Box::new(Statement {
                                location: location.clone(),
                                statement_type: StatementType::Expression {
                                    expression: Expression {
                                        expression_type: ExpressionType::VariableLiteral {
                                            identifier: "argument".to_owned(),
                                        },
                                        location: location.clone(),
                                    },
                                },
                            }),
                        ],
                    },
                }),
            },
            location: location.clone(),
        };
        let for_block = Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: VariableDeclaration {
                            expression: None,
                            name: "identifier".to_owned(),
                        },
                    }),
                    Box::new(while_statement),
                ],
            },
        };
        assert_eq!(result, for_block);
        assert!(parser.content.is_empty());
    }

    fn test_binary(token_type: TokenType) {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: token_type.clone(),
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                location: location.clone(),
                expression_type: ExpressionType::Binary {
                    operator: token_type,
                    left: Box::new(Expression {
                        expression_type: ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    }),
                    right: Box::new(Expression {
                        expression_type: ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location: location.clone(),
                    }),
                },
            }
        );
        assert!(parser.content.is_empty());
    }

    fn test_unary(token_type: TokenType) {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: token_type.clone(),
                lexeme: "1.0".to_string(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(1.0),
                },
                lexeme: "1.0".to_string(),
            },
        ];
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            Expression {
                location: location.clone(),
                expression_type: ExpressionType::Unary {
                    operator: token_type,
                    operand: Box::new(Expression {
                        expression_type: ExpressionLiteral {
                            value: Literal::Number(1.0)
                        },
                        location: location.clone(),
                    }),
                },
            }
        );
        assert!(parser.content.is_empty());
    }
}
