use crate::types::{
    DataKeyword, Expression, ExpressionFactory, ExpressionType, Literal, ProgramError,
    SourceCodeLocation, Statement, StatementType, Token, TokenType,
};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<I: Iterator<Item = Token>> {
    block_stack: u8,
    content: Peekable<I>,
    expression_factory: ExpressionFactory,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(content: Peekable<I>) -> Parser<I> {
        Parser {
            block_stack: 0,
            expression_factory: ExpressionFactory::new(),
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
                     token_type: TokenType::Class,
                     ..
                 }) => self.parse_class_statement(&location),
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
                self.content.next();
                let expression = self.parse_expression()?;
                self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
                Ok(Statement {
                    location,
                    statement_type: StatementType::PrintStatement { expression },
                })
            }
            Some(Token {
                location,
                token_type: TokenType::Break,
                ..
            }) => {
                self.content.next();
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after break statement",
                    &location,
                )?;
                if self.block_stack > 0 {
                    Ok(Statement {
                        location,
                        statement_type: StatementType::Break,
                    })
                } else {
                    Err(ProgramError {
                        message: "Break statement can't go here".to_owned(),
                        location,
                    })
                }
            }
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

    fn parse_class_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
            ..
        }) = self.content.next() {
            self.consume(
                TokenType::LeftBrace,
                "Expected '{' before class body",
                &location,
            )?;
            let mut methods = vec![];
            while !self.peek(TokenType::RightBrace) {
                methods.push(Box::new(self.parse_function(&location)?));
            }
            self.consume(
                TokenType::RightBrace,
                "Expected '}' after class body",
                &location,
            )?;

            Ok(Statement {
                statement_type: StatementType::Class {
                    name,
                    methods,
                },
                location,
            })
        } else {
            Err(ProgramError {
                message: "Expected name in class definition".to_owned(),
                location: location.clone(),
            })
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
            self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
            Ok(Statement {
                location,
                statement_type: StatementType::Return { value: None },
            })
        } else {
            let value = self.parse_expression()?;
            self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
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
                    self.consume(TokenType::Semicolon, "Expected semicolon", location)?;
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
        self.block_stack += 1;
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
        self.consume(TokenType::RightBrace, "Expected '}' after block", &location)?;
        self.block_stack -= 1;
        Ok(Statement {
            location,
            statement_type: StatementType::Block { body: statements },
        })
    }

    fn parse_anonymous_function(&mut self, location: SourceCodeLocation) -> Result<Expression, ProgramError> {
        self.content.next();
        let arguments = self.parse_parameters(&location, Parser::parse_identifier)?;
        self.consume(
            TokenType::RightParen,
            "Expected a parenthesis after parameters!",
            &location,
        )?;
        let body = if let StatementType::Block { body } =
        self.parse_block_statement(location.clone())?.statement_type
        {
            body.into_iter().map(|b| *b).collect::<Vec<Statement>>()
        } else {
            panic!("Can't happen")
        };
        Ok(self.expression_factory.new_expression(
            ExpressionType::AnonymousFunction {
                arguments,
                body,
            },
            location,
        ))
    }

    fn parse_fun_statement(
        &mut self,
        location: &SourceCodeLocation,
    ) -> Result<Statement, ProgramError> {
        self.content.next();
        self.parse_function(location)
    }

    fn parse_function(&mut self, location: &SourceCodeLocation) -> Result<Statement, ProgramError> {
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
            ..
        }) = self.content.next() {
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
                        expression: self.expression_factory.new_expression(
                            ExpressionType::ExpressionLiteral {
                                value: Literal::Keyword(DataKeyword::Nil),
                            },
                            location,
                        ),
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
                self.expression_factory.new_expression(
                    ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                    location.clone(),
                )
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
                self.expression_factory.new_expression(
                    ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                    location.clone(),
                )
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
                            ..
                        } => Ok(self.expression_factory.new_expression(
                            ExpressionType::VariableAssignment {
                                identifier,
                                expression: Box::new(expression),
                            },
                            location,
                        )),
                        Expression {
                            expression_type: ExpressionType::Get { callee, property },
                            location,
                            ..
                        } => Ok(self.expression_factory.new_expression(
                            ExpressionType::Set { callee, property, value: Box::new(expression) },
                            location,
                        )),
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
            let location = condition.location.clone();
            Ok(self.expression_factory.new_expression(
                ExpressionType::Conditional {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                location,
            ))
        } else {
            Ok(condition)
        }
    }

    fn parse_or(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(Parser::parse_and, Parser::parse_or, &[TokenType::Or])
    }

    fn parse_and(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(Parser::parse_equality, Parser::parse_and, &[TokenType::And])
    }

    fn parse_equality(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_comparison,
            Parser::parse_equality,
            &[TokenType::EqualEqual, TokenType::BangEqual],
        )
    }

    fn parse_comparison(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_addition,
            Parser::parse_comparison,
            &[
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
            &[TokenType::Minus, TokenType::Plus],
        )
    }

    fn parse_multiplication(&mut self) -> Result<Expression, ProgramError> {
        self.parse_binary(
            Parser::parse_unary,
            Parser::parse_multiplication,
            &[TokenType::Star, TokenType::Slash],
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
            Ok(self.expression_factory.new_expression(
                ExpressionType::Unary {
                    operator: t.token_type,
                    operand: Box::new(value),
                },
                t.location,
            ))
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expression, ProgramError> {
        let callee = self.parse_primary()?;
        match self.content.peek().map(|t| t.token_type.clone()) {
            Some(TokenType::LeftParen) => self.parse_call_function(callee),
            Some(TokenType::Dot) => self.parse_call_property(callee),
            _ => Ok(callee)
        }
    }

    fn parse_call_property(&mut self, callee: Expression) -> Result<Expression, ProgramError> {
        self.consume(
            TokenType::Dot,
            "Expected '.' on property call expression",
            &callee.location,
        )?;
        if let Some(TokenType::Identifier { name }) = self.content.next().map(|t| t.token_type) {
            let location = callee.location.clone();
            Ok(self.expression_factory.new_expression(
                ExpressionType::Get {
                    callee: Box::new(callee),
                    property: name.to_owned(),
                },
                location,
            ))
        } else {
            Err(callee.create_program_error("Expected property name after '.'"))
        }
    }

    fn parse_call_function(&mut self, callee: Expression) -> Result<Expression, ProgramError> {
        self.consume(
            TokenType::LeftParen,
            "Expected '(' on function call expression",
            &callee.location,
        )?;
        let args = self.parse_parameters(&callee.location, Parser::parse_ternary)?;
        if self.content.peek().is_some() {
            self.content.next();
            let location = callee.location.clone();
            Ok(self.expression_factory.new_expression(
                ExpressionType::Call {
                    callee: Box::new(callee),
                    arguments: args.into_iter().map(Box::new).collect(),
                },
                location,
            ))
        } else {
            Err(ProgramError {
                location: callee.location.clone(),
                message: "Expecting right parenthesis".to_owned(),
            })
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
                token_type: TokenType::Fun,
                location,
                ..
             }) => self.parse_anonymous_function(location),
            Some(Token {
                token_type: TokenType::TokenLiteral { value },
                location,
                ..
            }) => Ok(self
                .expression_factory
                .new_expression(ExpressionType::ExpressionLiteral { value }, location)),
            Some(Token {
                token_type: TokenType::Identifier { name },
                location,
                ..
            }) => Ok(self.expression_factory.new_expression(
                ExpressionType::VariableLiteral { identifier: name },
                location,
            )),
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
            Ok(self
                .expression_factory
                .new_expression(ExpressionType::Grouping { expression }, location))
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
            let location = left.location.clone();
            Ok(self.expression_factory.new_expression(
                ExpressionType::Binary {
                    left: Box::new(left),
                    operator: t.token_type,
                    right: Box::new(right),
                },
                location,
            ))
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
        Expression, ExpressionFactory, ExpressionType, Literal, SourceCodeLocation, Statement,
        StatementType, Token, TokenType,
    };

    fn create_expression(
        expression_type: ExpressionType,
        location: SourceCodeLocation,
    ) -> Expression {
        let mut factory = ExpressionFactory::new();
        factory.new_expression(expression_type, location)
    }

    fn create_expression_with_id(
        expression_type: ExpressionType,
        location: SourceCodeLocation,
        counter: usize,
    ) -> Expression {
        let mut factory = ExpressionFactory::new_starting(counter);
        factory.new_expression(expression_type, location)
    }

    fn create_statement_expression(
        expression_type: ExpressionType,
        location: SourceCodeLocation,
        counter: usize,
    ) -> Statement {
        Statement {
            location: location.clone(),
            statement_type: StatementType::Expression {
                expression: create_expression_with_id(expression_type, location, counter),
            },
        }
    }

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
            create_expression(
                ExpressionType::ExpressionLiteral {
                    value: Literal::Number(1.0),
                },
                location,
            )
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
            create_expression(
                ExpressionType::VariableLiteral {
                    identifier: "identifier".to_owned(),
                },
                location,
            )
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
            create_expression(
                ExpressionType::Grouping {
                    expression: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                },
                location,
            )
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
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![],
                },
                location.clone(),
                1,
            )
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
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                        1,
                    ))],
                },
                location.clone(),
                2,
            )
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
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![
                        Box::new(create_expression_with_id(
                            ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location.clone(),
                            1,
                        )),
                        Box::new(create_expression_with_id(
                            ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location.clone(),
                            2,
                        )),
                    ],
                },
                location.clone(),
                3,
            )
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
            create_expression_with_id(
                ExpressionType::Conditional {
                    condition: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    then_branch: Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier1".to_owned(),
                        },
                        location.clone(),
                        1,
                    )),
                    else_branch: Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier2".to_owned(),
                        },
                        location.clone(),
                        2
                    )),
                },
                location.clone(),
                3,
            )
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
            create_expression_with_id(
                ExpressionType::VariableAssignment {
                    identifier: "identifier".to_string(),
                    expression: Box::new(create_expression_with_id(
                        ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                        1,
                    ))
                },
                location.clone(),
                2,
            )
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
                    condition: create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                    ),
                    then: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: create_expression_with_id(
                                ExpressionType::ExpressionLiteral {
                                    value: Literal::Number(1.0),
                                },
                                location.clone(),
                                1,
                            ),
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
        let mut parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::If {
                    condition: create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                    ),
                    then: Box::new(create_statement_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0)
                        },
                        location.clone(),
                        1
                    )),
                    otherwise: Some(Box::new(create_statement_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0)
                        },
                        location.clone(),
                        2
                    ))),
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
                    expression: Some(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                    )),
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
                                expression: create_expression(
                                    ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned(),
                                    },
                                    location.clone(),
                                ),
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression_with_id(
                                    ExpressionType::ExpressionLiteral {
                                        value: Literal::Number(1.0),
                                    },
                                    location.clone(),
                                    1,
                                ),
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
                                expression: create_expression(
                                    ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned()
                                    },
                                    location.clone(),
                                ),
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression_with_id(
                                    ExpressionType::ExpressionLiteral {
                                        value: Literal::Number(1.0),
                                    },
                                    location.clone(),
                                    1,
                                ),
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
                    condition: create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "argument".to_owned(),
                        },
                        location.clone(),
                    ),
                    action: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Block {
                            body: vec![
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: create_expression_with_id(
                                            ExpressionType::VariableLiteral {
                                                identifier: "identifier".to_owned(),
                                            },
                                            location.clone(),
                                            1,
                                        ),
                                    }
                                }),
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: create_expression_with_id(
                                            ExpressionType::ExpressionLiteral {
                                                value: Literal::Number(1.0),
                                            },
                                            location.clone(),
                                            2,
                                        ),
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
                            expression: create_expression_with_id(
                                ExpressionType::VariableLiteral {
                                    identifier: "identifier".to_owned(),
                                },
                                location.clone(),
                                2,
                            ),
                        },
                    }),
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: create_expression_with_id(
                                ExpressionType::ExpressionLiteral {
                                    value: Literal::Number(1.0),
                                },
                                location.clone(),
                                3,
                            ),
                        },
                    }),
                ],
            },
        };
        let while_statement = Statement {
            statement_type: StatementType::While {
                condition: create_expression(
                    ExpressionType::VariableLiteral {
                        identifier: "argument".to_owned(),
                    },
                    location.clone(),
                ),
                action: Box::new(Statement {
                    location: location.clone(),
                    statement_type: StatementType::Block {
                        body: vec![
                            Box::new(body_statement),
                            Box::new(Statement {
                                location: location.clone(),
                                statement_type: StatementType::Expression {
                                    expression: create_expression_with_id(
                                        ExpressionType::VariableLiteral {
                                            identifier: "argument".to_owned(),
                                        },
                                        location.clone(),
                                        1,
                                    ),
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
            create_expression_with_id(
                ExpressionType::Binary {
                    operator: token_type,
                    left: Box::new(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                    )),
                    right: Box::new(create_expression_with_id(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                        1,
                    )),
                },
                location.clone(),
                2,
            )
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
            create_expression_with_id(
                ExpressionType::Unary {
                    operator: token_type,
                    operand: Box::new(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Number(1.0),
                        },
                        location.clone(),
                    )),
                },
                location.clone(),
                1,
            ),
        );
        assert!(parser.content.is_empty());
    }
}
