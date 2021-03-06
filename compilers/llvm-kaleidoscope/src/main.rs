use std::convert::TryFrom;
use std::io::{stdin, Read};
use std::iter::Peekable;

mod llvm;
use llvm::*;

#[derive(Debug)]
enum ParsingError {
    UnexpectedEndOfFile,
    ExpectingXGotY(Token, Option<Token>),
    ExpectingNumberOrIdentifier(Option<Token>),
}

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    LeftParen,
    RightParen,
    Comma,
    Plus,
    Dash,
    Star,
    Slash,
    Percent,
    GreaterThan,
    LesserThan,
    DoubleEquals,
    BangEquals,
    And,
    Or,
}

#[derive(Debug)]
enum Operation {
    Sum,
    Difference,
    Multiplication,
    Division,
    Modulo,
    IsGreaterThan,
    IsLesserThan,
    IsEqualsTo,
    IsNotEqualsTo,
    Or,
    And,
}

#[derive(Debug)]
enum UnaryOperation {
    ChangeSign,
}

#[derive(Debug)]
struct FunctionPrototype(pub(crate) String, pub(crate) Vec<String>);

#[derive(Debug)]
enum ExprAst {
    Number(f64),
    Variable(String),
    BinaryExpression(Box<ExprAst>, Operation, Box<ExprAst>),
    Call(String, Vec<Box<ExprAst>>),
    Unary(UnaryOperation, Box<ExprAst>),
    Grouping(Box<ExprAst>),
}

#[derive(Debug)]
enum Statement {
    ExpressionStatement(ExprAst),
    ExternFunction(FunctionPrototype),
    Function(FunctionPrototype, ExprAst),
}

#[derive(Debug)]
struct TokenList(Vec<Token>);
#[derive(Debug)]
struct Ast(Vec<Statement>);

fn accumulate_while<T, I: Iterator<Item = T>, F: Fn(&T) -> bool + Copy>(
    source: &mut Peekable<I>,
    zero: T,
    condition: F,
) -> Vec<T> {
    let mut result = Vec::new();
    result.push(zero);
    let mut peeked = source.peek();
    while peeked.is_some() && peeked.map(condition).unwrap_or(false) {
        result.push(source.next().unwrap());
        peeked = source.peek();
    }
    result
}

impl From<String> for TokenList {
    fn from(s: String) -> TokenList {
        let mut res = Vec::new();
        let mut chars = s.chars().peekable();
        while chars.peek().map(|i| i.clone()).is_some() {
            let c = chars.next().unwrap();
            match c {
                '(' => res.push(Token::LeftParen),
                ')' => res.push(Token::RightParen),
                ',' => res.push(Token::Comma),
                '+' => res.push(Token::Plus),
                '-' => res.push(Token::Dash),
                '*' => res.push(Token::Star),
                '/' => res.push(Token::Slash),
                '%' => res.push(Token::Percent),
                '>' => res.push(Token::GreaterThan),
                '<' => res.push(Token::LesserThan),
                '=' => match chars.next().unwrap() {
                    '=' => res.push(Token::DoubleEquals),
                    c => panic!("Unexpected character {}", c),
                },
                '!' => match chars.next().unwrap() {
                    '=' => res.push(Token::BangEquals),
                    c => panic!("Unexpected character {}", c),
                },
                _ if c.is_whitespace() => {}
                _ if c.is_ascii_alphabetic() => {
                    let ident: String =
                        accumulate_while(&mut chars, c, |c| c.is_ascii_alphanumeric())
                            .into_iter()
                            .collect();
                    res.push(match ident.as_str() {
                        "def" => Token::Def,
                        "extern" => Token::Extern,
                        "and" => Token::And,
                        "or" => Token::Or,
                        i => Token::Identifier(i.to_owned()),
                    });
                }
                _ if c.is_ascii_digit() || c == '.' => {
                    let number: String =
                        accumulate_while(&mut chars, c, |c| c.is_ascii_digit() || *c == '.')
                            .into_iter()
                            .collect();
                    res.push(Token::Number(str::parse(&number).unwrap()));
                }
                _ => {}
            }
        }
        TokenList(res)
    }
}

fn parse_call_args<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<Vec<Box<ExprAst>>, ParsingError> {
    let mut res = Vec::new();
    let mut n = source
        .peek()
        .map(|v| v.clone())
        .ok_or(ParsingError::UnexpectedEndOfFile)?;
    while Token::RightParen != n {
        match n {
            Token::Comma => {
                source.next();
            }
            _ => {
                let arg = parse_expression(source)?;
                res.push(Box::new(arg));
            }
        }
        n = source
            .peek()
            .map(|v| v.clone())
            .ok_or(ParsingError::UnexpectedEndOfFile)?;
    }
    source.next();
    Ok(res)
}

fn parse_operator<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    match source.next() {
        Some(Token::Number(n)) => Ok(ExprAst::Number(n)),
        Some(Token::Identifier(i)) => {
            let peeked = source.peek().map(|v| v.clone());
            if let Some(Token::LeftParen) = peeked {
                source.next();
                let args = parse_call_args(source)?;
                Ok(ExprAst::Call(i, args))
            } else {
                Ok(ExprAst::Variable(i))
            }
        }
        t => Err(ParsingError::ExpectingNumberOrIdentifier(t)),
    }
}

fn parse_group<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let peeked = source.peek().map(|v| v.clone());
    match peeked {
        Some(Token::LeftParen) => {
            source.next();
            let exp = parse_expression(source)?;
            expect(source, Token::RightParen)?;
            Ok(ExprAst::Grouping(Box::new(exp)))
        }
        Some(_) => parse_operator(source),
        None => Err(ParsingError::UnexpectedEndOfFile),
    }
}

fn parse_unary<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let peeked = source.peek().map(|v| v.clone());
    match peeked {
        Some(Token::Dash) => {
            source.next();
            let op = parse_group(source)?;
            Ok(ExprAst::Unary(UnaryOperation::ChangeSign, Box::new(op)))
        }
        Some(_) => parse_group(source),
        None => Err(ParsingError::UnexpectedEndOfFile),
    }
}

fn parse_mult_div<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let l = parse_unary(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::Star) | Some(Token::Slash) | Some(Token::Percent) = peeked {
        let op = source.next().unwrap();
        let r = parse_unary(source)?;
        if let Token::Star = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Multiplication,
                Box::new(r),
            ))
        } else if let Token::Slash = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Division,
                Box::new(r),
            ))
        } else {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Modulo,
                Box::new(r),
            ))
        }
    } else {
        Ok(l)
    }
}

fn parse_sum_rest<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let l = parse_mult_div(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::Plus) | Some(Token::Dash) = peeked {
        let op = source.next().unwrap();
        let r = parse_mult_div(source)?;
        if let Token::Plus = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Sum,
                Box::new(r),
            ))
        } else {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Difference,
                Box::new(r),
            ))
        }
    } else {
        Ok(l)
    }
}

fn parse_comp<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let l = parse_sum_rest(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::LesserThan) | Some(Token::GreaterThan) = peeked {
        let op = source.next().unwrap();
        let r = parse_sum_rest(source)?;
        if let Token::LesserThan = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::IsLesserThan,
                Box::new(r),
            ))
        } else {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::IsGreaterThan,
                Box::new(r),
            ))
        }
    } else {
        Ok(l)
    }
}

fn parse_eq_comp<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let l = parse_comp(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::DoubleEquals) | Some(Token::BangEquals) = peeked {
        let op = source.next().unwrap();
        let r = parse_comp(source)?;
        if let Token::DoubleEquals = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::IsEqualsTo,
                Box::new(r),
            ))
        } else {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::IsNotEqualsTo,
                Box::new(r),
            ))
        }
    } else {
        Ok(l)
    }
}

fn parse_and_or<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    let l = parse_eq_comp(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::And) | Some(Token::Or) = peeked {
        let op = source.next().unwrap();
        let r = parse_eq_comp(source)?;
        if let Token::And = op {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::And,
                Box::new(r),
            ))
        } else {
            Ok(ExprAst::BinaryExpression(
                Box::new(l),
                Operation::Or,
                Box::new(r),
            ))
        }
    } else {
        Ok(l)
    }
}

fn parse_expression<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<ExprAst, ParsingError> {
    parse_and_or(source)
}

#[inline]
fn expect<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
    t: Token,
) -> Result<(), ParsingError> {
    let n = source.next();
    if let Some(t1) = n {
        if t1 == t {
            Ok(())
        } else {
            Err(ParsingError::ExpectingXGotY(t, Some(t1)))
        }
    } else {
        Err(ParsingError::ExpectingXGotY(t, None))
    }
}

fn parse_prototype<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
    identifier: String,
) -> Result<FunctionPrototype, ParsingError> {
    expect(source, Token::LeftParen)?;
    let mut peeked = source.peek().map(|v| v.clone());
    let mut args = Vec::new();
    while let Some(t @ Token::Identifier(_)) = peeked {
        args.push(t);
        source.next();
        peeked = source.peek().map(|v| v.clone());
        if let Some(Token::Comma) = peeked {
            source.next();
            peeked = source.peek().map(|v| v.clone());
        }
    }
    expect(source, Token::RightParen)?;
    Ok(FunctionPrototype(identifier, Vec::new()))
}

fn parse_def<I: Iterator<Item = Token>>(
    source: &mut Peekable<I>,
) -> Result<Statement, Vec<ParsingError>> {
    match source.next() {
        Some(Token::Identifier(i)) => {
            let prototype = parse_prototype(source, i);
            let body = parse_expression(source);
            match (prototype, body) {
                (Ok(p), Ok(b)) => Ok(Statement::Function(p, b)),
                (Ok(_), Err(e)) => Err(vec![e]),
                (Err(e), Ok(_)) => Err(vec![e]),
                (Err(e1), Err(e2)) => Err(vec![e1, e2]),
            }
        }
        t => Err(vec![ParsingError::ExpectingNumberOrIdentifier(t)]),
    }
}

impl TryFrom<TokenList> for Ast {
    type Error = Vec<ParsingError>;
    fn try_from(tokens: TokenList) -> Result<Ast, Self::Error> {
        let mut statements = Vec::new();
        let mut source = tokens.0.into_iter().peekable();
        let mut errors = Vec::new();
        let mut peeked = source.peek().map(|v| v.clone());
        while let Some(n) = peeked {
            match n {
                Token::Def => {
                    source.next();
                    match parse_def(&mut source) {
                        Ok(f) => statements.push(f),
                        Err(e) => errors.extend(e),
                    }
                }
                Token::Extern => {
                    source.next();
                    match source.next() {
                        Some(Token::Identifier(i)) => match parse_prototype(&mut source, i) {
                            Ok(s) => {
                                statements.push(Statement::ExternFunction(s));
                            }
                            Err(e) => errors.push(e),
                        },
                        e => {
                            errors.push(ParsingError::ExpectingNumberOrIdentifier(e));
                        }
                    }
                }
                _ => match parse_expression(&mut source) {
                    Ok(e) => statements.push(Statement::ExpressionStatement(e)),
                    Err(e) => errors.push(e),
                },
            }
            peeked = source.peek().map(|v| v.clone());
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(Ast(statements))
        }
    }
}

fn main() {
    let mut content = Vec::new();
    stdin().read_to_end(&mut content).unwrap();
    let tokens = TokenList::from(String::from_utf8(content).unwrap());
    println!("Your lexer output is: {:?}", tokens);
    let ast = Ast::try_from(tokens).unwrap();
    println!("Your parsing result is: {:?}", ast);
    let mut context = Context::new("LLVM Module");
    ast.0.iter().for_each(|v| {
        v.codegen(&mut context).unwrap();
    });
    context.print();
}
