use std::convert::TryFrom;
use std::io::{Read, stdin};
use std::iter::Peekable;

#[derive(Debug)]
enum ParsingError {
    UnexpectedToken(Token),
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
}

#[derive(Debug)]
enum Operation {
    Sum,
    Difference,
    Multiplication,
    Division,
    Modulo,
}

#[derive(Debug)]
enum UnaryOperation {
    ChangeSign,
}

#[derive(Debug)]
struct FunctionPrototype(String, Vec<String>);

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
    Function(FunctionPrototype, ExprAst),
}

#[derive(Debug)]
struct TokenList(Vec<Token>);
#[derive(Debug)]
struct Ast(Vec<Statement>);

fn accumulate_while<T, I: Iterator<Item=T>, F: Fn(&T) -> bool + Copy>(source: &mut Peekable<I>, zero: T, condition: F) -> Vec<T> {
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
            println!("{}", c);
            match c {
                '(' => { res.push(Token::LeftParen) },
                ')' => { res.push(Token::RightParen) },
                ',' => { res.push(Token::Comma) },
                '+' => { res.push(Token::Plus) },
                '-' => { res.push(Token::Dash) },
                '*' => { res.push(Token::Star) },
                '/' => { res.push(Token::Slash) },
                '%' => { res.push(Token::Percent) },
                _ if c.is_whitespace() => {},
                _ if c.is_ascii_alphabetic() => {
                    let ident: String = accumulate_while(&mut chars, c,  |c| {
                        c.is_ascii_alphanumeric()
                    }).into_iter().collect();
                    res.push(match ident.as_str() {
                        "def" => Token::Def,
                        "extern" => Token::Extern,
                        i => Token::Identifier(i.to_owned()),
                    });
                }
                _ if c.is_ascii_digit() || c == '.' => {
                    let number: String = accumulate_while(&mut chars, c, |c| {
                        c.is_ascii_digit() || *c == '.'
                    }).into_iter().collect();
                    println!("{} {:?}", number, chars.peek());
                    res.push(Token::Number(str::parse(&number).unwrap()));
                }
                _ => {},
            }
        }
        TokenList(res)
    }
}

fn parse_call_args<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<Vec<Box<ExprAst>>, ParsingError> {
    let mut res = Vec::new();
    let mut n = source.peek().map(|v| v.clone()).ok_or(ParsingError::UnexpectedEndOfFile)?;
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
        n = source.peek().map(|v| v.clone()).ok_or(ParsingError::UnexpectedEndOfFile)?;
    }
    source.next();
    Ok(res)
}

fn parse_operator<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    match source.next() {
        Some(Token::Number(n)) => { Ok(ExprAst::Number(n)) }
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
        t => { Err(ParsingError::ExpectingNumberOrIdentifier(t)) }
    }
}

fn parse_group<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    let peeked = source.peek().map(|v| v.clone());
    match peeked {
        Some(Token::LeftParen) => {
            source.next();
            let exp = parse_expression(source)?;
            expect(source, Token::RightParen)?;
            Ok(ExprAst::Grouping(Box::new(exp)))
        }
        Some(_) => { parse_operator(source) }
        None => Err(ParsingError::UnexpectedEndOfFile)
    }
}

fn parse_unary<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    let peeked = source.peek().map(|v| v.clone());
    match peeked {
        Some(Token::Dash) => {
            source.next();
            let op = parse_group(source)?;
            Ok(ExprAst::Unary(UnaryOperation::ChangeSign, Box::new(op)))
        }
        Some(_) => { parse_group(source) }
        None => Err(ParsingError::UnexpectedEndOfFile)
    }
}

fn parse_mult_div<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    let l = parse_unary(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::Star) | Some(Token::Slash) | Some(Token::Percent) = peeked {
        let op = source.next().unwrap();
        let r = parse_unary(source)?;
        if let Token::Star = op {
            Ok(ExprAst::BinaryExpression(Box::new(l), Operation::Multiplication, Box::new(r)))
        } else if let Token::Slash = op {
            Ok(ExprAst::BinaryExpression(Box::new(l), Operation::Division, Box::new(r)))
        } else {
            Ok(ExprAst::BinaryExpression(Box::new(l), Operation::Modulo, Box::new(r)))
        }
    } else {
        Ok(l)
    }
}

fn parse_sum_rest<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    let l = parse_mult_div(source)?;
    let peeked = source.peek().map(|v| v.clone());
    if let Some(Token::Plus) | Some(Token::Dash) = peeked {
        let op = source.next().unwrap();
        let r = parse_mult_div(source)?;
        if let Token::Plus = op {
            Ok(ExprAst::BinaryExpression(Box::new(l), Operation::Sum, Box::new(r)))
        } else {
            Ok(ExprAst::BinaryExpression(Box::new(l), Operation::Difference, Box::new(r)))
        }
    } else {
        Ok(l)
    }
}

fn parse_expression<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    parse_sum_rest(source)
}

#[inline]
fn expect<I: Iterator<Item=Token>>(source: &mut Peekable<I>, t: Token) -> Result<(), ParsingError> {
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

#[inline]
fn handle_error<T>(r: Result<T, ParsingError>, errors: &mut Vec<ParsingError>) {
    if let Err(e) = r {
        errors.push(e)
    }
}

fn parse_prototype<I: Iterator<Item=Token>>(
    source: &mut Peekable<I>,
    identifier: String,
    errors: &mut Vec<ParsingError>,
) -> FunctionPrototype {
    handle_error(expect(source, Token::LeftParen), errors);
    let mut peeked = source.peek().map(|v| v.clone());
    let mut args = Vec::new();
    while let Some(t@Token::Identifier(_)) = peeked {
        args.push(t);
        source.next();
        peeked = source.peek().map(|v| v.clone());
        if let Some(Token::Comma) = peeked {
            source.next();
            peeked = source.peek().map(|v| v.clone());
        }
    }
    handle_error(expect(source, Token::RightParen), errors);
    FunctionPrototype(identifier, Vec::new())
}

impl TryFrom<TokenList> for Ast {
    type Error = Vec<ParsingError>;
    fn try_from(tokens: TokenList) -> Result<Ast, Self::Error> {
        let mut statements = Vec::new();
        let mut source = tokens.0.into_iter().peekable();
        let mut errors = Vec::new();
        while let Some(n) = source.next() {
            let next = n.clone();
            let peeked = source.peek().map(|v| v.clone());
            match (next, peeked) {
                (Token::Def, Some(Token::Identifier(i))) => {
                    source.next();
                    let prototype = parse_prototype(&mut source, i, &mut errors);
                    let body = parse_expression(&mut source);
                    match body {
                        Ok(b) => { statements.push(Statement::Function(prototype, b)) }
                        Err(e) => { errors.push(e) },
                    }
                }
                (Token::Def, Some(t)) => {
                    errors.push(ParsingError::UnexpectedToken(t));
                    parse_expression(&mut source);
                }
                (Token::Def, None) => {
                    errors.push(ParsingError::UnexpectedEndOfFile);
                }
                _ => {
                    match parse_expression(&mut source) {
                        Ok(e) => statements.push(Statement::ExpressionStatement(e)),
                        Err(e) => errors.push(e),
                    }
                }
            }
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
    let ast = Ast::try_from(tokens);
    println!("Your parsing result is: {:?}", ast)
}
