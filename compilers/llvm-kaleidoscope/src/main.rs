use std::convert::TryFrom;
use std::io::{Read, stdin};
use std::iter::Peekable;

#[derive(Debug)]
enum ParsingError {
    UnexpectedToken(Token),
    UnexpectedEndOfFile,
    ExpectingXGotY(Token, Option<Token>),
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
}

#[derive(Debug)]
enum Operation {
    Sum,
}

#[derive(Debug)]
struct FunctionPrototype(String, Vec<String>);

#[derive(Debug)]
enum ExprAst {
    Number(f64),
    Variable(String),
    BinaryExpression(Box<ExprAst>, Operation, Box<ExprAst>),
    Call(String, Vec<Box<ExprAst>>),
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

impl From<String> for TokenList {
    fn from(s: String) -> TokenList {
        let mut res = Vec::new();
        let mut chars = s.chars().peekable();
        while chars.peek().map(|i| i.clone()).is_some() {
            let c = chars.next().unwrap();
            match c {
                '(' => { res.push(Token::LeftParen) },
                ')' => { res.push(Token::RightParen) },
                ',' => { res.push(Token::Comma) },
                '+' => { res.push(Token::Plus) },
                _ if c.is_whitespace() => {},
                _ if c.is_ascii_alphabetic() => {
                    let ident = (&mut chars)
                        .take_while(|c| c.is_ascii_alphanumeric())
                        .fold(c.to_string(), |mut acc, c| {
                            acc.push(c);
                            acc
                        });
                    res.push(match ident.as_str() {
                        "def" => Token::Def,
                        "extern" => Token::Extern,
                        i => Token::Identifier(i.to_owned()),
                    });
                }
                _ if c.is_ascii_digit() || c == '.' => {
                    let number = (&mut chars)
                        .take_while(|c| c.is_ascii_digit() || *c == '.')
                        .fold(c.to_string(), |mut acc, c| {
                            acc.push(c);
                            acc
                        });
                    res.push(Token::Number(str::parse(&number).unwrap()));
                }
                _ => {},
            }
        }
        TokenList(res)
    }
}

fn parse_expression<I: Iterator<Item=Token>>(source: &mut Peekable<I>) -> Result<ExprAst, ParsingError> {
    let peek = source.peek().map(|v| v.clone());
    match (source.next(), peek) {
        (Some(Token::Number(l)), Some(Token::Plus)) => {
            source.next();
            let r = parse_expression(source)?;
            Ok(ExprAst::BinaryExpression(Box::new(ExprAst::Number(l)), Operation::Sum, Box::new(r)))
        },
        (Some(Token::Number(n)), _) => Ok(ExprAst::Number(n)),
        (None, _) => Err(ParsingError::UnexpectedEndOfFile),
        _ => Ok(ExprAst::Number(0f64)),
    }
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
