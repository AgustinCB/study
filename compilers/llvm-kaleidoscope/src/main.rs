enum Token {
    EOF,
    Def,
    Extern,
    Identifier(String),
    Number(f64),
}

struct TokenList(Vec<Token>);

impl From<String> for TokenList {
    fn from(s: String) -> TokenList {
        let mut res = Vec::new();
        let mut chars = s.chars().peekable();
        while chars.peek().map(|i| i.clone()).is_some() {
            let c = chars.next().unwrap();
            match c {
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
        res.push(Token::EOF);
        TokenList(res)
    }
}

fn main() {
    println!("Hello, world!");
}
