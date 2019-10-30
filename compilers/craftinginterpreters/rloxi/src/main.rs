#![feature(exact_size_is_empty)]
use std::io::{self, Read};

mod lexer;
mod parser;
mod types;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer)?;

    let mut lexer = lexer::Lexer::new(buffer, "stdin".to_owned());
    let lexems = lexer.parse();
    match lexems {
        Ok(ts) => {
            let mut parser = parser::Parser::new(ts.into_iter().peekable());
            let result = parser.parse();
            match result {
                Ok(es) => println!("{:?}", es),
                Err(es) => eprintln!("{:?}", es),
            }
        },
        Err(e) => eprintln!("{:?}", e),
    };
    Ok(())
}
