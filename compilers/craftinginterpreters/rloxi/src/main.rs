#![feature(exact_size_is_empty)]
#![feature(option_flattening)]
use crate::interpreter::Interpreter;
use std::io::{self, Read};

mod interpreter;
mod lexer;
mod parser;
mod state;
mod types;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer)?;

    let mut lexer = lexer::Lexer::new(buffer, "stdin".to_owned());
    let result = lexer
        .parse()
        .and_then(|ts| {
            let mut parser = parser::Parser::new(ts.into_iter().peekable());
            parser.parse()
        })
        .and_then(|ss| Interpreter::new(ss).run().map_err(|e| vec![e]));
    match result {
        Ok(_) => {}
        Err(es) => es.iter().for_each(|e| eprintln!("{}", e)),
    }
    Ok(())
}
