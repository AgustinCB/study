#![feature(exact_size_is_empty)]
use crate::interpreter::Evaluable;
use crate::types::State;
use std::io::{self, Read};

mod interpreter;
mod lexer;
mod parser;
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
        .and_then(|ss| {
            let mut current_state = State::default();
            for s in ss {
                match s.evaluate(current_state) {
                    Ok((next_state, _)) => {
                        current_state = next_state;
                    }
                    Err(e) => return Err(vec![e]),
                }
            }
            Ok(())
        });
    match result {
        Ok(_) => {}
        Err(es) => eprintln!("{:?}", es),
    }
    Ok(())
}
