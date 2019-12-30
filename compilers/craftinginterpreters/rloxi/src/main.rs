#![feature(exact_size_is_empty)]
use crate::interpreter::Interpreter;
use crate::resolver::Resolver;
use crate::types::ProgramError;
use std::io::{self, Read};

mod interpreter;
mod lexer;
mod parser;
mod resolver;
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
        .and_then(|ss| {
            let mut interpreter = Interpreter::new(ss);
            let mut resolver = Resolver::new(&mut interpreter);
            let passes: Vec<&mut dyn resolver::Pass> = vec![&mut resolver];
            passes
                .into_iter()
                .map(|p| p.run())
                .collect::<Result<Vec<()>, ProgramError>>()
                .map(|_| interpreter)
                .map_err(|e| vec![e])
        })
        .and_then(|interpreter| interpreter.run().map_err(|e| vec![e]));
    match result {
        Ok(_) => {}
        Err(es) => es.iter().for_each(|e| eprintln!("{}", e)),
    }
    Ok(())
}
