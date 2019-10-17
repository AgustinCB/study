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
    let lexems = lexer.parse().unwrap();
    println!("LEXEMS: {:?}", lexems);
    Ok(())
}
