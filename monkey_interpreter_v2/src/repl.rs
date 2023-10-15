use crate::{lexer, token::Token};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();
    loop {
        print!("{} ", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read in line");

        let mut l = lexer::Lexer::new(&buffer);
        let mut tok = l.next_token();
        while tok != Token::EOF {
            println!("{:?}", tok);
            tok = l.next_token();
        }
        buffer.clear();
    }
}
