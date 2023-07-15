use super::{
    token::*,
    lexer::*,    
};
use std::io::{self, Write};

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{} ", PROMPT);
        io::stdout().flush().unwrap();

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).expect("Failed to read in line");
        
        let mut lex = Lexer::new(buffer);

        let mut tok = lex.next_token();
        while !tok.token_type.eq(EOF) {
            println!("{:?}", tok);
            tok = lex.next_token();
        }
    }
}
