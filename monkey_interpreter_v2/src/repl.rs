use crate::{ast::Node, lexer::Lexer, parser::Parser};
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

        let l = Lexer::new(&buffer);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if p.errors().len() != 0 {
            print_parser_errors(p.errors());
            continue;
        }

        println!("{}", program.string());

        buffer.clear();
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    errors.iter().for_each(|m| println!("\t{m}"));
}
