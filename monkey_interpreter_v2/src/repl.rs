use crate::{
    ast::Node,
    environment::Environment,
    evaluator::{self, builtins::new_builtins},
    lexer::Lexer,
    object::Object,
    parser::Parser,
};
use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    let builtins = new_builtins();

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
            buffer.clear();
            continue;
        }

        let eval = evaluator::eval(&Node::Program(Box::new(program)), &env, &builtins);
        if eval != Object::Null {
            println!("{}", eval.inspect());
        }

        buffer.clear();
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    errors.iter().for_each(|m| println!("\t{m}"));
}
