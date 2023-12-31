use crate::{ast::*, environment::Environment, evaluator::*, lexer::*, parser::*};
use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

const PROMPT: &str = ">>";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'"#;

pub fn start() {
    let env = Rc::new(RefCell::new(Environment::new()));
    let builtins = builtins::new_builtins();
    loop {
        print!("{} ", PROMPT);
        io::stdout().flush().unwrap();

        let mut buffer = String::new();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read in line");

        let lex = Lexer::new(buffer);
        let mut par = Parser::new(lex);
        let program = par.parse_program().expect("Nothing was able to be parsed");
        if par.errors().len() != 0 {
            print_parser_errors(par.errors());
            continue;
        }

        if let Some(evaluated) = eval(program.into_node().into(), env.clone(), builtins.clone()) {
            println!("{}", evaluated.inspect());
        }
    }
}

fn print_parser_errors(errors: Vec<String>) {
    println!("{}", MONKEY_FACE);
    println!("Woops! We ran into some monkey business here!");
    println!(" | Parser Errors:");
    errors.iter().for_each(|msg| println!(" |--> {msg}"));
}
