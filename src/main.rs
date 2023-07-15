use whoami;
use monkey_intepreter::*;

fn main() {
    println!("Hello {}! This is the Monkey Language Intepreter!", whoami::username());
    println!("Feel free to type in commands");
    repl::start();
}

