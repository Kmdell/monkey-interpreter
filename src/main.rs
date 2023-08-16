use monkey_intepreter::*;
use whoami;

fn main() {
    println!(
        "Hello {}! This is the Monkey Language Intepreter!",
        whoami::username()
    );
    println!("Feel free to type in commands");
    repl::start();
}
