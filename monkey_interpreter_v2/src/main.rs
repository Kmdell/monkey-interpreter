mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!\nFeel free to type in commands",
        whoami::username()
    );
    repl::start();
}
