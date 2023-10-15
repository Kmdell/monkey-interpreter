mod lexer;
mod repl;
mod token;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!\nFeel free to type in commands",
        whoami::username()
    );
    repl::start();
}
