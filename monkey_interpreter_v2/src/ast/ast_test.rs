use super::{Expr, Node, Program, Stmt};

#[test]
fn test_string() {
    let prog = Program {
        stmts: vec![Stmt::LetStatement {
            ident: Expr::Identifier {
                name: "myVar".into(),
            },
            value: Expr::Identifier {
                name: "anotherVar".into(),
            },
        }],
    };

    if &prog.string() != "let myVar = anotherVar;" {
        panic!("program.string() wrong. got={}", prog.string());
    }
}
