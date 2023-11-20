use super::{Expr, Node, Program, Stmt, TNode};

#[test]
fn test_string() {
    let prog = Program {
        stmts: vec![Node::Statement(Box::new(Stmt::LetStatement {
            ident: Node::Expression(Box::new(Expr::Identifier {
                name: "myVar".into(),
            })),
            value: Node::Expression(Box::new(Expr::Identifier {
                name: "anotherVar".into(),
            })),
        }))],
    };

    if &prog.string() != "let myVar = anotherVar;" {
        panic!("program.string() wrong. got={}", prog.string());
    }
}
