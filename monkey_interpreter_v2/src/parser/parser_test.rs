use crate::{
    ast::{Expr, Node, Program, Stmt},
    lexer::Lexer,
    token::Str,
};

use super::Parser;

enum Expected {
    Int(i64),
    String(String),
    Boolean(bool),
}

#[test]
fn test_let_statements() {
    let test = vec!["x", "y", "foobar"];

    let input = "
let x = 5;
let y = 10;
let foobar = 838384;
"
    .to_string();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() == 0 {
        panic!("parse_program returned empty");
    }

    if prog.stmts.len() != 3 {
        panic!(
            "program stmts does not contain 3 statements. got={}",
            prog.stmts.len()
        );
    }

    for (i, tt) in test.iter().enumerate() {
        let stmt = &prog.stmts[i];
        test_let_statement(stmt, *tt);
    }
}

#[test]
fn test_return_statements() {
    let input = "
return 5;
return 10;
return 993322;
"
    .to_string();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements. got={}",
            prog.stmts.len()
        );
    }

    for stmt in &prog.stmts {
        let Stmt::ReturnStatement { .. } = stmt else {
            panic!("stmt not a Stmt::ReturnStatement, got={:?}", stmt);
        };
        if stmt.token_literal() != "return" {
            panic!(
                "Stmt::ReturnStatement.token_literal() not 'return', got='{}'",
                stmt.token_literal()
            );
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;".to_string();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() != 1 {
        panic!(
            "program has not enough statements, got={}",
            prog.stmts.len()
        );
    }

    let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
        panic!(
            "prog.stmts[0] is not a ExpressionStatement. got {:?} instead",
            prog.stmts[0]
        );
    };

    let Expr::Identifier { ref name } = expr else {
        panic!("exp is not a Identifier. got={:?}", expr);
    };

    if &(**name) != "foobar" {
        panic!("ident.name not foobar. got={}", name);
    }

    if expr.token_literal() != "foobar" {
        panic!(
            "ident.token_literal() not foobar. got={}",
            expr.token_literal()
        );
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;".to_string();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() != 1 {
        panic!(
            "program does not have enough statements. got={}",
            prog.stmts.len()
        );
    }

    let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
        panic!(
            "program.stmts[0] is not ExpressionStatement. got={:?}",
            prog.stmts[0]
        );
    };

    let Expr::IntegerLiteral { value } = expr else {
        panic!("expr is not IntegerLiteral. got={:?}", expr);
    };

    if value.1 != 5 {
        panic!("IntegerLiteral.value is not 5, got={}", value.1);
    }

    if expr.token_literal() != "5" {
        panic!(
            "IntegerLiteral.token_literal() not '5'. got='{}'",
            expr.token_literal()
        );
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct PrefixTest(Str, Str, Expected);
    let tests = vec![
        PrefixTest("!5;".into(), "!".into(), Expected::Int(5)),
        PrefixTest("-15;".into(), "-".into(), Expected::Int(15)),
        PrefixTest("!true;".into(), "!".into(), Expected::Boolean(true)),
        PrefixTest("!false;".into(), "!".into(), Expected::Boolean(false)),
    ];

    for tt in tests {
        let l = Lexer::new(&(*tt.0));
        let mut p = Parser::new(l);

        let prog = p.parse_program();
        check_parser_errors(&p);

        if prog.stmts.len() != 1 {
            panic!(
                "program.stmts does not contain {} statements. got={}",
                1,
                prog.stmts.len()
            );
        }

        let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
            panic!(
                "program.stmts[0] is not a ExpressionStatement. got={:?}",
                prog.stmts[0]
            );
        };

        let Expr::PrefixExpression {
            operator, right, ..
        } = expr
        else {
            panic!("stmt is not an PrefixExpression. got={:?}", expr);
        };

        if &(**operator) != &(*tt.1) {
            panic!("expr.operator is not '{}'. got='{}'", tt.1, operator);
        }

        test_literal_expression(&right, tt.2);
    }
}

#[test]
fn test_parsing_infix_expression() {
    struct InfixTest<'a>(&'a str, Expected, &'a str, Expected);
    let tests = vec![
        InfixTest("5 + 5;", Expected::Int(5), "+", Expected::Int(5)),
        InfixTest("5 - 5;", Expected::Int(5), "-", Expected::Int(5)),
        InfixTest("5 * 5;", Expected::Int(5), "*", Expected::Int(5)),
        InfixTest("5 / 5;", Expected::Int(5), "/", Expected::Int(5)),
        InfixTest("5 > 5;", Expected::Int(5), ">", Expected::Int(5)),
        InfixTest("5 < 5;", Expected::Int(5), "<", Expected::Int(5)),
        InfixTest("5 == 5;", Expected::Int(5), "==", Expected::Int(5)),
        InfixTest("5 != 5;", Expected::Int(5), "!=", Expected::Int(5)),
        InfixTest(
            "true == true;",
            Expected::Boolean(true),
            "==",
            Expected::Boolean(true),
        ),
        InfixTest(
            "true != false;",
            Expected::Boolean(true),
            "!=",
            Expected::Boolean(false),
        ),
        InfixTest(
            "false == false;",
            Expected::Boolean(false),
            "==",
            Expected::Boolean(false),
        ),
    ];

    for tt in tests {
        let l = Lexer::new(&(*tt.0));
        let mut p = Parser::new(l);

        let prog = p.parse_program();
        check_parser_errors(&p);

        if prog.stmts.len() != 1 {
            panic!(
                "program.stmts does not contain {}, got={}",
                1,
                prog.stmts.len()
            );
        }

        let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
            panic!(
                "prog.stmts[0] is not ExpressionStatement. got={:?}",
                prog.stmts[0]
            );
        };

        let Expr::InfixExpression { .. } = expr else {
            panic!("expr is not InfixExpression. got={:?}", expr);
        };

        test_infix_expression(expr, tt.1, tt.2, tt.3)
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let prog = p.parse_program();
        check_parser_errors(&p);

        if prog.string() != tt.1 {
            panic!("expected='{}', got='{}'", tt.1, prog.string())
        }
    }
}

#[test]
fn test_boolean_expression() {
    let tests = vec![("true", true), ("false", false)];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let prog = p.parse_program();
        check_parser_errors(&p);

        if prog.stmts.len() != 1 {
            panic!(
                "program.stmts does not contain {}, got={}",
                1,
                prog.stmts.len()
            );
        }

        let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
            panic!(
                "prog.stmts[0] is not ExpressionStatement. got={:?}",
                prog.stmts[0]
            );
        };

        let Expr::Boolean { .. } = expr else {
            panic!("expr is not Boolean. got={:?}", expr);
        };

        test_boolean(&expr, tt.1);
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() != 1 {
        panic!(
            "program.stmts does not contain {}, got={}",
            1,
            prog.stmts.len()
        );
    }

    let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
        panic!(
            "prog.stmts[0] is not ExpressionStatement. got={:?}",
            prog.stmts[0]
        );
    };

    let Expr::IfExpression {
        condition,
        consequence,
        alternative,
        ..
    } = expr
    else {
        panic!("expr is not IfExpression. got={:?}", expr);
    };

    test_infix_expression(
        &(**condition),
        Expected::String("x".into()),
        "<",
        Expected::String("y".into()),
    );

    let Stmt::BlockStatement { ref stmts } = **consequence else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }

    let Stmt::ExpressionStatement { ref expr, .. } = *stmts[0] else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", expr)
    };

    test_identifier(expr, "x");

    if **alternative != Stmt::Nil {
        panic!("expr.alternative was not nil. got={:?}", alternative);
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    check_parser_errors(&p);

    if prog.stmts.len() != 1 {
        panic!(
            "program.stmts does not contain {}, got={}",
            1,
            prog.stmts.len()
        );
    }

    let Stmt::ExpressionStatement { ref expr, .. } = prog.stmts[0] else {
        panic!(
            "prog.stmts[0] is not ExpressionStatement. got={:?}",
            prog.stmts[0]
        );
    };

    let Expr::IfExpression {
        condition,
        consequence,
        alternative,
        ..
    } = expr
    else {
        panic!("expr is not IfExpression. got={:?}", expr);
    };

    test_infix_expression(
        &(**condition),
        Expected::String("x".into()),
        "<",
        Expected::String("y".into()),
    );

    let Stmt::BlockStatement { ref stmts } = **consequence else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }

    let Stmt::ExpressionStatement { ref expr, .. } = *stmts[0] else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", expr)
    };

    test_identifier(expr, "x");

    let Stmt::BlockStatement { ref stmts } = **alternative else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }

    let Stmt::ExpressionStatement { ref expr, .. } = *stmts[0] else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", expr)
    };

    test_identifier(expr, "y");
}

fn test_let_statement(stmt: &Stmt, literal: &str) {
    if let Stmt::LetStatement { ident, .. } = stmt {
        if let Expr::Identifier { ref name } = ident {
            if &(**name) != literal {
                panic!("LetStmt ident name not {}, got={}", literal, name);
            }
            if ident.token_literal() != literal {
                panic!("Identifier token_literal() not {}, got={}", literal, name);
            }
        } else {
            panic!("ident is not an identifier, got={:?}", ident);
        }
    } else {
        panic!("stmt is not a LetStatement, got={:?}", stmt);
    }
}

fn test_literal_expression(expr: &Expr, expected: Expected) {
    match expected {
        Expected::Int(int) => test_integer_literal(expr, int),
        Expected::String(string) => test_identifier(expr, &string),
        Expected::Boolean(boolean) => test_boolean(expr, boolean),
    }
}

fn test_integer_literal(expr: &Expr, value: i64) {
    let Expr::IntegerLiteral { value: expr_value } = expr else {
        panic!("expr is not an IntegerLiteral");
    };

    if expr_value.1 != value {
        panic!("integ.value not {}. got={}", value, expr_value.1);
    }

    if &(*expr_value.0) != &format!("{}", value) {
        panic!(
            "integ.token_literal() not '{}'. got='{}'",
            value, expr_value.0
        );
    }
}

fn test_infix_expression(expr: &Expr, exp_left: Expected, op: &str, exp_right: Expected) {
    let Expr::InfixExpression {
        left,
        operator,
        right,
        ..
    } = expr
    else {
        panic!("expr is not InfixExpression. got={:?}", expr);
    };

    test_literal_expression(left, exp_left);

    if &(**operator) != op {
        panic!("expr.operator is not {}. got {}", op, operator);
    }

    test_literal_expression(right, exp_right)
}

fn test_identifier(expr: &Expr, value: &str) {
    let Expr::Identifier { name } = expr else {
        panic!("expr not an Expr::Identifier. got={:?}", expr);
    };

    if &(**name) != value {
        panic!("ident.value not '{}'. got='{}'", value, name);
    }

    if expr.token_literal() != value {
        panic!(
            "ident.token_literal not '{}'. got='{}'",
            value,
            expr.token_literal()
        );
    }
}

fn test_boolean(expr: &Expr, bool_value: bool) {
    let Expr::Boolean { value } = expr else {
        panic!("expr not an Expr::Boolean. got={:?}", expr);
    };

    if *value != bool_value {
        panic!("ident.value not '{}'. got='{}'", bool_value, value);
    }

    if expr.token_literal() != bool_value.to_string() {
        panic!(
            "ident.token_literal not '{}'. got='{}'",
            value,
            expr.token_literal()
        );
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.len() == 0 {
        return;
    }

    eprintln!("Parser has {} errors", errors.len());
    errors.iter().for_each(|m| eprintln!("parser error: {}", m));
    panic!()
}
