use crate::{
    ast::{Expr, Node, Stmt, TNode},
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
    let tests = vec![
        ("let x = 5;", "x", Expected::Int(5)),
        ("let y = true", "y", Expected::Boolean(true)),
        ("let foobar = y", "foobar", Expected::String("y".into())),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let prog = p.parse_program();
        check_parser_errors(&p);

        if prog.stmts.len() == 0 {
            panic!("parse_program returned empty");
        }

        if prog.stmts.len() != 1 {
            panic!(
                "program stmts does not contain 3 statements. got={}",
                prog.stmts.len()
            );
        }

        test_let_statement(&prog.stmts[0], tt.1);

        let Node::Statement(ref stmt) = prog.stmts[0] else {
            panic!("Expected Statement got={:?}", prog.stmts[0]);
        };
        let Stmt::LetStatement { ref value, .. } = **stmt else {
            panic!("Expected LetStatement got={:?}", stmt);
        };
        test_literal_expression(value, tt.2);
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
        let Node::Statement(stmt) = stmt else {
            panic!("Expected Statement got={:?}", stmt);
        };
        let Stmt::ReturnStatement { .. } = **stmt else {
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
    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!(
            "prog.stmts[0] is not a ExpressionStatement. got {:?} instead",
            stmt
        );
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Expression got={:?}", expr);
    };

    let Expr::Identifier { ref name } = **expr else {
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

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!(
            "program.stmts[0] is not ExpressionStatement. got={:?}",
            stmt
        );
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Statement got={:?}", expr);
    };

    let Expr::IntegerLiteral { ref value } = **expr else {
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
        let Node::Statement(ref stmt) = prog.stmts[0] else {
            panic!("Expected Statement got={:?}", prog.stmts[0]);
        };

        let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
            panic!(
                "program.stmts[0] is not a ExpressionStatement. got={:?}",
                stmt
            );
        };

        let Node::Expression(expr) = expr else {
            panic!("Expected Statement got={:?}", expr);
        };

        let Expr::PrefixExpression {
            ref operator,
            ref right,
            ..
        } = **expr
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

        let Node::Statement(ref stmt) = prog.stmts[0] else {
            panic!("Expected Statement got={:?}", prog.stmts[0]);
        };

        let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
            panic!("stmt is not ExpressionStatement. got={:?}", stmt);
        };

        let Node::Expression(in_expr) = expr else {
            panic!("Expected Statement got={:?}", expr);
        };

        let Expr::InfixExpression { .. } = **in_expr else {
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
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
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

        let Node::Statement(ref stmt) = prog.stmts[0] else {
            panic!("Expected Statement got={:?}", prog.stmts[0]);
        };

        let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
            panic!("prog.stmts[0] is not ExpressionStatement. got={:?}", stmt);
        };

        let Node::Expression(in_expr) = expr else {
            panic!("Expected Statement got={:?}", expr);
        };

        let Expr::Boolean { .. } = **in_expr else {
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

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("prog.stmts[0] is not ExpressionStatement. got={:?}", stmt);
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Statement got={:?}", expr);
    };

    let Expr::IfExpression {
        ref condition,
        ref consequence,
        ref alternative,
        ..
    } = **expr
    else {
        panic!("expr is not IfExpression. got={:?}", expr);
    };

    test_infix_expression(
        &condition,
        Expected::String("x".into()),
        "<",
        Expected::String("y".into()),
    );

    let Node::Statement(stmt) = consequence else {
        panic!("Expected Statement got={:?}", consequence);
    };

    let Stmt::BlockStatement { ref stmts } = **stmt else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }

    let Node::Statement(ref stmt) = stmts[0] else {
        panic!("Expected Statement got={:?}", stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", stmt)
    };

    test_identifier(expr, "x");

    let Node::Statement(stmt) = alternative else {
        panic!("Expected Statement got={:?}", alternative);
    };

    if **stmt != Stmt::Nil {
        panic!("expr.alternative was not nil. got={:?}", stmt);
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

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("prog.stmts[0] is not ExpressionStatement. got={:?}", stmt);
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Statement got={:?}", expr);
    };

    let Expr::IfExpression {
        ref condition,
        ref consequence,
        ref alternative,
        ..
    } = **expr
    else {
        panic!("expr is not IfExpression. got={:?}", expr);
    };

    test_infix_expression(
        &condition,
        Expected::String("x".into()),
        "<",
        Expected::String("y".into()),
    );

    let Node::Statement(stmt) = consequence else {
        panic!("Expected Statement got={:?}", consequence);
    };

    let Stmt::BlockStatement { ref stmts } = **stmt else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }
    let Node::Statement(ref stmt) = stmts[0] else {
        panic!("Expected Statement got={:?}", stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", expr)
    };

    test_identifier(expr, "x");

    let Node::Statement(stmt) = alternative else {
        panic!("Expected Statement got={:?}", alternative);
    };

    let Stmt::BlockStatement { ref stmts } = **stmt else {
        panic!("consequence is not a BlockStatement");
    };

    if stmts.len() != 1 {
        panic!("consequence is not 1 statement. got={}", stmts.len());
    }

    let Node::Statement(ref stmt) = stmts[0] else {
        panic!("Expected Statement got={:?}", stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("stmts[0] is not an ExpressionStatement got={:?}", expr)
    };

    test_identifier(expr, "y");
}

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y };";

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
    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!("prog.stmts[0] is not ExpressionStatement. got={:?}", stmt);
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Statement got={:?}", expr);
    };

    let Expr::FunctionLiteral {
        ref parameters,
        ref body,
    } = **expr
    else {
        panic!("Expression is not an FunctionLiteral. got={:?}", expr);
    };

    if parameters.len() != 2 {
        panic!(
            "function literal parameters wrong. want 2, got={}",
            parameters.len()
        );
    }

    test_literal_expression(&parameters[0], Expected::String("x".into()));
    test_literal_expression(&parameters[1], Expected::String("y".into()));

    let Node::Statement(stmt) = body else {
        panic!("Expected Statement got={:?}", body);
    };

    let Stmt::BlockStatement { ref stmts } = **stmt else {
        panic!("function body is not a BlockStatement. got={:?}", stmt);
    };

    if stmts.len() != 1 {
        panic!("function.body.stmts does not have 1. got={}", stmts.len());
    }

    let Node::Statement(ref stmt) = stmts[0] else {
        panic!("Expected Statement got={:?}", stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!(
            "function body stmt is not ExpressionStatement. got={:?}",
            stmts[0]
        );
    };

    test_infix_expression(
        expr,
        Expected::String("x".into()),
        "+",
        Expected::String("y".into()),
    );
}

#[test]
fn test_function_parameters_parsing() {
    let tests = vec![
        ("fn() {}", vec![]),
        ("fn(x) {}", vec!["x"]),
        ("fn(x, y, z) {}", vec!["x", "y", "z"]),
    ];

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

        let Node::Statement(ref stmt) = prog.stmts[0] else {
            panic!("Expected Statement got={:?}", prog.stmts[0]);
        };

        let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
            panic!(
                "prog.stmts[0] is not an ExpressionStatement. got={:?}",
                stmt
            );
        };

        let Node::Expression(expr) = expr else {
            panic!("Expected Statement got={:?}", expr);
        };

        let Expr::FunctionLiteral { ref parameters, .. } = **expr else {
            panic!("expr is not a FunctionLiteral. got={:?}", expr);
        };

        if parameters.len() != tt.1.len() {
            panic!(
                "length of parameters wrong. want={}. got={}",
                tt.1.len(),
                parameters.len()
            );
        }

        for (i, ident) in tt.1.iter().enumerate() {
            test_literal_expression(&parameters[i], Expected::String((*ident).to_string()))
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

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

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Expected Statement got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr, .. } = **stmt else {
        panic!(
            "prog.stmts[0] is not an ExpressionStatement. got={:?}",
            stmt
        );
    };

    let Node::Expression(expr) = expr else {
        panic!("Expected Statement got={:?}", expr);
    };

    let Expr::CallExpression {
        ref function,
        ref arguments,
    } = **expr
    else {
        panic!("expr is not CallExpression. got={:?}", expr);
    };

    test_literal_expression(&function, Expected::String("add".into()));

    if arguments.len() != 3 {
        panic!("wrong length of arguments. want=3, got={}", arguments.len());
    }

    test_literal_expression(&arguments[0], Expected::Int(1));
    test_infix_expression(&arguments[1], Expected::Int(2), "*", Expected::Int(3));
    test_infix_expression(&arguments[2], Expected::Int(4), "+", Expected::Int(5));
}

#[test]
fn test_string_literal_expression() {
    let input = "\"hello world\";";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("Not a statement, got={:?}", prog.stmts[0])
    };

    let Stmt::ExpressionStatement { ref expr } = **stmt else {
        panic!("Not a Expression Stmt, got={:?}", stmt)
    };

    let Node::Expression(ref expr) = expr else {
        panic!("Not a Expression, got={:?}", expr);
    };

    let Expr::StringLiteral { ref value } = **expr else {
        panic!("Not a StringLiteral, got={:?}", expr);
    };

    if &(**value) != "hello world" {
        panic!("Literal value not '{}', got='{}'", "hello world", value)
    }
}

#[test]
fn test_parsing_array_literal() {
    let input = "[1, 2 * 2, 3 + 3]";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();

    check_parser_errors(&p);

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("The prog stmts is not a statemet");
    };

    let Stmt::ExpressionStatement { ref expr } = **stmt else {
        panic!("The stmt is not an expression statement, got={:?}", stmt);
    };

    let Node::Expression(ref expr) = *expr else {
        panic!("The node is not an expr, got={:?}", expr);
    };

    let Expr::ArrayLiteral { ref elements } = **expr else {
        panic!("The expr is not an ArrayLiteral, got={:?}", expr);
    };

    if elements.len() != 3 {
        panic!("len of elements is not 3, got={}", elements.len());
    }

    test_integer_literal(&elements[0], 1);
    test_infix_expression(&elements[1], Expected::Int(2), "*", Expected::Int(2));
    test_infix_expression(&elements[2], Expected::Int(3), "+", Expected::Int(3));
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);

    let Node::Statement(ref stmt) = prog.stmts[0] else {
        panic!("program stmt is not a statement, got={:?}", prog.stmts[0]);
    };

    let Stmt::ExpressionStatement { ref expr } = **stmt else {
        panic!("stmt is not an expression stmt, got={:?}", stmt);
    };

    let Node::Expression(ref expr) = *expr else {
        panic!("node is not an expression, got={:?}", expr);
    };

    let Expr::IndexExpression {
        ref left,
        ref index,
    } = **expr
    else {
        panic!("expr is not an IndexExpression, got={:?}", expr);
    };

    test_identifier(left, "myArray");
    test_infix_expression(index, Expected::Int(1), "+", Expected::Int(1));
}

fn test_let_statement(node: &Node, literal: &str) {
    let Node::Statement(stmt) = node else {
        panic!("node is not a statement, got={:?}", node);
    };
    let Stmt::LetStatement { ref ident, .. } = **stmt else {
        panic!("stmt is not a LetStatement, got={:?}", stmt);
    };
    let Node::Expression(expr) = ident else {
        panic!("node is not an expression, got={:?}", node);
    };
    if let Expr::Identifier { ref name } = **expr {
        if &(**name) != literal {
            panic!("LetStmt ident name not {}, got={}", literal, name);
        }
        if ident.token_literal() != literal {
            panic!("Identifier token_literal() not {}, got={}", literal, name);
        }
    } else {
        panic!("ident is not an identifier, got={:?}", ident);
    }
}

fn test_literal_expression(expr: &Node, expected: Expected) {
    match expected {
        Expected::Int(int) => test_integer_literal(expr, int),
        Expected::String(string) => test_identifier(expr, &string),
        Expected::Boolean(boolean) => test_boolean(expr, boolean),
    }
}

fn test_integer_literal(node: &Node, value: i64) {
    let Node::Expression(expr) = node else {
        panic!("node is not an expression, got={:?}", node);
    };
    let Expr::IntegerLiteral {
        value: ref expr_value,
    } = **expr
    else {
        panic!("expr is not an IntegerLiteral, got={:?}", expr);
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

fn test_infix_expression(node: &Node, exp_left: Expected, op: &str, exp_right: Expected) {
    let Node::Expression(ref expr) = node else {
        panic!("Node is not an expression, got={:?}", node);
    };
    let Expr::InfixExpression {
        ref left,
        ref operator,
        ref right,
        ..
    } = **expr
    else {
        panic!("expr is not InfixExpression. got={:?}", expr);
    };

    test_literal_expression(left, exp_left);

    if &(**operator) != op {
        panic!("expr.operator is not {}. got {}", op, operator);
    }

    test_literal_expression(right, exp_right)
}

fn test_identifier(node: &Node, value: &str) {
    let Node::Expression(expr) = node else {
        panic!("node is not an expression, got={:?}", node);
    };
    let Expr::Identifier { ref name } = **expr else {
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

fn test_boolean(node: &Node, bool_value: bool) {
    let Node::Expression(expr) = node else {
        panic!("node is not an expression, got={:?}", node);
    };
    let Expr::Boolean { value } = **expr else {
        panic!("expr not an Expr::Boolean. got={:?}", expr);
    };

    if value != bool_value {
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
