use crate::{ast::*, lexer::*, parser::*, token::*};

struct IdentTest {
    expected_identifier: String,
}

impl IdentTest {
    fn new(expected_identifier: &str) -> Self {
        IdentTest {
            expected_identifier: expected_identifier.to_string(),
        }
    }
}

fn test_integer_literal(il: &Box<dyn Expression>, value: i64) {
    let integ = il
        .into_integer_literal()
        .unwrap_or_else(|x| panic!("{}", x));
    if integ.value != value {
        panic!("integ.value not {}, got={}", value, integ.value);
    }

    if !integ.token_literal().eq(&format!("{}", value)) {
        panic!(
            "integ.token_literal() not {}, got={}",
            value,
            integ.token_literal()
        );
    }
}

fn test_identifier(exp: &Box<dyn Expression>, value: String) {
    let ident = exp.into_identifier();
    if let Err(e) = ident {
        panic!("{}", e);
    }

    let ident = ident.unwrap();
    if !ident.value.eq(&value) {
        panic!("Ident.value not {}, got ={}", value, ident.value);
    }

    if ident.token_literal() != value {
        panic!(
            "ident.token_literal not {}, got={}",
            value,
            ident.token_literal()
        );
    }
}

fn test_let_statement(stmt: &Box<dyn Statement>, test: String) {
    if !stmt.token_literal().eq("let") {
        panic!("s.token_literal is not 'let'. got={}", stmt.token_literal());
    }

    let let_stmt = stmt
        .into_let()
        .unwrap_or_else(|x| panic!("s is not a LetStatement, got={}", x));

    if !let_stmt.name.value.eq(&test) {
        panic!(
            "let_stmt.value.name not '{}', got='{}'",
            test, let_stmt.name.value
        );
    }

    if !let_stmt.name.token_literal().eq(&test) {
        panic!(
            "let_stmt.name.token_literal() not '{}', got='{}'",
            test,
            let_stmt.name.token_literal()
        );
    }
}

fn check_parser_errors(program: &Parser) {
    let errors = program.errors();
    if errors.is_empty() {
        return;
    }

    eprintln!("Parser had {} errors", errors.len());
    errors.iter().for_each(|error| eprintln!("{}", error));
    panic!("Parser ran into errors");
}

fn test_literal_expression(exp: &Box<dyn Expression>, expected: String) {
    if let Ok(int) = expected.parse::<i64>() {
        test_integer_literal(&exp, int);
    } else if let Ok(boolean) = expected.parse::<bool>() {
        test_boolean_literal(&exp, boolean);
    } else {
        test_identifier(&exp, expected);
    }
}

fn test_infix_expression(exp: &Box<dyn Expression>, left: String, operator: String, right: String) {
    let op_exp = exp.into_infix().unwrap_or_else(|e| panic!("{}", e));

    test_literal_expression(op_exp.left.as_ref().unwrap(), left);

    if !op_exp.operator.eq(&operator) {
        panic!("exp.operator is not {}, got={}", operator, op_exp.operator);
    }

    test_literal_expression(op_exp.right.as_ref().unwrap(), right);
}

fn test_boolean_literal(exp: &Box<dyn Expression>, value: bool) {
    let bo = exp.into_bool().unwrap_or_else(|e| panic!("{}", e));

    if bo.value != value {
        panic!("bo.value not {}, got={}", value, bo.value);
    }

    if !bo.token_literal().eq(&value.to_string()) {
        panic!("bo.token_literal not {}, got={}", value, bo.token_literal());
    }
}
#[test]
fn test_let_statement_parsing() {
    let input = "
let x = 5;
let y = 10;
let foobar = 838383;
"
    .to_string();

    let lexer: Lexer = Lexer::new(input);
    let mut parser: Parser = Parser::new(lexer);

    let program: Option<Program> = parser.parse_program();
    check_parser_errors(&parser);
    if program.is_none() {
        panic!("parsed_program() returned a None");
    }

    let program = program.unwrap();
    if program.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements, instead got {}",
            program.statements.len()
        );
    }

    let tests = vec!["x".into(), "y".into(), "foobar".into()];

    tests.into_iter().enumerate().for_each(|(i, test)| {
        let stmt = program.statements.get(i).unwrap();
        test_let_statement(stmt, test);
    });
}

#[test]
fn test_return_statement_parsing() {
    let input = "
return 5;
return 10;
return 993322;
"
    .to_string();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);

    if program.is_none() {
        panic!("parsed_program() returned a None");
    }

    let program = program.unwrap();
    if program.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements, instead got {}",
            program.statements.len()
        );
    }

    program.statements.into_iter().for_each(|stmt| {
        let return_stmt = stmt
            .into_return()
            .unwrap_or_else(|x| panic!("s is not a ReturnStatment, got={}", x));

        if !return_stmt.token_literal().eq("return") {
            panic!(
                "stmt.token_literal is not 'return', got='{}'",
                return_stmt.token_literal()
            )
        }
    });
}

#[test]
fn test_string() {
    let program = Program {
        statements: vec![Box::new(LetStatement {
            token: Token {
                token_type: LET.to_string(),
                literal: "let".to_string(),
            },
            name: Identifier {
                token: Token {
                    token_type: IDENT.to_string(),
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            },
            value: Some(Box::new(Identifier {
                token: Token {
                    token_type: IDENT.to_string(),
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            })),
        })],
    };

    if !program.to_string().eq("let myVar = anotherVar;") {
        panic!("program.to_string() wrong. got='{}'", program.to_string())
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;".to_string();
    let l = Lexer::new(input);

    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    if program.is_none() {
        panic!("parsed_program() returned a None");
    }

    let program = program.unwrap();
    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statements, instead got {}",
            program.statements.len()
        );
    }

    let exp_stmt = program.statements[0]
        .into_expression()
        .unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

    if let Some(exp) = &exp_stmt.expression {
        let ident = exp
            .into_identifier()
            .unwrap_or_else(|_| panic!("Expression is not an Identifier"));
        if !ident.value.eq("foobar") {
            panic!("ident.value is not '{}' got='{}'", "foobar", ident.value);
        }

        if !ident.token_literal().eq("foobar") {
            panic!(
                "ident.token_literal() is not '{}', got='{}'",
                "foobar",
                ident.token_literal()
            );
        }
    } else {
        panic!("The Expression is None");
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;".to_string();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let program = program.unwrap();
    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statements, instead got {}",
            program.statements.len()
        );
    }

    let exp_stmt = program.statements[0]
        .into_expression()
        .unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

    if let Some(exp) = &exp_stmt.expression {
        let literal = exp
            .into_integer_literal()
            .unwrap_or_else(|x| panic!("{}", x));
        if literal.value != 5 {
            panic!("literal.value is not '{}' got='{}'", 5, literal.value);
        }

        if !literal.token_literal().eq("5") {
            panic!(
                "ident.token_literal() is not '{}', got='{}'",
                "5",
                literal.token_literal()
            );
        }
    } else {
        panic!("The Expression is None");
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct PrefixTest {
        input: String,
        operator: String,
        value: String,
    }

    impl PrefixTest {
        fn new(input: String, operator: String, value: String) -> Self {
            PrefixTest {
                input,
                operator,
                value,
            }
        }
    }

    let prefix_tests = vec![
        PrefixTest::new("!5".into(), "!".into(), 5.to_string()),
        PrefixTest::new("-15".into(), "-".into(), 15.to_string()),
        PrefixTest::new("!true;".into(), "!".into(), true.to_string()),
        PrefixTest::new("!false;".into(), "!".into(), false.to_string()),
    ];

    for tt in prefix_tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if program.is_none() {
            panic!("parse_program() returned None");
        }

        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements, got={}",
                1,
                program.statements.len()
            );
        }

        let exp_stmt = program.statements[0]
            .into_expression()
            .unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

        if let Some(exp) = &exp_stmt.expression {
            let prefix = exp.into_prefix().unwrap_or_else(|x| panic!("{}", x));
            if !prefix.operator.eq(&tt.operator) {
                panic!(
                    "prefix.operator is not '{}' got='{}'",
                    tt.operator, prefix.operator
                );
            }

            if let Some(expr) = &prefix.right {
                test_literal_expression(&expr, tt.value);
            } else {
                panic!("right side of expression is none");
            }
        } else {
            panic!("The Expression is None");
        }
    }
}

#[test]
fn test_parsing_infix_expression() {
    struct InfixTest {
        input: String,
        left: String,
        operator: String,
        right: String,
    }

    impl InfixTest {
        fn new(input: String, left: String, operator: String, right: String) -> Self {
            InfixTest {
                input,
                left,
                operator,
                right,
            }
        }
    }

    let infix_tests = vec![
        InfixTest::new("5 + 5;".into(), 5.to_string(), "+".into(), 5.to_string()),
        InfixTest::new("5 - 5;".into(), 5.to_string(), "-".into(), 5.to_string()),
        InfixTest::new("5 * 5;".into(), 5.to_string(), "*".into(), 5.to_string()),
        InfixTest::new("5 / 5;".into(), 5.to_string(), "/".into(), 5.to_string()),
        InfixTest::new("5 > 5;".into(), 5.to_string(), ">".into(), 5.to_string()),
        InfixTest::new("5 < 5;".into(), 5.to_string(), "<".into(), 5.to_string()),
        InfixTest::new("5 == 5;".into(), 5.to_string(), "==".into(), 5.to_string()),
        InfixTest::new("5 != 5;".into(), 5.to_string(), "!=".into(), 5.to_string()),
        InfixTest::new(
            "true == true".into(),
            true.to_string(),
            "==".into(),
            true.to_string(),
        ),
        InfixTest::new(
            "false == false".into(),
            false.to_string(),
            "==".into(),
            false.to_string(),
        ),
        InfixTest::new(
            "true != false".into(),
            true.to_string(),
            "!=".into(),
            false.to_string(),
        ),
    ];

    for tt in infix_tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if program.is_none() {
            panic!("parse_program() returned None");
        }

        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements, got={}",
                1,
                program.statements.len()
            );
        }

        let exp_stmt = program.statements[0]
            .into_expression()
            .unwrap_or_else(|x| panic!("{}", x));

        if let Some(exp) = &exp_stmt.expression {
            test_infix_expression(exp, tt.left, tt.operator, tt.right);
        } else {
            panic!("There were no expressions in the statement");
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct Test {
        input: String,
        expected: String,
    }

    impl Test {
        fn new(input: String, expected: String) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("-a * b".into(), "((-a) * b)".into()),
        Test::new("!-a".into(), "(!(-a))".into()),
        Test::new("a + b + c".into(), "((a + b) + c)".into()),
        Test::new("a + b - c".into(), "((a + b) - c)".into()),
        Test::new("a * b * c".into(), "((a * b) * c)".into()),
        Test::new("a * b / c".into(), "((a * b) / c)".into()),
        Test::new("a + b / c".into(), "(a + (b / c))".into()),
        Test::new(
            "a + b * c + d / e - f".into(),
            "(((a + (b * c)) + (d / e)) - f)".into(),
        ),
        Test::new("3 + 4; -5 * 5".into(), "(3 + 4)((-5) * 5)".into()),
        Test::new("5 > 4 == 3 < 4".into(), "((5 > 4) == (3 < 4))".into()),
        Test::new("5 < 4 != 3 > 4".into(), "((5 < 4) != (3 > 4))".into()),
        Test::new(
            "3 + 4 * 5 == 3 * 1 + 4 * 5".into(),
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".into(),
        ),
        Test::new("true".into(), "true".into()),
        Test::new("false".into(), "false".into()),
        Test::new("3 > 5 == false".into(), "((3 > 5) == false)".into()),
        Test::new("3 < 5 == true".into(), "((3 < 5) == true)".into()),
        Test::new("1 + (2 + 3) + 4".into(), "((1 + (2 + 3)) + 4)".into()),
        Test::new("(5 + 5) * 2".into(), "((5 + 5) * 2)".into()),
        Test::new("2 / (5 + 5)".into(), "(2 / (5 + 5))".into()),
        Test::new("-(5 + 5)".into(), "(-(5 + 5))".into()),
        Test::new("!(true == true)".into(), "(!(true == true))".into()),
        Test::new(
            "a + add(b * c) + d".into(),
            "((a + add((b * c))) + d)".into(),
        ),
        Test::new(
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".into(),
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".into(),
        ),
        Test::new(
            "add(a + b + c * d / f + g)".into(),
            "add((((a + b) + ((c * d) / f)) + g))".into(),
        ),
    ];

    for test in tests {
        let l = Lexer::new(test.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let actual = program.unwrap().to_string();
        if !actual.eq(&test.expected) {
            panic!("expected={}, got={}", test.expected, actual);
        }
    }
}

#[test]
fn test_if_expression() {
    let input: String = "if (x < y) { x }".into();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmts = program
        .unwrap_or_else(|| panic!("No program is parsed"))
        .statements;

    if stmts.len() != 1 {
        panic!(
            "program.statments does not contain {} statements, got={}",
            1,
            stmts.len()
        );
    }

    let stmt = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    let exp = stmt
        .expression
        .as_ref()
        .unwrap_or_else(|| panic!("There is no expression in the statement"))
        .into_if()
        .unwrap_or_else(|e| panic!("{}", e));

    test_infix_expression(
        exp.condition
            .as_ref()
            .unwrap_or_else(|| panic!("There is no condition")),
        "x".into(),
        "<".into(),
        "y".into(),
    );

    let stmts = &exp
        .consequence
        .as_ref()
        .unwrap_or_else(|| panic!("There is no consequence"))
        .statements;

    if stmts.len() != 1 {
        panic!("consequence is not 1 statements, got={}", stmts.len());
    }

    let consequence = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    test_identifier(
        consequence
            .expression
            .as_ref()
            .unwrap_or_else(|| panic!("Consequence has no expression")),
        "x".into(),
    );

    if exp.alternative.is_some() {
        panic!("exp.alternative.statements was not none");
    }
}

#[test]
fn test_if_else_expression() {
    let input: String = "if (x < y) { x } else { y }".into();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmts = program
        .unwrap_or_else(|| panic!("No program is parsed"))
        .statements;

    if stmts.len() != 1 {
        panic!(
            "program.statments does not contain {} statements, got={}",
            1,
            stmts.len()
        );
    }

    let stmt = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    let exp = stmt
        .expression
        .as_ref()
        .unwrap_or_else(|| panic!("There is no expression in the statement"))
        .into_if()
        .unwrap_or_else(|e| panic!("{}", e));

    test_infix_expression(
        exp.condition
            .as_ref()
            .unwrap_or_else(|| panic!("There is no condition")),
        "x".into(),
        "<".into(),
        "y".into(),
    );

    let stmts = &exp
        .consequence
        .as_ref()
        .unwrap_or_else(|| panic!("There is no consequence"))
        .statements;

    if stmts.len() != 1 {
        panic!("consequence is not 1 statements, got={}", stmts.len());
    }

    let consequence = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    test_identifier(
        consequence
            .expression
            .as_ref()
            .unwrap_or_else(|| panic!("Consequence has no expression")),
        "x".into(),
    );

    let stmts = &exp
        .alternative
        .as_ref()
        .unwrap_or_else(|| panic!("There is no alternative"))
        .statements;

    if stmts.len() != 1 {
        panic!("alternative is not 1 statements, got={}", stmts.len());
    }

    let alternative = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    test_identifier(
        alternative
            .expression
            .as_ref()
            .unwrap_or_else(|| panic!("Alternative has no expression")),
        "y".into(),
    );
}

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y; }".to_string();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmts = program.expect("Failed to parse the program").statements;

    if stmts.len() != 1 {
        panic!(
            "program.statements does not contain {} statements, got={}",
            1,
            stmts.len()
        );
    }

    let stmt = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    let function = stmt
        .expression
        .as_ref()
        .expect("There is no expression")
        .into_func()
        .unwrap_or_else(|e| panic!("{}", e));

    if function.parameters.len() != 2 {
        panic!(
            "function literal parameters wrong, expected 2, got={}",
            function.parameters.len()
        );
    }

    test_literal_expression(&function.parameters[0], "x".into());
    test_literal_expression(&function.parameters[1], "y".into());

    let stmts = &function
        .body
        .as_ref()
        .expect("There is no body for the function")
        .statements;

    if stmts.len() != 1 {
        panic!(
            "function.body.statements doesnt have 1 statements, got={}",
            stmts.len()
        );
    }

    let body_stmt = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    test_infix_expression(
        body_stmt
            .expression
            .as_ref()
            .expect("There is not expression"),
        "x".into(),
        "+".into(),
        "y".into(),
    );
}

#[test]
fn test_function_parameter_parsing() {
    struct Test {
        input: String,
        expected_parameters: Vec<String>,
    }

    impl Test {
        fn new(input: String, expected_parameters: Vec<String>) -> Self {
            Test {
                input,
                expected_parameters,
            }
        }
    }

    let tests = vec![
        Test::new("fn() {}".into(), vec![]),
        Test::new("fn(x) {}".into(), vec!["x".into()]),
        Test::new(
            "fn(x, y, z) {}".into(),
            vec!["x".into(), "y".into(), "z".into()],
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = program
            .as_ref()
            .expect("There was no program to parse")
            .statements[0]
            .into_expression()
            .unwrap_or_else(|e| panic!("{}", e));

        let function = stmt
            .expression
            .as_ref()
            .expect("There is no expression parsed")
            .into_func()
            .unwrap_or_else(|e| panic!("{}", e));

        if function.parameters.len() != tt.expected_parameters.len() {
            panic!(
                "length of parameters are wrong, expected {}, got={}",
                tt.expected_parameters.len(),
                function.parameters.len()
            );
        }

        for (i, ident) in tt.expected_parameters.iter().enumerate() {
            test_literal_expression(&function.parameters[i], ident.to_string());
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);".to_string();

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    check_parser_errors(&p);

    let stmts = prog.expect("There is no program parsed").statements;

    if stmts.len() != 1 {
        panic!(
            "program.statements does not contain {} statements, got={}",
            1,
            stmts.len()
        );
    }

    let stmt = stmts[0]
        .into_expression()
        .unwrap_or_else(|e| panic!("{}", e));

    let exp = stmt
        .expression
        .as_ref()
        .expect("There is no expression in the statement")
        .into_call()
        .unwrap_or_else(|e| panic!("{}", e));

    test_identifier(
        exp.function.as_ref().expect("There is not function"),
        "add".to_string(),
    );

    if exp.arguments.len() != 3 {
        panic!("wrong length of arguments, got={}", exp.arguments.len());
    }

    test_literal_expression(&exp.arguments[0], "1".into());
    test_infix_expression(&exp.arguments[1], "2".into(), "*".into(), "3".into());
    test_infix_expression(&exp.arguments[2], "4".into(), "+".into(), "5".into());
}

#[test]
fn test_call_expression_parameter_parsing() {
    struct Test {
        input: String,
        expected_ident: String,
        expected_args: Vec<String>,
    }

    impl Test {
        fn new(input: String, expected_ident: String, expected_args: Vec<String>) -> Self {
            Test {
                input,
                expected_ident,
                expected_args,
            }
        }
    }

    let tests = vec![
        Test::new("add()".into(), "add".into(), vec![]),
        Test::new("add(1)".into(), "add".into(), vec!["1".into()]),
        Test::new(
            "add(1, 2 * 3, 4 + 5);".into(),
            "add".into(),
            vec!["1".into(), "(2 * 3)".into(), "(4 + 5)".into()],
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        let stmt = prog
            .as_ref()
            .expect("There is no program parsed")
            .statements[0]
            .into_expression()
            .unwrap_or_else(|e| panic!("{}", e));
        let exp = stmt
            .expression
            .as_ref()
            .expect("There is no expression in the statement")
            .into_call()
            .unwrap_or_else(|e| panic!("{}", e));

        test_identifier(
            exp.function.as_ref().expect("There is no function"),
            tt.expected_ident,
        );

        if exp.arguments.len() != tt.expected_args.len() {
            panic!(
                "wrong number of arguments. expected={}, got={}",
                tt.expected_args.len(),
                exp.arguments.len()
            );
        }

        for (i, arg) in tt.expected_args.iter().enumerate() {
            if !exp.arguments[i].to_string().eq(arg) {
                panic!(
                    "argument {} wrong, expected={}, got={}",
                    i,
                    arg,
                    exp.arguments[i].to_string()
                );
            }
        }
    }
}

#[test]
fn test_let_statements() {
    struct Test {
        input: String,
        expected_identifier: String,
        expected_value: String,
    }

    impl Test {
        fn new(input: String, expected_identifier: String, expected_value: String) -> Self {
            Test {
                input,
                expected_value,
                expected_identifier,
            }
        }
    }

    let tests = vec![
        Test::new("let x = 5;".into(), "x".into(), "5".into()),
        Test::new("let y = true;".into(), "y".into(), "true".into()),
        Test::new("let foobar = y;".into(), "foobar".into(), "y".into()),
    ];

    for tt in tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        let stmts = prog.expect("There is no program parsed").statements;

        if stmts.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement, got={}",
                stmts.len()
            );
        }

        let stmt = &stmts[0];
        test_let_statement(stmt, tt.expected_identifier);

        let val = stmt.into_let().unwrap_or_else(|e| panic!("{}", e));
        test_literal_expression(
            val.value.as_ref().expect("There is no value in let"),
            tt.expected_value,
        )
    }
}

#[test]
fn test_return_statements() {
    struct Test {
        input: String,
        expected_value: String,
    }

    impl Test {
        fn new(input: String, expected_value: String) -> Self {
            Test {
                input,
                expected_value,
            }
        }
    }

    let tests = vec![
        Test::new("return 5;".into(), "5".into()),
        Test::new("return true;".into(), "true".into()),
        Test::new("return foobar;".into(), "foobar".into()),
    ];

    for tt in tests {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        let stmts = prog.expect("There is no program to be parsed").statements;
        if stmts.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement, got={}",
                stmts.len()
            );
        }

        let stmt = &stmts[0];
        let return_stmt = stmt.into_return().unwrap_or_else(|e| panic!("{}", e));

        if !return_stmt.token_literal().eq("return") {
            panic!(
                "return_stmt.token_literal not 'return', got={}",
                return_stmt.token_literal()
            );
        }

        test_literal_expression(
            return_stmt
                .return_value
                .as_ref()
                .expect("There is no return value"),
            tt.expected_value,
        );
    }
}
