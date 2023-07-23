mod token;
mod lexer;
pub mod repl;
mod ast;
mod parser;


#[cfg(test)]
mod tests {
    use crate::ast::Expression;

    use super::*;
    use ast::Node;
    
    #[test]
    fn test_next_token() {
        struct Test {
            expected_type: token::TokenType,
            expected_literal: String,
        }

        impl Test {
            fn new(token_type: token::TokenType, literal: String) -> Self {
                Test {
                    expected_type: token_type,
                    expected_literal: literal,
                }
            }
        }

        let tests = vec![
            Test::new(token::LET.to_string(), "let".to_string()),
            Test::new(token::IDENT.to_string(), "five".to_string()),
            Test::new(token::ASSIGN.to_string(), "=".to_string()),
            Test::new(token::INT.to_string(), "5".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::LET.to_string(), "let".to_string()),
            Test::new(token::IDENT.to_string(), "ten".to_string()),
            Test::new(token::ASSIGN.to_string(), "=".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::LET.to_string(), "let".to_string()),
            Test::new(token::IDENT.to_string(), "add".to_string()),
            Test::new(token::ASSIGN.to_string(), "=".to_string()),
            Test::new(token::FUNCTION.to_string(), "fn".to_string()),
            Test::new(token::LPAREN.to_string(), "(".to_string()),
            Test::new(token::IDENT.to_string(), "x".to_string()),
            Test::new(token::COMMA.to_string(), ",".to_string()),
            Test::new(token::IDENT.to_string(), "y".to_string()),
            Test::new(token::RPAREN.to_string(), ")".to_string()),
            Test::new(token::LBRACE.to_string(), "{".to_string()),
            Test::new(token::IDENT.to_string(), "x".to_string()),
            Test::new(token::PLUS.to_string(), "+".to_string()),
            Test::new(token::IDENT.to_string(), "y".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::RBRACE.to_string(), "}".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::LET.to_string(), "let".to_string()),
            Test::new(token::IDENT.to_string(), "result".to_string()),
            Test::new(token::ASSIGN.to_string(), "=".to_string()),
            Test::new(token::IDENT.to_string(), "add".to_string()),
            Test::new(token::LPAREN.to_string(), "(".to_string()),
            Test::new(token::IDENT.to_string(), "five".to_string()),
            Test::new(token::COMMA.to_string(), ",".to_string()),
            Test::new(token::IDENT.to_string(), "ten".to_string()),
            Test::new(token::RPAREN.to_string(), ")".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::BANG.to_string(), "!".to_string()),
            Test::new(token::MINUS.to_string(), "-".to_string()),
            Test::new(token::SLASH.to_string(), "/".to_string()),
            Test::new(token::ASTERICK.to_string(), "*".to_string()),
            Test::new(token::INT.to_string(), "5".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::INT.to_string(), "5".to_string()),
            Test::new(token::LT.to_string(), "<".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::GT.to_string(), ">".to_string()),
            Test::new(token::INT.to_string(), "5".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::IF.to_string(), "if".to_string()),
            Test::new(token::LPAREN.to_string(), "(".to_string()),
            Test::new(token::INT.to_string(), "5".to_string()),
            Test::new(token::LT.to_string(), "<".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::RPAREN.to_string(), ")".to_string()),
            Test::new(token::LBRACE.to_string(), "{".to_string()),
            Test::new(token::RETURN.to_string(), "return".to_string()),
            Test::new(token::TRUE.to_string(), "true".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::RBRACE.to_string(), "}".to_string()),
            Test::new(token::ELSE.to_string(), "else".to_string()),
            Test::new(token::LBRACE.to_string(), "{".to_string()),
            Test::new(token::RETURN.to_string(), "return".to_string()),
            Test::new(token::FALSE.to_string(), "false".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::RBRACE.to_string(), "}".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::EQ.to_string(), "==".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
            Test::new(token::INT.to_string(), "10".to_string()),
            Test::new(token::NOT_EQ.to_string(), "!=".to_string()),
            Test::new(token::INT.to_string(), "9".to_string()),
            Test::new(token::SEMICOLON.to_string(), ";".to_string()),
        ];

        let input: String = "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
".to_string();

        let mut l = lexer::Lexer::new(input);

        for (i, tt) in tests.iter().enumerate() {
            let tok = l.next_token();
            
            assert!(tok.token_type.eq(&tt.expected_type), "tests[{}] - token_type wrong, expected=\"{}\", got=\"{}\"", i, tt.expected_type, tok.token_type);

            println!("{} | {}", tok.literal, tt.expected_literal);
            assert!(tok.literal.eq(&tt.expected_literal), "tests[{}] - literal wrong, expectec=\"{}\", got=\"{}\"", i, tt.expected_literal, tok.literal);
        }
    }

    #[test]
    pub fn test_let_statement_parsing() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
".to_string();

        let lexer: lexer::Lexer = lexer::Lexer::new(input);
        let mut parser: parser::Parser = parser::Parser::new(lexer); 

        let program: Option<ast::Program> = parser.parse_program();
        check_parser_errors(&parser);
        if program.is_none() {
            panic!("parsed_program() returned a None");
        }

        let program = program.unwrap();
        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements, instead got {}", program.statements.len());
        }
        
        let tests: Vec<Test> = ["x", "y", "foobar"].iter().map(|x| Test::new(x)).collect();

        tests.into_iter().enumerate().for_each(|(i, test)| {
            let stmt = program.statements.get(i).unwrap();
            test_let_statements(stmt, test);
        });
        
    }

    struct Test {
        expected_identifier: String,
    }

    impl Test {
        fn new(expected_identifier: &str) -> Self {
            Test {
                expected_identifier: expected_identifier.to_string(),
            }
        }
    }

    fn test_let_statements(stmt: &Box<dyn ast::Statement>, test: Test) {
        if !stmt.token_literal().eq("let") {
            panic!("s.token_literal is not 'let'. got={}", stmt.token_literal()); 
        }

        let let_stmt = stmt.into_let().unwrap_or_else(|x| panic!("s is not a LetStatement, got={}", x));

        if !let_stmt.name.value.eq(&test.expected_identifier) {
            panic!("let_stmt.value.name not '{}', got='{}'", test.expected_identifier, let_stmt.name.value);
        }

        if !let_stmt.name.token_literal().eq(&test.expected_identifier) {
            panic!("let_stmt.name.token_literal() not '{}', got='{}'", test.expected_identifier, let_stmt.name.token_literal());
        }
    }

    fn check_parser_errors(program: &parser::Parser) {
        let errors = program.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("Parser had {} errors", errors.len());
        errors.iter().for_each(|error| eprintln!("{}", error));
        panic!("Parser ran into errors");
    }

    #[test]
    fn test_return_statment_parsing() {
        let input = "
return 5;
return 10;
return 993322;
".to_string();
    
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        
        let program = p.parse_program();
        check_parser_errors(&p);

        if program.is_none() {
            panic!("parsed_program() returned a None");
        }

        let program = program.unwrap();
        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements, instead got {}", program.statements.len());
        }

        program.statements.into_iter().for_each(|stmt| {
            let return_stmt = stmt.into_return().unwrap_or_else(|x| panic!("s is not a ReturnStatment, got={}", x));

            if !return_stmt.token_literal().eq("return") {
                panic!("stmt.token_literal is not 'return', got='{}'", return_stmt.token_literal())
            }
        });
    }

    #[test]
    fn test_string() {
        let program = ast::Program {
            statements: vec![
                Box::new(ast::LetStatement {
                    token: token::Token { token_type: token::LET.to_string(), literal: "let".to_string() },
                    name: ast::Identifier {
                        token: token::Token { token_type: token::IDENT.to_string(), literal: "myVar".to_string() },
                        value: "myVar".to_string(),
                    },
                    value: Some(Box::new(ast::Identifier {
                        token: token::Token { token_type: token::IDENT.to_string(), literal: "anotherVar".to_string() },
                        value: "anotherVar".to_string(),
                    }))
                }),
            ]
        };

        if !program.to_string().eq("let myVar = anotherVar;") {
            panic!("program.to_string() wrong. got='{}'", program.to_string())
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_string();
        let l = lexer::Lexer::new(input);

        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if program.is_none() {
            panic!("parsed_program() returned a None");
        }

        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!("program.statements does not contain 1 statements, instead got {}", program.statements.len());
        }

        let exp_stmt = program.statements[0].into_expression().unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

        if let Some(exp) = &exp_stmt.expression {
            let ident = exp.into_identifer().unwrap_or_else(|_| panic!("Expression is not an Identifier"));
            if !ident.value.eq("foobar") {
                panic!("ident.value is not '{}' got='{}'", "foobar", ident.value);
            }
            
            if !ident.token_literal().eq("foobar") {
                panic!("ident.token_literal() is not '{}', got='{}'", "foobar", ident.token_literal());
            }
        } else {
            panic!("The Expression is None");
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();

        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!("program.statements does not contain 1 statements, instead got {}", program.statements.len());
        }

        let exp_stmt = program.statements[0].into_expression().unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

        if let Some(exp) = &exp_stmt.expression {
            let literal = exp.into_integer_literal().unwrap_or_else(|x| panic!("{}", x));
            if literal.value != 5 {
                panic!("literal.value is not '{}' got='{}'", 5, literal.value);
            }
            
            if !literal.token_literal().eq("5") {
                panic!("ident.token_literal() is not '{}', got='{}'", "5", literal.token_literal());
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
            integer_value: i64, 
        }

        impl PrefixTest {
            fn new(input: String, operator: String, integer_value: i64) -> Self {
                PrefixTest { input, operator, integer_value }
            }
        }

        let prefix_tests = vec![
            PrefixTest::new("!5".into(), "!".into(), 5),
            PrefixTest::new("-15".into(), "-".into(), 15)
        ];

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.input);
            let mut p = parser::Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.is_none() {
                panic!("parse_program() returned None");
            }

            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!("program.statements does not contain {} statements, got={}", 1, program.statements.len());
            }

            let exp_stmt = program.statements[0].into_expression().unwrap_or_else(|x| panic!("stmt is not an ExpressionStatement, got='{}'", x));

            if let Some(exp) = &exp_stmt.expression {
                let prefix = exp.into_prefix().unwrap_or_else(|x| panic!("{}", x));
                if !prefix.operator.eq(&tt.operator) {
                    panic!("prefix.operator is not '{}' got='{}'", tt.operator, prefix.operator);
                }
                
                if let Some(expr) = &prefix.right {
                    test_integer_literal(&expr, tt.integer_value);
                } else {
                    panic!("right side of expression is none");
                }
            } else {
                panic!("The Expression is None");
            }
        }
    }

    fn test_integer_literal(il: &Box<dyn Expression>, value: i64) {
        let integ = il.into_integer_literal().unwrap_or_else(|x| panic!("{}", x));
        if integ.value != value {
            panic!("integ.value not {}, got={}", value, integ.value);
        }

        if !integ.token_literal().eq(&format!("{}", value)) {
            panic!("integ.token_literal() not {}, got={}", value, integ.token_literal());
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        struct InfixTest {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        }

        impl InfixTest {
            fn new(input: String, left_value: i64, operator: String, right_value: i64) -> Self {
                InfixTest {
                    input,
                    left_value,
                    operator,
                    right_value
                }
            }
        }

        let infix_tests = vec![
            InfixTest::new("5 + 5;".into(), 5, "+".into(), 5),
            InfixTest::new("5 - 5;".into(), 5, "-".into(), 5),
            InfixTest::new("5 * 5;".into(), 5, "*".into(), 5),
            InfixTest::new("5 / 5;".into(), 5, "/".into(), 5),
            InfixTest::new("5 > 5;".into(), 5, ">".into(), 5),
            InfixTest::new("5 < 5;".into(), 5, "<".into(), 5),
            InfixTest::new("5 == 5;".into(), 5, "==".into(), 5),
            InfixTest::new("5 != 5;".into(), 5, "!=".into(), 5)
        ];

        for tt in infix_tests {
            let l = lexer::Lexer::new(tt.input);
            let mut p = parser::Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            
            if program.is_none() {
                panic!("parse_program() returned None");
            }

            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!("program.statements does not contain {} statements, got={}", 1, program.statements.len());
            }

            let exp_stmt = program.statements[0].into_expression().unwrap_or_else(|x| panic!("{}", x));

            if let Some(expr) = &exp_stmt.expression {
                let infix = expr.into_infix().unwrap_or_else(|x| panic!("{}", x));
                
                if let Some(left) = &infix.left {
                    test_integer_literal(&left, tt.left_value);
                } else {
                    panic!("The left side of the infix is empty");
                }

                if !infix.operator.eq(&tt.operator) {
                    panic!("infix.operator is not '{}', got={}", tt.operator, infix.operator);
                }

                if let Some(right) = &infix.right {
                    test_integer_literal(&right, tt.right_value);
                } else {
                    panic!("The right side of the infix is empty");
                }
            } else {
                panic!("No Expression in the ExpressionStatment");
            }
        }
    }

    #[test] 
    fn test_operator_precendence_parsing() {
        struct Test {
            input: String,
            expected: String,
        }

        impl Test {
            fn new(input: String, expected: String) -> Self {
                Test {
                    input,
                    expected,       
                }
            }
        }

        let tests = vec![
            Test::new(
                "-a * b".into(),
                "((-a) * b)".into()
            ),
            Test::new(
                "!-a".into(),
                "(!(-a))".into(),
            ),
            Test::new(
                "a + b + c".into(),
                "((a + b) + c)".into()
            ),
            Test::new(
                "a + b - c".into(),
                "((a + b) - c)".into()
            ),
            Test::new(
                "a * b * c".into(),
                "((a * b) * c)".into()
            ),
            Test::new(
                "a * b / c".into(),
                "((a * b) / c)".into()
            ),
            Test::new(
                "a + b / c".into(),
                "(a + (b / c))".into(),
            ),
            Test::new(
                "a + b * c + d / e - f".into(),
                "(((a + (b * c)) + (d / e)) - f)".into()
            ),
            Test::new(
                "3 + 4; -5 * 5".into(),
                "(3 + 4)((-5) * 5)".into()
            ),
            Test::new(
                "5 > 4 == 3 < 4".into(),
                "((5 > 4) == (3 < 4))".into()
            ),
            Test::new(
                "5 < 4 != 3 > 4".into(),
                "((5 < 4) != (3 > 4))".into()
            ),
            Test::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5".into(),
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".into()
            )
        ];

        for test in tests {
            let l = lexer::Lexer::new(test.input);
            let mut p = parser::Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.unwrap().to_string();
            if !actual.eq(&test.expected) {
                panic!("expected={}, got={}", test.expected, actual);
            }
        }
    }
}
