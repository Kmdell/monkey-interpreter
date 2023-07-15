pub mod token;
pub mod lexer;
pub mod repl;
mod ast;
mod parser;


#[cfg(test)]
mod tests {
    use crate::token::RETURN;

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
}
