mod token;
mod lexer;
pub mod repl;
mod ast;
mod parser;
mod parser_test;


#[cfg(test)]
mod tests {
    use super::*;
    
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
}
