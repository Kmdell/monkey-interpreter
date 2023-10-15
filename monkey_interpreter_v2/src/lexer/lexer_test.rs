use crate::{lexer::*, *};

#[test]
fn test_next_token() {
    let tests = vec![
        Token::LET,
        Token::IDENT("five".to_string()),
        Token::ASSIGN,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".to_string()),
        Token::ASSIGN,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("add".to_string()),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT("x".to_string()),
        Token::COMMA,
        Token::IDENT("y".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT("x".to_string()),
        Token::PLUS,
        Token::IDENT("y".to_string()),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("result".to_string()),
        Token::ASSIGN,
        Token::IDENT("add".to_string()),
        Token::LPAREN,
        Token::IDENT("five".to_string()),
        Token::COMMA,
        Token::IDENT("ten".to_string()),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERICK,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::GT,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::RETURN,
        Token::TRUE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::ELSE,
        Token::LBRACE,
        Token::RETURN,
        Token::FALSE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::INT("10".to_string()),
        Token::EQ,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::INT("10".to_string()),
        Token::NOTEQ,
        Token::INT("9".to_string()),
        Token::SEMICOLON,
        Token::EOF,
    ];

    let input = "let five = 5;
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
";

    let mut l = lexer::Lexer::new(input);

    for (i, tt) in tests.iter().enumerate() {
        let tok = l.next_token();

        if tok != *tt {
            panic!(
                "test[{}] - token_type wrong. expected={:?}, got={:?}",
                i, tt, tok
            );
        }

        match (tt, tok) {
            (Token::INT(test_int), Token::INT(token_int)) => {
                if !token_int.eq(test_int) {
                    panic!(
                        "test[{}] - token_int wrong. expected={}, got={}",
                        i, test_int, token_int
                    )
                }
            }
            (Token::IDENT(test_ident), Token::IDENT(token_ident)) => {
                if !token_ident.eq(test_ident) {
                    panic!(
                        "test[{}] - token_ident wrong. expected={}, got={}",
                        i, test_ident, token_ident
                    )
                }
            }
            _ => {}
        }
    }
}
