use crate::{lexer::*, *};

#[test]
fn test_next_token() {
    let tests = vec![
        Token::LET,
        Token::IDENT("five".into()),
        Token::ASSIGN,
        Token::INT("5".into()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".into()),
        Token::ASSIGN,
        Token::INT("10".into()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("add".into()),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT("x".into()),
        Token::COMMA,
        Token::IDENT("y".into()),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT("x".into()),
        Token::PLUS,
        Token::IDENT("y".into()),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("result".into()),
        Token::ASSIGN,
        Token::IDENT("add".into()),
        Token::LPAREN,
        Token::IDENT("five".into()),
        Token::COMMA,
        Token::IDENT("ten".into()),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERICK,
        Token::INT("5".into()),
        Token::SEMICOLON,
        Token::INT("5".into()),
        Token::LT,
        Token::INT("10".into()),
        Token::GT,
        Token::INT("5".into()),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT("5".into()),
        Token::LT,
        Token::INT("10".into()),
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
        Token::INT("10".into()),
        Token::EQ,
        Token::INT("10".into()),
        Token::SEMICOLON,
        Token::INT("10".into()),
        Token::NOTEQ,
        Token::INT("9".into()),
        Token::SEMICOLON,
        Token::STRING("foobar".into()),
        Token::STRING("foo bar".into()),
        Token::LBRACE,
        Token::INT("1".into()),
        Token::COMMA,
        Token::INT("2".into()),
        Token::RBRACE,
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
\"foobar\"
\"foo bar\"
[1, 2];
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
