use std::rc::Rc;

pub type Str = Rc<str>;

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(Str),
    INT(Str),
    STRING(Str),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERICK,
    SLASH,

    LT,
    GT,
    EQ,
    NOTEQ,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}
