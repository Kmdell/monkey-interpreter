use std::collections::HashMap;

pub type TokenType = String;

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// Identifiers and literals
pub const IDENT: &str = "IDENT"; // add, x, y...
pub const INT: &str = "INT"; // 1343234
pub const STRING: &str = "STRING";

// Operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERICK: &str = "*";
pub const SLASH: &str = "/";

// Comparisons
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";

// Delimiters
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";

// Keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "false";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn lookup_ident(ident: &str) -> TokenType {
    let keywords = HashMap::from([
        ("fn", FUNCTION.to_string()),
        ("let", LET.to_string()),
        ("true", TRUE.to_string()),
        ("false", FALSE.to_string()),
        ("if", IF.to_string()),
        ("else", ELSE.to_string()),
        ("return", RETURN.to_string()),
    ]);

    if let Some(tok) = keywords.get(ident) {
        return tok.to_string();
    }
    return IDENT.to_string();
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }
}
