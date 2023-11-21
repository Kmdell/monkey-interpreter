use std::collections::HashMap;

use crate::token::*;

#[cfg(test)]
mod lexer_test;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    read_pos: usize,
    ch: char,
    keywords: HashMap<String, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            pos: 0,
            read_pos: 0,
            ch: char::default(),
            keywords: HashMap::from([
                ("fn".into(), Token::FUNCTION),
                ("let".into(), Token::LET),
                ("true".into(), Token::TRUE),
                ("false".into(), Token::FALSE),
                ("if".into(), Token::IF),
                ("else".into(), Token::ELSE),
                ("return".into(), Token::RETURN),
            ]),
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_pos).unwrap();
        }

        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_pos >= self.input.len() {
            return '\0';
        }
        self.input.chars().nth(self.read_pos).unwrap()
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char().eq(&'=') {
                    self.read_char();
                    tok = Token::EQ;
                } else {
                    tok = Token::ASSIGN;
                }
            }
            '+' => {
                tok = Token::PLUS;
            }
            '-' => {
                tok = Token::MINUS;
            }
            '!' => {
                if self.peek_char().eq(&'=') {
                    self.read_char();
                    tok = Token::NOTEQ;
                } else {
                    tok = Token::BANG;
                }
            }
            '/' => {
                tok = Token::SLASH;
            }
            '*' => {
                tok = Token::ASTERICK;
            }
            '<' => {
                tok = Token::LT;
            }
            '>' => {
                tok = Token::GT;
            }
            ';' => {
                tok = Token::SEMICOLON;
            }
            ',' => {
                tok = Token::COMMA;
            }
            '(' => {
                tok = Token::LPAREN;
            }
            ')' => {
                tok = Token::RPAREN;
            }
            '{' => {
                tok = Token::LBRACE;
            }
            '}' => {
                tok = Token::RBRACE;
            }
            '\0' => {
                tok = Token::EOF;
            }
            '"' => {
                tok = Token::STRING(self.read_string());
            }
            '[' => {
                tok = Token::LBRACKET;
            }
            ']' => {
                tok = Token::RBRACKET;
            }
            ':' => {
                tok = Token::COLON;
            }
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return self.lookup_ident(ident);
                } else if is_digit(self.ch) {
                    return Token::INT(self.read_number());
                } else {
                    tok = Token::ILLEGAL;
                }
            }
        }
        self.read_char();

        tok
    }

    fn read_identifier(&mut self) -> Str {
        let pos = self.pos;
        while is_letter(self.ch) {
            self.read_char();
        }

        self.input[pos..self.pos].into()
    }

    fn read_number(&mut self) -> Str {
        let pos = self.pos;
        while is_digit(self.ch) {
            self.read_char();
        }

        self.input[pos..self.pos].into()
    }

    fn read_string(&mut self) -> Str {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }

        self.input[pos..self.pos].into()
    }

    fn lookup_ident(&self, ident: Str) -> Token {
        if let Some(tok) = self.keywords.get(&(*ident)) {
            return tok.clone();
        }
        Token::IDENT(ident)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.eq(&' ') || self.ch.eq(&'\t') || self.ch.eq(&'\r') || self.ch.eq(&'\n') {
            self.read_char()
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch.eq(&'_')
}

fn is_digit(ch: char) -> bool {
    ch.is_numeric()
}
