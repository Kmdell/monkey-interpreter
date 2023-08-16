use super::*;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub input: Vec<char>,
    pub pos: usize,      // current position in input (points to current char)
    pub read_pos: usize, // current reading position in input (after current char)
    pub ch: char,        // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lex = Lexer {
            input: input.chars().collect(),
            pos: 0,
            read_pos: 0,
            ch: '\n',
        };
        lex.read_char();
        lex
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_pos];
        }

        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_pos]
        }
    }

    pub fn next_token(&mut self) -> token::Token {
        let tok: token::Token;

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let ch: char = self.ch;
                    self.read_char();
                    let literal = [ch, self.ch].iter().collect();
                    tok = token::Token::new(token::EQ.to_string(), literal);
                } else {
                    tok = token::Token::new(token::ASSIGN.to_string(), self.ch.to_string());
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = [ch, self.ch].iter().collect();
                    tok = token::Token::new(token::NOT_EQ.to_string(), literal);
                } else {
                    tok = token::Token::new(token::BANG.to_string(), self.ch.to_string());
                }
            }
            ';' => {
                tok = token::Token::new(token::SEMICOLON.to_string(), self.ch.to_string());
            }
            '+' => {
                tok = token::Token::new(token::PLUS.to_string(), self.ch.to_string());
            }
            '-' => tok = token::Token::new(token::MINUS.to_string(), self.ch.to_string()),
            '/' => {
                tok = token::Token::new(token::SLASH.to_string(), self.ch.to_string());
            }
            '*' => {
                tok = token::Token::new(token::ASTERICK.to_string(), self.ch.to_string());
            }
            '<' => {
                tok = token::Token::new(token::LT.to_string(), self.ch.to_string());
            }
            '>' => {
                tok = token::Token::new(token::GT.to_string(), self.ch.to_string());
            }
            '(' => {
                tok = token::Token::new(token::LPAREN.to_string(), self.ch.to_string());
            }
            ')' => {
                tok = token::Token::new(token::RPAREN.to_string(), self.ch.to_string());
            }
            ',' => {
                tok = token::Token::new(token::COMMA.to_string(), self.ch.to_string());
            }
            '{' => {
                tok = token::Token::new(token::LBRACE.to_string(), self.ch.to_string());
            }
            '}' => {
                tok = token::Token::new(token::RBRACE.to_string(), self.ch.to_string());
            }
            '\0' => {
                tok = token::Token::new(token::EOF.to_string(), "".to_string());
            }
            _ => {
                if self.is_letter() {
                    let literal = self.read_identifier();
                    return token::Token::new(token::lookup_ident(&literal), literal);
                } else if self.is_digit() {
                    return token::Token::new(token::INT.to_string(), self.read_number());
                } else {
                    tok = token::Token::new(token::ILLEGAL.to_string(), self.ch.to_string());
                }
            }
        }

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;

        while self.is_letter() {
            self.read_char();
        }

        self.input[pos..self.pos].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;

        while self.is_digit() {
            self.read_char();
        }

        self.input[pos..self.pos].iter().collect()
    }

    fn is_letter(&self) -> bool {
        self.ch.is_alphabetic() || self.ch.eq(&'_')
    }

    fn is_digit(&self) -> bool {
        self.ch.is_numeric()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.eq(&' ') || self.ch.eq(&'\t') || self.ch.eq(&'\n') || self.ch.eq(&'\r') {
            self.read_char();
        }
    }
}
