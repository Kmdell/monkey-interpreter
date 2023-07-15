use super::{
    lexer::*, 
    token::*, 
    ast::*,
};
use std::mem;

#[derive(Debug, Clone)]
pub struct Parser {
    l: Lexer,
    pub errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let cur = l.next_token();
        let peek = l.next_token();

        Parser {
            l,
            errors: vec![],
            cur_token: cur,
            peek_token: peek,
        }
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program {
            statements: vec![],   
        };
        while !self.peek_token_is(EOF.to_string()) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        return match &self.cur_token.token_type[..] {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => None
        };
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token = self.cur_token.clone();
        if !self.expect_peek(IDENT.to_string()) {
            return None;
        }
        let stmt = LetStatement {
            token: cur_token,
            name: Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            },
            value: None,
        };

        if !self.expect_peek(ASSIGN.to_string()) {
            return None;
        }

        while !self.cur_token_is(SEMICOLON.to_string()) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token = self.cur_token.clone();
        let stmt = ReturnStatement {
            token: cur_token,
            return_value: None,
        };

        self.next_token();

        while !self.cur_token_is(SEMICOLON.to_string()) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type[..].to_string()) {
            self.next_token();
            return true;
        } else {
            self.peek_error(token_type);
            return false;
        }
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        token_type.eq(&self.peek_token.token_type)
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        token_type.eq(&self.cur_token.token_type)
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.iter().map(|x| x.to_string()).collect()
    }

    fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!("Expected next token to be {}, got {} instead", t, self.peek_token.token_type));
    }
}
