use super::{
    lexer::*, 
    token::*, 
    ast::*,
};
use std::{
    mem, 
    rc::Rc,
    collections::HashMap,
};
use Precedence::*;

#[derive(Clone)]
enum Precedence {
    LOWEST = 1,
    EQUALS = 2, // ==
    LESSGREATER = 3, // < or >
    SUM = 4, // +
    PRODUCT = 5, // *
    PREFIX = 6, // -X or !X
    CALL = 7, //myFunction(X)
}

type PrefixParseFn = Rc<dyn Fn(&mut Parser) -> Option<Box<dyn Expression>>>;
type InfixParseFn = Rc<dyn Fn(&mut Parser, Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>>>;


pub struct Parser {
    l: Lexer,
    pub errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
    precedences: HashMap<String, Precedence>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let cur = l.next_token();
        let peek = l.next_token();

        let mut p = Parser {
            l,
            errors: vec![],
            cur_token: cur,
            peek_token: peek,
            precedences: HashMap::from([
                (EQ.into(), Precedence::EQUALS),
                (NOT_EQ.into(), Precedence::EQUALS),
                (LT.into(), Precedence::LESSGREATER),
                (GT.into(), Precedence::LESSGREATER),
                (PLUS.into(), Precedence::SUM),
                (MINUS.into(), Precedence::SUM),
                (SLASH.into(), Precedence::PRODUCT),
                (ASTERICK.into(), Precedence::PRODUCT),
            ]),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(IDENT.to_string(), Rc::new(Parser::parse_identifier));
        p.register_prefix(INT.to_string(), Rc::new(Parser::parse_integer_literal));
        p.register_prefix(TRUE.to_string(), Rc::new(Parser::parse_boolean));
        p.register_prefix(FALSE.to_string(), Rc::new(Parser::parse_boolean));
        p.register_prefix(LPAREN.to_string(), Rc::new(Parser::parse_grouped_expression));
        p.register_prefix(BANG.to_string(), Rc::new(Parser::parse_prefix_expression));
        p.register_prefix(MINUS.to_string(), Rc::new(Parser::parse_prefix_expression));
        
        p.register_infix(PLUS.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(MINUS.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(SLASH.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(ASTERICK.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(EQ.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(NOT_EQ.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(LT.to_string(), Rc::new(Parser::parse_infix_expression));
        p.register_infix(GT.to_string(), Rc::new(Parser::parse_infix_expression));

        p
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program {
            statements: vec![],   
        };
        while !self.cur_token_is(EOF.to_string()) {
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
            _ => self.parse_expression_statement(),
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

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token = self.cur_token.clone();
        let stmt = ExpressionStatement {
            token: cur_token,
            expression: self.parse_expression(LOWEST as u8),
        };

        if self.peek_token_is(SEMICOLON.to_string()) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        self.errors.push(format!("no prefix parse function for {} found", t));
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.cur_token.token_type.clone());
            return None;
        } 

        let mut left_prefix = prefix.unwrap().clone()(self);

        while !self.peek_token_is(SEMICOLON.to_string()) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fns.get(&self.peek_token.token_type);
            if infix.is_none() {
                return left_prefix;
            }
            let infix = infix.unwrap().clone();

            self.next_token();

            left_prefix = infix(self, left_prefix);
        }

        left_prefix
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

    fn peek_precedence(&self) -> u8 {
        if let Some(prec) = self.precedences.get(&self.peek_token.token_type) {
            return (*prec).clone() as u8;    
        }

        LOWEST as u8
    }

    fn cur_precedece(&self) -> u8 {
        if let Some(prec) = self.precedences.get(&self.cur_token.token_type) {
            return (*prec).clone() as u8
        }

        LOWEST as u8
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn parse_identifier(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        return Some(Box::new(Identifier {
            token: parser.cur_token.clone(),
            value: parser.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let res = parser.cur_token.literal.parse::<i64>();

        if let Err(_) = res {
            parser.errors.push(format!("Could not parse {} as integer", parser.cur_token.literal));
            return None;
        } else {
            return Some(Box::new(IntegerLiteral {
                token: parser.cur_token.clone(),
                value: res.unwrap(),
            }))
        }
    }

    fn parse_boolean(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        Some(Box::new(Boolean { token: parser.cur_token.clone(), value: parser.cur_token_is(TRUE.to_string()) }))
    }

    fn parse_grouped_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        parser.next_token();

        let exp = parser.parse_expression(LOWEST as u8);

        if !parser.expect_peek(RPAREN.to_string()) {
            return None;
        }

        exp
    }

    fn parse_prefix_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
        let cur_token = parser.cur_token.clone();
        let literal = parser.cur_token.literal.clone();

        parser.next_token();

        Some(Box::new(PrefixExpression {
            token: cur_token,
            operator: literal,
            right: parser.parse_expression(PREFIX as u8),
        }))
    }

    fn parse_infix_expression(parser: &mut Parser, left: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>> {
        let mut expr = InfixExpression {
            token: parser.cur_token.clone(),
            operator: parser.cur_token.literal.clone(),
            left,
            right: None,
        };

        let precedence = parser.cur_precedece();
        parser.next_token();
        expr.right = parser.parse_expression(precedence);

        Some(Box::new(expr))
    }
}
