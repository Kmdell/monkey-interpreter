use std::{
    collections::HashMap,
    mem::{self, swap},
    rc::Rc,
};

use crate::{
    ast::{Expr, Node, Program, Stmt},
    lexer::Lexer,
    token::{Str, Token},
};

#[cfg(test)]
mod parser_test;

type PrefixParseFn = Rc<dyn Fn(&mut Parser) -> Expr>;
type InfixParseFn = Rc<dyn Fn(&mut Parser, Expr) -> Expr>;

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
enum Prec {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
    precedences: HashMap<Token, Prec>,
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Self {
        let cur = l.next_token();
        let peek = l.next_token();

        let mut p = Parser {
            l,
            errors: vec![],
            cur_token: cur,
            peek_token: peek,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: HashMap::from([
                (Token::EQ, Prec::EQUALS),
                (Token::NOTEQ, Prec::EQUALS),
                (Token::LT, Prec::LESSGREATER),
                (Token::GT, Prec::LESSGREATER),
                (Token::PLUS, Prec::SUM),
                (Token::MINUS, Prec::SUM),
                (Token::SLASH, Prec::PRODUCT),
                (Token::ASTERICK, Prec::PRODUCT),
                (Token::LPAREN, Prec::CALL),
                (Token::LBRACE, Prec::INDEX),
            ]),
        };

        p.register_prefix(&Token::IDENT("".into()), Rc::new(parse_identifier));
        p.register_prefix(&Token::INT("".into()), Rc::new(parse_integer_literal));
        p.register_prefix(&Token::STRING("".into()), Rc::new(parse_string_literal));
        p.register_prefix(&Token::BANG, Rc::new(parse_prefix_expression));
        p.register_prefix(&Token::MINUS, Rc::new(parse_prefix_expression));
        p.register_prefix(&Token::TRUE, Rc::new(parse_boolean));
        p.register_prefix(&Token::FALSE, Rc::new(parse_boolean));
        p.register_prefix(&Token::LPAREN, Rc::new(parse_grouped_expression));
        p.register_prefix(&Token::IF, Rc::new(parse_if_expression));
        p.register_prefix(&Token::FUNCTION, Rc::new(parse_function_literal));
        p.register_prefix(&Token::LBRACE, Rc::new(parse_array_literal));

        p.register_infix(&Token::PLUS, Rc::new(parse_infix_expression));
        p.register_infix(&Token::MINUS, Rc::new(parse_infix_expression));
        p.register_infix(&Token::SLASH, Rc::new(parse_infix_expression));
        p.register_infix(&Token::ASTERICK, Rc::new(parse_infix_expression));
        p.register_infix(&Token::EQ, Rc::new(parse_infix_expression));
        p.register_infix(&Token::NOTEQ, Rc::new(parse_infix_expression));
        p.register_infix(&Token::LT, Rc::new(parse_infix_expression));
        p.register_infix(&Token::GT, Rc::new(parse_infix_expression));
        p.register_infix(&Token::LPAREN, Rc::new(parse_call_expression));
        p.register_infix(&Token::LBRACE, Rc::new(parse_index_expression));

        p
    }

    fn next_token(&mut self) {
        swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut prog = Program { stmts: vec![] };

        while self.cur_token != Token::EOF {
            let stmt: Stmt = self.parse_statement();
            if stmt != Stmt::Nil {
                prog.stmts.push(Node::Statement(Box::new(stmt)));
            }
            self.next_token();
        }

        prog
    }

    fn parse_statement(&mut self) -> Stmt {
        match self.cur_token {
            Token::LET => self.parse_let_stmt(),
            Token::RETURN => self.parse_return_stmt(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt {
        if !self.expect_peek(&Token::IDENT("any identifier".into())) {
            return Stmt::Nil;
        }

        let Token::IDENT(ref val) = self.cur_token else {
            return Stmt::Nil;
        };
        let ident = Node::Expression(Box::new(Expr::Identifier { name: val.clone() }));

        if !self.expect_peek(&Token::ASSIGN) {
            return Stmt::Nil;
        }

        self.next_token();

        let value = Node::Expression(Box::new(self.parse_expression(Prec::LOWEST)));

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Stmt::LetStatement { ident, value }
    }

    fn parse_return_stmt(&mut self) -> Stmt {
        self.next_token();

        let value = Node::Expression(Box::new(self.parse_expression(Prec::LOWEST)));

        while self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Stmt::ReturnStatement { value }
    }

    fn parse_expression_stmt(&mut self) -> Stmt {
        let stmt = Stmt::ExpressionStatement {
            expr: Node::Expression(Box::new(self.parse_expression(Prec::LOWEST))),
        };

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        stmt
    }

    fn parse_expression(&mut self, precedence: Prec) -> Expr {
        let func = match self.cur_token {
            Token::IDENT(_) => self.prefix_parse_fns.get(&Token::IDENT("".into())),
            Token::INT(_) => self.prefix_parse_fns.get(&Token::INT("".into())),
            Token::STRING(_) => self.prefix_parse_fns.get(&Token::STRING("".into())),
            _ => self.prefix_parse_fns.get(&self.cur_token),
        };

        if let Some(func) = func {
            let func = func.clone();
            let mut left_expr = func(self);

            while !self.peek_token_is(&Token::SEMICOLON) && precedence < self.peek_precedence() {
                if let Some(infix) = self.infix_parse_fns.get(&self.peek_token) {
                    let infix = infix.clone();
                    self.next_token();
                    left_expr = infix(self, left_expr);
                } else {
                    return left_expr;
                }
            }
            return left_expr;
        }

        self.no_prefix_parse_fn_error();
        Expr::Nil
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        mem::discriminant(token) == mem::discriminant(&self.peek_token)
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        mem::discriminant(token) == mem::discriminant(&self.cur_token)
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true;
        }

        self.error_peek(token);
        false
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn error_peek(&mut self, token: &Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            *token, self.peek_token
        ))
    }

    fn no_prefix_parse_fn_error(&mut self) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            self.cur_token
        ));
    }

    fn register_prefix(&mut self, token: &Token, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token.clone(), func);
    }

    fn register_infix(&mut self, token: &Token, func: InfixParseFn) {
        self.infix_parse_fns.insert(token.clone(), func);
    }

    fn peek_precedence(&self) -> Prec {
        if let Some(prec) = self.precedences.get(&self.peek_token) {
            return *prec;
        }

        Prec::LOWEST
    }

    fn cur_precedence(&self) -> Prec {
        if let Some(prec) = self.precedences.get(&self.cur_token) {
            return *prec;
        }

        Prec::LOWEST
    }

    fn parse_block_statement(&mut self) -> Stmt {
        let mut stmts = vec![];

        self.next_token();
        while !self.cur_token_is(&Token::RBRACE) && !self.cur_token_is(&Token::EOF) {
            let stmt = self.parse_statement();
            if stmt != Stmt::Nil {
                stmts.push(Node::Statement(Box::new(stmt)));
            }
            self.next_token();
        }

        Stmt::BlockStatement { stmts }
    }

    fn parse_function_parameters(&mut self) -> Vec<Node> {
        let mut idents = vec![];

        if self.peek_token_is(&Token::RPAREN) {
            self.next_token();
            return idents;
        }

        self.next_token();

        let Token::IDENT(ref string) = self.cur_token else {
            return vec![Node::Nil];
        };
        let ident = Expr::Identifier {
            name: string.clone(),
        };
        idents.push(Node::Expression(Box::new(ident)));

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            let Token::IDENT(ref string) = self.cur_token else {
                return vec![Node::Nil];
            };
            let ident = Expr::Identifier {
                name: string.clone(),
            };
            idents.push(Node::Expression(Box::new(ident)));
        }

        if !self.expect_peek(&Token::RPAREN) {
            return vec![Node::Nil];
        }

        idents
    }

    fn parse_expression_list(&mut self, token: Token) -> Vec<Node> {
        let mut list = vec![];

        if self.peek_token_is(&token) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(Node::Expression(Box::new(
            self.parse_expression(Prec::LOWEST),
        )));

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            list.push(Node::Expression(Box::new(
                self.parse_expression(Prec::LOWEST),
            )));
        }

        if !self.expect_peek(&token) {
            return vec![];
        }

        list
    }
}

fn parse_identifier(parser: &mut Parser) -> Expr {
    if let Token::IDENT(name) = &parser.cur_token {
        return Expr::Identifier { name: name.clone() };
    }
    Expr::Nil
}

fn parse_integer_literal(parser: &mut Parser) -> Expr {
    let Token::INT(int) = &parser.cur_token else {
        parser
            .errors
            .push(format!("could not parse {:?} as integer", parser.cur_token));
        return Expr::Nil;
    };

    if let Ok(int_value) = int.parse::<i64>() {
        return Expr::IntegerLiteral {
            value: (int.clone(), int_value),
        };
    }
    Expr::Nil
}

fn parse_string_literal(parser: &mut Parser) -> Expr {
    let Token::STRING(value) = &parser.cur_token else {
        parser
            .errors
            .push(format!("could not parse {:?} as string", parser.cur_token));
        return Expr::Nil;
    };
    Expr::StringLiteral {
        value: value.clone(),
    }
}

fn parse_prefix_expression(parser: &mut Parser) -> Expr {
    let operator: Str;

    match parser.cur_token {
        Token::BANG => operator = "!".into(),
        Token::MINUS => operator = "-".into(),
        _ => return Expr::Nil,
    }

    parser.next_token();

    Expr::PrefixExpression {
        operator,
        right: Node::Expression(Box::new(parser.parse_expression(Prec::PREFIX))),
    }
}

fn parse_array_literal(parser: &mut Parser) -> Expr {
    Expr::ArrayLiteral {
        elements: parser.parse_expression_list(Token::RBRACE),
    }
}

fn parse_boolean(parser: &mut Parser) -> Expr {
    Expr::Boolean {
        value: parser.cur_token_is(&Token::TRUE),
    }
}

fn parse_grouped_expression(parser: &mut Parser) -> Expr {
    parser.next_token();

    let expr = parser.parse_expression(Prec::LOWEST);

    if !parser.expect_peek(&Token::RPAREN) {
        return Expr::Nil;
    }

    expr
}

fn parse_function_literal(parser: &mut Parser) -> Expr {
    if !parser.expect_peek(&Token::LPAREN) {
        return Expr::Nil;
    }

    let parameters = parser.parse_function_parameters();

    if !parser.expect_peek(&Token::LBRACE) {
        return Expr::Nil;
    }

    Expr::FunctionLiteral {
        parameters,
        body: Node::Statement(Box::new(parser.parse_block_statement())),
    }
}

fn parse_if_expression(parser: &mut Parser) -> Expr {
    if !parser.expect_peek(&Token::LPAREN) {
        return Expr::Nil;
    }

    parser.next_token();

    let condition = Node::Expression(Box::new(parser.parse_expression(Prec::LOWEST)));

    if !parser.expect_peek(&Token::RPAREN) {
        return Expr::Nil;
    }

    if !parser.expect_peek(&Token::LBRACE) {
        return Expr::Nil;
    }

    let consequence = Node::Statement(Box::new(parser.parse_block_statement()));

    let mut alternative = Node::Statement(Box::new(Stmt::Nil));
    if parser.peek_token_is(&Token::ELSE) {
        parser.next_token();

        if !parser.expect_peek(&Token::LBRACE) {
            return Expr::Nil;
        }

        alternative = Node::Statement(Box::new(parser.parse_block_statement()));
    }

    Expr::IfExpression {
        condition,
        consequence,
        alternative,
    }
}

fn parse_infix_expression(parser: &mut Parser, left: Expr) -> Expr {
    let operator: Str;

    match parser.cur_token {
        Token::PLUS => operator = "+".into(),
        Token::MINUS => operator = "-".into(),
        Token::ASTERICK => operator = "*".into(),
        Token::SLASH => operator = "/".into(),
        Token::LT => operator = "<".into(),
        Token::GT => operator = ">".into(),
        Token::EQ => operator = "==".into(),
        Token::NOTEQ => operator = "!=".into(),
        _ => return Expr::Nil,
    }
    let prec = parser.cur_precedence();

    parser.next_token();

    Expr::InfixExpression {
        left: Node::Expression(Box::new(left)),
        operator,
        right: Node::Expression(Box::new(parser.parse_expression(prec))),
    }
}

fn parse_call_expression(parser: &mut Parser, function: Expr) -> Expr {
    Expr::CallExpression {
        function: Node::Expression(Box::new(function)),
        arguments: parser.parse_expression_list(Token::RPAREN),
    }
}

fn parse_index_expression(parser: &mut Parser, left: Expr) -> Expr {
    parser.next_token();
    let index = parser.parse_expression(Prec::LOWEST);

    if !parser.expect_peek(&Token::RBRACE) {
        return Expr::Nil;
    }

    Expr::IndexExpression {
        left: Node::Expression(Box::new(left)),
        index: Node::Expression(Box::new(index)),
    }
}
