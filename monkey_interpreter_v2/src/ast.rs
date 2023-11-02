use crate::token::{Str, Token};

#[cfg(test)]
mod ast_test;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn string(&self) -> String;
}

pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    // Requires a Token::LET
    LetStatement { ident: Expr, value: Expr },
    // Requires a Token::Return
    ReturnStatement { value: Expr },
    ExpressionStatement { token: Token, expr: Expr },
    BlockStatement { stmts: Vec<Box<Stmt>> },
    Nil,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier {
        name: Str,
    },
    IntegerLiteral {
        value: (Str, i64),
    },
    PrefixExpression {
        token: Token,
        operator: Str,
        right: Box<Expr>,
    },
    InfixExpression {
        token: Token,
        left: Box<Expr>,
        operator: Str,
        right: Box<Expr>,
    },
    Boolean {
        value: bool,
    },
    IfExpression {
        condition: Box<Expr>,
        consequence: Box<Stmt>,
        alternative: Box<Stmt>,
    },
    FunctionLiteral {
        parameters: Vec<Box<Expr>>,
        body: Box<Stmt>,
    },
    CallExpression {
        function: Box<Expr>,
        arguments: Vec<Box<Expr>>,
    },
    Nil,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        "program"
    }

    fn string(&self) -> String {
        let mut buf = String::new();

        for stmt in &self.stmts {
            buf += &stmt.string();
        }

        buf
    }
}

impl Node for Stmt {
    fn token_literal(&self) -> &str {
        match self {
            Self::LetStatement { .. } => "let",
            Self::ReturnStatement { .. } => "return",
            Self::ExpressionStatement { token, .. } => match token {
                Token::LT => "<",
                Token::GT => ">",
                Token::EQ => "==",
                Token::PLUS => "+",
                Token::SLASH => "/",
                Token::MINUS => "-",
                Token::ASTERICK => "*",
                _ => todo!("Need to implement for {:?}", token),
            },
            Self::BlockStatement { .. } => "{",
            _ => "",
        }
    }

    fn string(&self) -> String {
        return match self {
            Self::LetStatement { ident, value } => {
                String::from(self.token_literal())
                    + " "
                    + ident.token_literal()
                    + " = "
                    + &value.string()
                    + ";"
            }
            Self::ReturnStatement { value } => {
                String::from(self.token_literal()) + " " + &value.string() + ";"
            }
            Self::ExpressionStatement { expr, .. } => expr.string(),
            Self::BlockStatement { stmts } => {
                let mut buf = String::new();

                stmts.iter().for_each(|stmt| buf.push_str(&stmt.string()));

                buf
            }
            _ => String::from("Not Implemented"),
        };
    }
}

impl Node for Expr {
    fn token_literal(&self) -> &str {
        match self {
            Self::Identifier { name } => &(*name),
            Self::IntegerLiteral {
                value: (str_value, _),
            } => &(*str_value),
            Self::PrefixExpression { operator, .. } => &(*operator),
            Self::InfixExpression { operator, .. } => &(*operator),
            Self::Boolean { value } => {
                if *value {
                    return "true";
                }
                "false"
            }
            Self::IfExpression { .. } => "if",
            Self::FunctionLiteral { .. } => "fn",
            Self::CallExpression { .. } => "(",
            _ => "",
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Identifier { .. } => String::from(self.token_literal()),
            Self::IntegerLiteral { value } => value.0.to_string(),
            Self::PrefixExpression {
                operator, right, ..
            } => String::from("(") + &(*operator) + &right.string() + ")",
            Self::InfixExpression {
                left,
                operator,
                right,
                ..
            } => {
                String::from("(")
                    + &left.string()
                    + " "
                    + &(*operator)
                    + " "
                    + &right.string()
                    + ")"
            }
            Self::Boolean { value } => value.to_string(),
            Self::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let mut buf = String::from(self.token_literal());

                buf.push_str(&(condition.string() + " " + &consequence.string()));

                if **alternative != Stmt::Nil {
                    buf.push_str("else ");
                    buf.push_str(&alternative.string());
                }

                buf
            }
            Self::FunctionLiteral { parameters, body } => {
                let mut buf = String::from(self.token_literal());

                let mut strings = vec![];
                parameters.iter().for_each(|s| strings.push(s.string()));

                buf.push_str("(");
                buf.push_str(&(strings.join(", ") + ") "));
                buf.push_str(&body.string());

                buf
            }
            Self::CallExpression {
                function,
                arguments,
            } => {
                let mut buf = String::new();

                let strings: Vec<String> = arguments.iter().map(|s| s.string()).collect();

                buf.push_str(&(function.string() + "("));
                buf.push_str(&(strings.join(", ") + ")"));

                buf
            }
            _ => String::from("Not Implemented"),
        }
    }
}
