use crate::token::Str;

#[cfg(test)]
mod ast_test;

pub trait TNode {
    fn token_literal(&self) -> &str;
    fn string(&self) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program {
    pub stmts: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Node {
    Statement(Box<Stmt>),
    Expression(Box<Expr>),
    Program(Box<Program>),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    // Requires a Token::LET
    LetStatement { ident: Node, value: Node },
    // Requires a Token::Return
    ReturnStatement { value: Node },
    ExpressionStatement { expr: Node },
    BlockStatement { stmts: Vec<Node> },
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Identifier {
        name: Str,
    },
    IntegerLiteral {
        value: (Str, i64),
    },
    PrefixExpression {
        operator: Str,
        right: Node,
    },
    InfixExpression {
        left: Node,
        operator: Str,
        right: Node,
    },
    Boolean {
        value: bool,
    },
    IfExpression {
        condition: Node,
        consequence: Node,
        alternative: Node,
    },
    FunctionLiteral {
        parameters: Vec<Node>,
        body: Node,
    },
    CallExpression {
        function: Node,
        arguments: Vec<Node>,
    },
    StringLiteral {
        value: Str,
    },
    ArrayLiteral {
        elements: Vec<Node>,
    },
    IndexExpression {
        left: Node,
        index: Node,
    },
    Nil,
}

impl TNode for Node {
    fn token_literal(&self) -> &str {
        match self {
            Self::Program(prog) => prog.token_literal(),
            Self::Statement(stmt) => stmt.token_literal(),
            Self::Expression(expr) => expr.token_literal(),
            Self::Nil => "Nil",
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Program(prog) => prog.string(),
            Self::Statement(stmt) => stmt.string(),
            Self::Expression(expr) => expr.string(),
            Self::Nil => self.token_literal().into(),
        }
    }
}

impl TNode for Program {
    fn string(&self) -> String {
        let mut buf = String::new();

        for stmt in &self.stmts {
            buf += &stmt.string();
        }

        buf
    }

    fn token_literal(&self) -> &str {
        "program"
    }
}

impl TNode for Stmt {
    fn token_literal(&self) -> &str {
        match self {
            Self::LetStatement { .. } => "let",
            Self::ReturnStatement { .. } => "return",
            Self::ExpressionStatement { .. } => "expression",
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

impl TNode for Expr {
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
            Self::StringLiteral { value } => &(*value),
            Self::ArrayLiteral { .. } => "[",
            Self::IndexExpression { .. } => "[",
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
                let Node::Statement(alt) = alternative else {
                    return buf + "}";
                };

                if **alt != Stmt::Nil {
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
            Self::StringLiteral { .. } => String::from(self.token_literal()),
            Self::ArrayLiteral { elements } => {
                let entities: Vec<String> = elements.iter().map(|elem| elem.string()).collect();

                String::from("[") + &entities.join(", ") + "]"
            }
            Self::IndexExpression { left, index } => {
                String::from("(") + &left.string() + "[" + &index.string() + "])"
            }
            _ => String::from("Not Implemented"),
        }
    }
}
