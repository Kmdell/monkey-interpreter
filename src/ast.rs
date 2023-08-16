use super::token::*;
use std::rc::Rc;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
    fn into_let(&self) -> Result<&LetStatement, String> {
        Err("Not a LetStatement".to_string())
    }
    fn into_return(&self) -> Result<&ReturnStatement, String> {
        Err("Not a ReturnStatement".into())
    }
    fn into_expression(&self) -> Result<&ExpressionStatement, String> {
        Err("Not a ExpressionStatement".into())
    }
    fn into_block(&self) -> Result<&BlockStatement, String> {
        Err("Not a BlockStatement".into())
    }
    fn into_identifier(&self) -> Result<&Identifier, String> {
        Err("Not a Identifier".into())
    }
    fn into_integer_literal(&self) -> Result<&IntegerLiteral, String> {
        Err("Not a IntegerLiteral".into())
    }
    fn into_prefix(&self) -> Result<&PrefixExpression, String> {
        Err("Not a PrefixExpression".into())
    }
    fn into_infix(&self) -> Result<&InfixExpression, String> {
        Err("Not a InfixExpression".into())
    }
    fn into_bool(&self) -> Result<&Boolean, String> {
        Err("Not a Boolean".into())
    }
    fn into_if(&self) -> Result<&IfExpression, String> {
        Err("Not a IfExpression".into())
    }
    fn into_func(&self) -> Result<&FunctionLiteral, String> {
        Err("Not a FunctionLiteral".into())
    }
    fn into_call(&self) -> Result<&CallExpression, String> {
        Err("Not a CallExpression".into())
    }
    fn into_program(&self) -> Result<&Program, String> {
        Err("Not a Program".into())
    }
    fn into_node(&self) -> Rc<&dyn Node>;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return String::from("");
        }
    }

    fn to_string(&self) -> String {
        let mut buffer = String::new();

        self.statements.iter().for_each(|stmt| {
            buffer.push_str(&stmt.to_string());
        });

        buffer
    }

    fn into_program(&self) -> Result<&Program, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&(self.token_literal() + " " + &self.name.to_string() + " = "));

        if let Some(expression) = &self.value {
            buffer.push_str(&expression.to_string());
        }

        buffer + ";"
    }
    fn into_let(&self) -> Result<&LetStatement, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Statement for LetStatement {}

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
    fn into_identifier(&self) -> Result<&Identifier, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for Identifier {}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.token_literal());
        buffer.push_str(" ");

        if let Some(stmt) = &self.return_value {
            buffer.push_str(&stmt.to_string());
        }

        buffer + ";"
    }
    fn into_return(&self) -> Result<&ReturnStatement, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Statement for ReturnStatement {}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        if let Some(expression) = &self.expression {
            return expression.to_string();
        }
        return String::from("");
    }
    fn into_expression(&self) -> Result<&ExpressionStatement, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Statement for ExpressionStatement {}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
    fn into_integer_literal(&self) -> Result<&IntegerLiteral, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for IntegerLiteral {}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str("(");
        buffer.push_str(&self.operator);
        if let Some(expr) = &self.right {
            buffer.push_str(&expr.to_string());
        }
        buffer.push_str(")");

        buffer
    }
    fn into_prefix(&self) -> Result<&PrefixExpression, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for PrefixExpression {}

pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str("(");
        if let Some(expr) = &self.left {
            buffer.push_str(&expr.to_string());
        }
        buffer.push_str(" ");
        buffer.push_str(&self.operator);
        buffer.push_str(" ");
        if let Some(expr) = &self.right {
            buffer.push_str(&expr.to_string());
        }
        buffer.push_str(")");
        buffer
    }
    fn into_infix(&self) -> Result<&InfixExpression, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for InfixExpression {}

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn to_string(&self) -> String {
        println!("token literal: {}", self.token.literal);
        self.token.literal.clone()
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn into_bool(&self) -> Result<&Boolean, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for Boolean {}

pub struct IfExpression {
    pub token: Token,
    pub condition: Option<Box<dyn Expression>>,
    pub consequence: Option<Box<BlockStatement>>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buf = String::from("if");

        if let Some(cond) = &self.condition {
            buf.push_str(&cond.to_string());
        }

        buf.push_str(" ");

        if let Some(cons) = &self.consequence {
            buf.push_str(&cons.to_string());
        }

        if let Some(alt) = &self.alternative {
            buf.push_str("else ");
            buf.push_str(&alt.to_string());
        }

        buf
    }
    fn into_if(&self) -> Result<&IfExpression, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for IfExpression {}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buf = String::new();

        self.statements
            .iter()
            .for_each(|stmt| buf.push_str(&stmt.to_string()));

        buf
    }
    fn into_block(&self) -> Result<&BlockStatement, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Statement for BlockStatement {}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Box<dyn Expression>>,
    pub body: Option<Rc<BlockStatement>>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buf = self.token_literal();

        let para: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();

        buf.push_str("(");

        buf.push_str(&(para.join(", ")));

        buf.push_str(")");

        buf.push_str(
            &self
                .body
                .as_ref()
                .expect("There is not body for this function")
                .to_string(),
        );

        buf
    }
    fn into_func(&self) -> Result<&FunctionLiteral, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for FunctionLiteral {}

pub struct CallExpression {
    pub token: Token,
    pub function: Option<Box<dyn Expression>>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let mut buf = String::new();

        let mut args = vec![];
        self.arguments
            .iter()
            .for_each(|arg| args.push(arg.to_string()));

        buf.push_str(
            &self
                .function
                .as_ref()
                .expect("There is no function")
                .to_string(),
        );
        buf.push_str("(");
        buf.push_str(&args.join(", "));
        buf.push_str(")");

        buf
    }
    fn into_call(&self) -> Result<&CallExpression, String> {
        Ok(self)
    }
    fn into_node(&self) -> Rc<&dyn Node> {
        Rc::new(self)
    }
}

impl Expression for CallExpression {}
