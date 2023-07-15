use super::token::*;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
    fn into_let(&self) -> Result<&LetStatement, String> {
        Err("Not a LetStatement".to_string())
    }
    fn into_return(&self) -> Result<&ReturnStatement, String> {
        Err("Not a ReturnStatement".into())
    }
    fn into_expression(&self) -> Result<&ExpressionStatement, String> {
        Err("Not a ExpressionStatement".into())
    }
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
}

impl Statement for LetStatement {
    fn into_let(&self) -> Result<&LetStatement, String> {
        Ok(self)
    }
}

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
}

impl Expression for Identifier {}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>
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
}

impl Statement for ReturnStatement {
    fn into_return(&self) -> Result<&ReturnStatement, String> {
        Ok(self)
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>
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
}

impl Statement for ExpressionStatement {
    fn into_expression(&self) -> Result<&ExpressionStatement, String> {
        Ok(self)
    }
}
