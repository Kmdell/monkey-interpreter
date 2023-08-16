use std::rc::Rc;
use std::cell::RefCell;

use crate::{ast::{Identifier, BlockStatement, Node}, environment::Environment};

pub type ObjectType = String;

pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN";
pub const ERROR_OBJ: &str = "ERROR";
pub const FUNCTION_OBJ: &str = "FUNCTION";

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn into_int(&self) -> Result<&Integer, String> {
        Err("Not an Integer".into())
    }
    fn into_bool(&self) -> Result<&Boolean, String> {
        Err("Not a Boolean".into())
    }
    fn into_null(&self) -> Result<&Null, String> {
        Err("Not a Null".into())
    }
    fn into_return(&self) -> Result<&ReturnValue, String> {
        Err("Not a ReturnValue".into())
    }
    fn into_error(&self) -> Result<&Error, String> {
        Err("Not a Error".into())
    }
    fn into_fn(&self) -> Result<&Function, String> {
        Err("Not a Function".into())
    }
}

pub struct Integer {
    pub value: i64
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        INTEGER_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn into_int(&self) -> Result<&Integer, String> {
        Ok(self)
    }
}

pub struct Boolean {
    pub value: bool
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        BOOLEAN_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn into_bool(&self) -> Result<&Boolean, String> {
        Ok(self)
    }
}

pub struct Null {}

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        NULL_OBJ.into()
    }
    fn inspect(&self) -> String {
        "null".into()
    }
    fn into_null(&self) -> Result<&Null, String> {
        Ok(self)
    }
}

pub struct ReturnValue {
    pub value: Rc<dyn Object>
}

impl Object for ReturnValue {
    fn object_type(&self) -> ObjectType {
        RETURN_VALUE_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.inspect()
    }
    fn into_return(&self) -> Result<&ReturnValue, String> {
        Ok(self)
    }
}

pub struct Error {
    pub message: String
}

impl Object for Error {
    fn object_type(&self) -> ObjectType {
        ERROR_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.message.clone()
    }
    fn into_error(&self) -> Result<&Error, String> {
        Ok(self)
    }
}

pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: Rc<BlockStatement>,
    pub env: Rc<RefCell<Environment>>
}

impl Object for Function {
    fn object_type(&self) -> ObjectType {
        FUNCTION_OBJ.into()
    }
    fn inspect(&self) -> String {
        let mut buf = String::new();

        let mut params = vec![];
        self.parameters.iter().for_each(|p| params.push(p.to_string()));

        buf.push_str("fn(");
        buf.push_str(&params.join(", "));
        buf.push_str(") {\n");
        buf.push_str(&self.body.to_string());
        buf.push_str("}\n");

        buf
    }
    fn into_fn(&self) -> Result<&Function, String> {
        Ok(self)
    }
}
