use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    ast::{BlockStatement, Identifier, Node},
    environment::Environment,
};

pub type ObjectType = String;
type BuiltinFunction = Rc<dyn Fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object>>;

pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN";
pub const ERROR_OBJ: &str = "ERROR";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const STRING_OBJ: &str = "STRING";
pub const BUILTIN_OBJ: &str = "BUILTIN";
pub const ARRAY_OBJ: &str = "ARRAY";

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn into_int(&self) -> Result<&Integer, String> {
        Err(format!("Not an Integer, got={}", self.object_type()))
    }
    fn into_bool(&self) -> Result<&Boolean, String> {
        Err(format!("Not a Boolean, got={}", self.object_type()))
    }
    fn into_null(&self) -> Result<&Null, String> {
        Err(format!("Not a Null, got={}", self.object_type()))
    }
    fn into_return(&self) -> Result<&ReturnValue, String> {
        Err(format!("Not a ReturnValue, got={}", self.object_type()))
    }
    fn into_error(&self) -> Result<&Error, String> {
        Err(format!("Not a Error, got={}", self.object_type()))
    }
    fn into_fn(&self) -> Result<&Function, String> {
        Err(format!("Not a Function, got={}", self.object_type()))
    }
    fn into_string(&self) -> Result<&OString, String> {
        Err(format!("Not a OString, got={}", self.object_type()))
    }
    fn into_built(&self) -> Result<&Builtin, String> {
        Err(format!("Not a Builtin, got={}", self.object_type()))
    }
    fn into_array(&self) -> Result<&Array, String> {
        Err(format!("Not an Array, got={}", self.object_type()))
    }
}

pub struct Integer {
    pub value: i64,
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
    pub value: bool,
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
    pub value: Rc<dyn Object>,
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
    pub message: String,
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
    pub env: Rc<RefCell<Environment>>,
}

impl Object for Function {
    fn object_type(&self) -> ObjectType {
        FUNCTION_OBJ.into()
    }
    fn inspect(&self) -> String {
        let mut buf = String::new();

        let mut params = vec![];
        self.parameters
            .iter()
            .for_each(|p| params.push(p.to_string()));

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

pub struct OString {
    pub value: String,
}

impl Object for OString {
    fn object_type(&self) -> ObjectType {
        STRING_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.clone()
    }
    fn into_string(&self) -> Result<&OString, String> {
        Ok(self)
    }
}

pub struct Builtin {
    pub func: BuiltinFunction,
}

impl Object for Builtin {
    fn object_type(&self) -> ObjectType {
        BUILTIN_OBJ.into()
    }
    fn inspect(&self) -> String {
        "builtin function".into()
    }
    fn into_built(&self) -> Result<&Builtin, String> {
        Ok(self)
    }
}

pub struct Array {
    pub elements: Vec<Rc<dyn Object>>,
}

impl Object for Array {
    fn object_type(&self) -> ObjectType {
        ARRAY_OBJ.into()
    }
    fn inspect(&self) -> String {
        let mut buf = String::from("[");
        let mut elems = vec![];
        for el in self.elements.iter() {
            elems.push(el.inspect());
        }
        buf.push_str(&elems.join(", "));
        buf.push_str("]");

        buf
    }
    fn into_array(&self) -> Result<&Array, String> {
        Ok(self)
    }
}
