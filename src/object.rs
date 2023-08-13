pub type ObjectType = String;

pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";

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