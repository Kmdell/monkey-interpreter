use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Node, TNode},
    environment::Environment,
    evaluator::builtins::BuiltinFn,
    token::Str,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(Str),
    Return(Box<Object>),
    Error(Str),
    Environment(Environment),
    Builtin(BuiltinFn),
    Function(Vec<Node>, Node, Rc<RefCell<Environment>>),
    Array(Vec<Object>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        return match self {
            Self::Integer(int) => int.to_string(),
            Self::Boolean(loob) => loob.to_string(),
            Self::String(value) => value.to_string(),
            Self::Return(ret) => ret.inspect(),
            Self::Error(msg) => format!("Error: {}", msg),
            Self::Environment(_) => "Environment".into(),
            Self::Function(params, body, ..) => {
                "fn(".to_string()
                    + &params
                        .iter()
                        .map(|par| (*par).string())
                        .collect::<Vec<String>>()
                        .join(", ")
                    + ") \n{"
                    + &body.string()
                    + "\n}"
            }
            Self::Builtin(value) => format!("builtin function: {:?}", value),
            Self::Array(elems) => {
                let elems: Vec<String> = elems.iter().map(|e| e.inspect()).collect();
                String::from("[") + &elems.join(", ") + "]"
            }
            Self::Null => "null".into(),
        };
    }
}
