use crate::{
    ast::{Node, TNode},
    environment::Environment,
    evaluator::builtins::BuiltinFn,
    token::Str,
};
use std::{cell::RefCell, collections::BTreeMap, hash::DefaultHasher, hash::Hash, rc::Rc};
use std::{hash::Hasher, ptr::hash};

pub struct HashPair {
    key: Box<Object>,
    value: Box<Object>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(Str),
    Return(Box<Object>),
    Error(Str),
    Builtin(BuiltinFn),
    Function(Vec<Node>, Node, Rc<RefCell<Environment>>),
    Array(Vec<Object>),
    HashKey(Box<Object>, u64),
    Hash(BTreeMap<Object, Object>),
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
            Self::HashKey(value, _) => value.inspect(),
            Self::Hash(pairs) => {
                let strings: Vec<String> = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.inspect(), v.inspect()))
                    .collect();
                String::from("{") + &strings.join(", ") + "}"
            }
            Self::Null => "null".into(),
        };
    }

    pub fn hash_key(&self) -> Object {
        match self {
            Object::Boolean(loob) => {
                let value: u64;
                if *loob {
                    value = 1;
                } else {
                    value = 0;
                }
                Object::HashKey(Box::new(Object::Boolean(*loob)), value)
            }
            Object::Integer(value) => {
                Object::HashKey(Box::new(Object::Integer(*value)), *value as u64)
            }
            Object::String(string) => {
                let mut df = DefaultHasher::new();
                string.to_string().hash(&mut df);
                Object::HashKey(Box::new(Object::String(string.clone())), df.finish())
            }
            _ => Object::Null,
        }
    }
}

#[test]
fn test_string_hash_key() {
    let hello1 = Object::String("Hello World".into());
    let hello2 = Object::String("Hello World".into());
    let diff1 = Object::String("My name is johnny".into());
    let diff2 = Object::String("My name is johnny".into());

    if hello1.hash_key() != hello2.hash_key() {
        panic!("strings with same contents have different keys");
    }

    if diff1.hash_key() != diff2.hash_key() {
        panic!("strings with same contents have different keys");
    }

    if hello1.hash_key() == diff1.hash_key() {
        panic!("strings with different contents have same keys");
    }
}
