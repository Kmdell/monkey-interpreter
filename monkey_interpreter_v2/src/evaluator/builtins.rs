use std::collections::HashMap;

use crate::{object::Object, token::Str};

use super::{new_error, NULL};

pub type BuiltinFn = fn(Vec<Object>) -> Object;
pub type Builtins = HashMap<Str, Object>;

pub fn new_builtins() -> Builtins {
    let mut builtins = HashMap::<Str, Object>::new();
    for (name, func) in [
        ("len".into(), len as fn(Vec<Object>) -> Object),
        ("first".into(), first as fn(Vec<Object>) -> Object),
        ("last".into(), last as fn(Vec<Object>) -> Object),
        ("rest".into(), rest as fn(Vec<Object>) -> Object),
        ("push".into(), push as fn(Vec<Object>) -> Object),
        ("puts".into(), puts as fn(Vec<Object>) -> Object),
    ] {
        builtins.insert(name, Object::Builtin(func));
    }

    builtins
}

pub fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0] {
        Object::String(ref string) => Object::Integer(string.len() as i64),
        Object::Array(ref elems) => Object::Integer(elems.len() as i64),
        _ => new_error(format!(
            "argument to `len` not supported, got {:?}",
            args[0]
        )),
    }
}

pub fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0] {
        Object::String(ref string) => {
            if string.len() > 0 {
                return Object::String(string[..1].into());
            }
        }
        Object::Array(ref elems) => {
            if let Some(obj) = elems.get(0) {
                return obj.clone();
            }
        }
        _ => {
            return new_error(format!(
                "argument to `first` must be ARRAY or STRING, got={:?}",
                args[0]
            ))
        }
    }

    NULL
}

pub fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0] {
        Object::String(ref string) => {
            if string.len() > 0 {
                return Object::String(string[string.len() - 1..].into());
            }
        }
        Object::Array(ref elems) => {
            if let Some(obj) = elems.last() {
                return obj.clone();
            }
        }
        _ => {
            return new_error(format!(
                "argument to `last` must be ARRAY or STRING, got={:?}",
                args[0]
            ))
        }
    }

    NULL
}

pub fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match args[0] {
        Object::String(ref string) => {
            if string.len() > 0 {
                return Object::String(string[1..].into());
            }
        }
        Object::Array(ref elems) => {
            if elems.len() > 0 {
                return Object::Array(elems[1..].to_vec());
            }
        }
        _ => {
            return new_error(format!(
                "argument to `rest` must be ARRAY or STRING, got={:?}",
                args[0]
            ))
        }
    }

    NULL
}

pub fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }

    match args[0] {
        Object::String(ref string) => {
            let mut string = string.to_string();
            string.push_str(&args[1].inspect());
            return Object::String(string.into());
        }
        Object::Array(ref elems) => {
            let mut elems = elems.clone();
            elems.push(args[1].clone());
            return Object::Array(elems);
        }
        _ => {
            return new_error(format!(
                "argument to `push` must be ARRAY or STRING, got={:?}",
                args[0]
            ))
        }
    }
}

pub fn puts(args: Vec<Object>) -> Object {
    args.iter().for_each(|arg| println!("{}", arg.inspect()));
    NULL
}
