use crate::{evaluator::NULL, object::*};
use std::{collections::HashMap, rc::Rc};

use super::new_error;

pub type Builtins = Rc<HashMap<String, Rc<dyn Object>>>;

pub fn new_builtins() -> Builtins {
    let mut builtins = HashMap::<String, Rc<dyn Object>>::new();
    for (name, func) in [
        ("len".into(), Rc::new(Builtin { func: Rc::new(len) })),
        (
            "first".into(),
            Rc::new(Builtin {
                func: Rc::new(first),
            }),
        ),
        (
            "last".into(),
            Rc::new(Builtin {
                func: Rc::new(last),
            }),
        ),
        (
            "rest".into(),
            Rc::new(Builtin {
                func: Rc::new(rest),
            }),
        ),
        (
            "push".into(),
            Rc::new(Builtin {
                func: Rc::new(push),
            }),
        ),
        (
            "puts".into(),
            Rc::new(Builtin {
                func: Rc::new(puts),
            }),
        ),
    ]
    .into_iter()
    {
        builtins.insert(name, func);
    }
    Rc::new(builtins)
}

fn len(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    if let Ok(ostring) = args[0].into_string() {
        return Rc::new(Integer {
            value: ostring.value.len() as i64,
        });
    } else if let Ok(array) = args[0].into_array() {
        return Rc::new(Integer {
            value: array.elements.len() as i64,
        });
    }

    new_error(format!(
        "argument to `len` not supported, got={}",
        args[0].object_type()
    ))
}

fn first(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }
    if args[0].object_type() != ARRAY_OBJ {
        return new_error(format!(
            "argument to `first` must be ARRAY, got {}",
            args[0].object_type()
        ));
    }

    if let Ok(arr) = args[0].into_array() {
        if arr.elements.len() > 0 {
            return arr.elements[0].clone();
        }
    }

    Rc::new(NULL)
}

fn last(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }
    if args[0].object_type() != ARRAY_OBJ {
        return new_error(format!(
            "argument to `last` must be ARRAY, got {}",
            args[0].object_type()
        ));
    }

    if let Ok(arr) = args[0].into_array() {
        let len = arr.elements.len();
        if len > 0 {
            return arr.elements[len - 1].clone();
        }
    }

    Rc::new(NULL)
}

fn rest(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    if args[0].object_type() != ARRAY_OBJ {
        return new_error(format!(
            "argument to `rest` must be ARRAY, got {}",
            args[0].object_type()
        ));
    }

    if let Ok(arr) = args[0].into_array() {
        let len = arr.elements.len();
        if len > 0 {
            let mut new_elems = vec![];
            for el in arr.elements[1..].iter() {
                new_elems.push(el.clone());
            }
            return Rc::new(Array {
                elements: new_elems,
            });
        }
    }

    Rc::new(NULL)
}

fn push(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    if args.len() != 2 {
        return new_error(format!(
            "wrong number of arguments, got={}, want=2",
            args.len()
        ));
    }

    if args[0].object_type() != ARRAY_OBJ {
        return new_error(format!(
            "argument to `push` must be ARRAY, got {}",
            args[0].object_type()
        ));
    }

    let arr = args[0].into_array().unwrap_or_else(|e| panic!("{}", e));
    let mut new_elems = vec![];
    for el in arr.elements.iter() {
        new_elems.push(el.clone());
    }
    new_elems.push(args[1].clone());

    return Rc::new(Array {
        elements: new_elems,
    });
}

fn puts(args: Vec<Rc<dyn Object>>) -> Rc<dyn Object> {
    for arg in args {
        println!("{}", arg.inspect());
    }

    return Rc::new(NULL);
}
