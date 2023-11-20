use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{object::Object, token::Str};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<Str, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(env),
        }
    }

    pub fn get(&self, name: Str) -> Option<Object> {
        if let Some(obj) = self.store.get(&name).map(|x| x.clone()) {
            return Some(obj);
        }

        if let Some(outer) = &self.outer {
            return outer.borrow().get(name).map(|x| x.clone());
        }

        None
    }

    pub fn set(&mut self, name: Str, obj: Object) -> Option<Object> {
        self.store.insert(name, obj)
    }
}
