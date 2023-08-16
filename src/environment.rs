use crate::object::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Environment {
    store: HashMap<String, Rc<dyn Object>>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enc(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer.clone()),
        }
    }

    pub fn get(&self, name: &String) -> Option<Rc<dyn Object>> {
        let mut val = None;
        if let Some(obj) = self.store.get(name) {
            val = Some(obj.clone());
        } else if let Some(outer) = self.outer.clone() {
            if let Some(obj) = outer.borrow().get(name) {
                val = Some(obj.clone());
            }
        }
        val
    }

    pub fn set(&mut self, name: String, val: Rc<dyn Object>) -> Option<Rc<dyn Object>> {
        self.store.insert(name, val)
    }
}
