use crate::object::*;
use std::{
    rc::Rc,
    collections::HashMap
};

pub struct Environment {
    store: HashMap<String, Rc<dyn Object>>
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: HashMap::new() }
    }

    pub fn get(&self, name: &String) -> Option<Rc<dyn Object>> {
        if let Some(obj) = self.store.get(name) {
            return Some(obj.clone());
        }
        None
    }

    pub fn set(&mut self, name: String, val: Rc<dyn Object>) -> Option<Rc<dyn Object>> {
        self.store.insert(name, val)
    }
}


