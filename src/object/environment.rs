use super::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: Rc<Object>) -> Rc<Object> {
        self.store.insert(name, Rc::clone(&val));
        val
    }

    /// Update an existing variable in this scope or any outer scope
    pub fn update(&mut self, name: &str, val: Rc<Object>) -> Option<Rc<Object>> {
        if self.store.contains_key(name) {
            self.store.insert(name.to_string(), Rc::clone(&val));
            return Some(val);
        }

        if let Some(outer) = &self.outer {
            return outer.borrow_mut().update(name, val);
        }

        None
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// Registry for pattern definitions
#[derive(Debug, Clone, Default)]
pub struct PatternRegistry {
    patterns: HashMap<String, PatternDef>,
}

#[derive(Debug, Clone)]
pub struct PatternDef {
    pub name: String,
    pub parameters: Vec<String>,
    pub condition: crate::ast::Expression,
}

impl PatternRegistry {
    pub fn new() -> Self {
        PatternRegistry {
            patterns: HashMap::new(),
        }
    }

    pub fn register(&mut self, name: String, parameters: Vec<String>, condition: crate::ast::Expression) {
        self.patterns.insert(name.clone(), PatternDef {
            name,
            parameters,
            condition,
        });
    }

    pub fn get(&self, name: &str) -> Option<&PatternDef> {
        self.patterns.get(name)
    }
}
