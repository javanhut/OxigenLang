use super::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    type_constraints: HashMap<String, String>,
    immutable_vars: HashSet<String>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            type_constraints: HashMap::new(),
            immutable_vars: HashSet::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            type_constraints: HashMap::new(),
            immutable_vars: HashSet::new(),
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

    pub fn set_typed(
        &mut self,
        name: String,
        val: Rc<Object>,
        type_name: String,
        immutable: bool,
    ) -> Rc<Object> {
        self.store.insert(name.clone(), Rc::clone(&val));
        self.type_constraints.insert(name.clone(), type_name);
        if immutable {
            self.immutable_vars.insert(name);
        } else {
            self.immutable_vars.remove(&name);
        }
        val
    }

    pub fn is_immutable(&self, name: &str) -> bool {
        if self.store.contains_key(name) {
            return self.immutable_vars.contains(name);
        }
        match &self.outer {
            Some(outer) => outer.borrow().is_immutable(name),
            None => false,
        }
    }

    pub fn get_type_constraint(&self, name: &str) -> Option<String> {
        match self.type_constraints.get(name) {
            Some(tc) => Some(tc.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get_type_constraint(name),
                None => None,
            },
        }
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

    #[allow(dead_code)]
    pub fn get_all(&self) -> &HashMap<String, Rc<Object>> {
        &self.store
    }

    /// Collect all visible variable names from this scope and all outer scopes.
    pub fn all_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.store.keys().cloned().collect();
        if let Some(outer) = &self.outer {
            keys.extend(outer.borrow().all_keys());
        }
        keys.sort();
        keys.dedup();
        keys
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
    pub parameters: Vec<String>,
    pub condition: crate::ast::Expression,
}

impl PatternRegistry {
    pub fn new() -> Self {
        PatternRegistry {
            patterns: HashMap::new(),
        }
    }

    pub fn register(
        &mut self,
        name: String,
        parameters: Vec<String>,
        condition: crate::ast::Expression,
    ) {
        self.patterns.insert(
            name,
            PatternDef {
                parameters,
                condition,
            },
        );
    }

    pub fn get(&self, name: &str) -> Option<&PatternDef> {
        self.patterns.get(name)
    }
}
