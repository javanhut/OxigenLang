pub mod builtins;

use crate::ast::{Expression, Identifier, Program, Statement, TypeAnnotation};
use crate::object::environment::{Environment, PatternRegistry};
use crate::object::Object;
use builtins::get_builtins;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Evaluator {
    builtins: HashMap<String, Rc<Object>>,
    patterns: PatternRegistry,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            builtins: get_builtins(),
            patterns: PatternRegistry::new(),
        }
    }

    fn convert_to_type(obj: &Rc<Object>, target: &str) -> Result<Rc<Object>, String> {
        match target {
            "INTEGER" => match obj.as_ref() {
                Object::Integer(_) => Ok(Rc::clone(obj)),
                Object::Float(f) => Ok(Rc::new(Object::Integer(*f as i64))),
                Object::String(s) => s.parse::<i64>()
                    .map(|n| Rc::new(Object::Integer(n)))
                    .map_err(|_| format!("cannot convert STRING \"{}\" to INTEGER", s)),
                Object::Char(c) => Ok(Rc::new(Object::Integer(*c as i64))),
                Object::Boolean(b) => Ok(Rc::new(Object::Integer(if *b { 1 } else { 0 }))),
                _ => Err(format!("cannot convert {} to INTEGER", obj.type_name())),
            },
            "FLOAT" => match obj.as_ref() {
                Object::Float(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => Ok(Rc::new(Object::Float(*n as f64))),
                Object::String(s) => s.parse::<f64>()
                    .map(|f| Rc::new(Object::Float(f)))
                    .map_err(|_| format!("cannot convert STRING \"{}\" to FLOAT", s)),
                Object::Boolean(b) => Ok(Rc::new(Object::Float(if *b { 1.0 } else { 0.0 }))),
                _ => Err(format!("cannot convert {} to FLOAT", obj.type_name())),
            },
            "STRING" => match obj.as_ref() {
                Object::String(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => Ok(Rc::new(Object::String(n.to_string()))),
                Object::Float(f) => Ok(Rc::new(Object::String(f.to_string()))),
                Object::Char(c) => Ok(Rc::new(Object::String(c.to_string()))),
                Object::Boolean(b) => Ok(Rc::new(Object::String(if *b { "True".to_string() } else { "False".to_string() }))),
                _ => Err(format!("cannot convert {} to STRING", obj.type_name())),
            },
            "CHAR" => match obj.as_ref() {
                Object::Char(_) => Ok(Rc::clone(obj)),
                Object::Integer(n) => {
                    if let Some(c) = char::from_u32(*n as u32) {
                        Ok(Rc::new(Object::Char(c)))
                    } else {
                        Err(format!("cannot convert INTEGER {} to CHAR", n))
                    }
                }
                Object::String(s) => {
                    let mut chars = s.chars();
                    if let Some(c) = chars.next() {
                        if chars.next().is_none() {
                            Ok(Rc::new(Object::Char(c)))
                        } else {
                            Err(format!("cannot convert multi-char STRING to CHAR"))
                        }
                    } else {
                        Err("cannot convert empty STRING to CHAR".to_string())
                    }
                }
                _ => Err(format!("cannot convert {} to CHAR", obj.type_name())),
            },
            "BOOLEAN" => match obj.as_ref() {
                Object::Boolean(_) => Ok(Rc::clone(obj)),
                _ => Ok(Rc::new(Object::Boolean(obj.is_truthy()))),
            },
            "ARRAY" => match obj.as_ref() {
                Object::Array(_) => Ok(Rc::clone(obj)),
                _ => Err(format!("cannot convert {} to ARRAY", obj.type_name())),
            },
            _ => Err(format!("unknown target type: {}", target)),
        }
    }

    fn zero_value_for_type(type_ann: &TypeAnnotation) -> Rc<Object> {
        match type_ann {
            TypeAnnotation::Int => Rc::new(Object::Integer(0)),
            TypeAnnotation::Float => Rc::new(Object::Float(0.0)),
            TypeAnnotation::Str => Rc::new(Object::String(String::new())),
            TypeAnnotation::Char => Rc::new(Object::Char('\0')),
            TypeAnnotation::Bool => Rc::new(Object::Boolean(false)),
            TypeAnnotation::Array => Rc::new(Object::Array(Vec::new())),
        }
    }

    pub fn eval_program(&mut self, program: &Program, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        let mut result = Rc::new(Object::None);

        for stmt in &program.statements {
            result = self.eval_statement(stmt, Rc::clone(&env));

            // Handle return values and errors
            match result.as_ref() {
                Object::Return(val) => return Rc::clone(val),
                Object::Error(_) => return result,
                _ => {}
            }
        }

        result
    }

    pub fn eval_statement(&mut self, stmt: &Statement, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        match stmt {
            Statement::Let { name, value } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                // If variable exists in any scope, update it; otherwise create new
                let existing = env.borrow().get(&name.value);
                if existing.is_some() {
                    // Check if the variable has a type constraint
                    let tc = env.borrow().get_type_constraint(&name.value);
                    if let Some(target_type) = tc {
                        // Walrus reassignment on typed variable: strict type check
                        // Type is immutable — no implicit conversion. Use explicit
                        // typed re-declaration (x <type> := val) to change the type.
                        if val.type_name() != target_type {
                            return Rc::new(Object::Error(format!(
                                "type mismatch: '{}' is locked to {}, got {}. use explicit type declaration to change type",
                                name.value,
                                target_type,
                                val.type_name()
                            )));
                        }
                        env.borrow_mut().update(&name.value, val.clone());
                        return val;
                    }
                    env.borrow_mut().update(&name.value, val.clone());
                } else {
                    env.borrow_mut().set(name.value.clone(), val.clone());
                }
                val
            }
            Statement::Expr(expr) => self.eval_expression(expr, env),
            Statement::Each {
                variable,
                iterable,
                body,
                ..
            } => self.eval_each(variable, iterable, body, env),
            Statement::Repeat {
                condition, body, ..
            } => self.eval_repeat(condition, body, env),
            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.patterns
                    .register(name.value.clone(), param_names, condition.clone());
                Rc::new(Object::None)
            }
            Statement::Choose {
                subject, arms, ..
            } => self.eval_choose(subject, arms, env),
            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => self.eval_if(condition, consequence, alternative, env),
            Statement::TypedLet {
                name,
                type_ann,
                value,
                walrus,
            } => {
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                let target = type_ann.type_name();
                let final_val = if *walrus {
                    match Self::convert_to_type(&val, target) {
                        Ok(converted) => converted,
                        Err(msg) => return Rc::new(Object::Error(msg)),
                    }
                } else {
                    if val.type_name() != target {
                        return Rc::new(Object::Error(format!(
                            "type mismatch: expected {}, got {}",
                            target,
                            val.type_name()
                        )));
                    }
                    val
                };
                let immutable = !*walrus;
                env.borrow_mut().set_typed(
                    name.value.clone(),
                    final_val.clone(),
                    target.to_string(),
                    immutable,
                );
                final_val
            }
            Statement::TypedDeclare { name, type_ann } => {
                let val = Self::zero_value_for_type(type_ann);
                let target = type_ann.type_name();
                env.borrow_mut().set_typed(
                    name.value.clone(),
                    val.clone(),
                    target.to_string(),
                    false, // as-declarations are mutable
                );
                val
            }
            Statement::Assign { name, value } => {
                let existing = env.borrow().get(&name.value);
                if existing.is_none() {
                    return Rc::new(Object::Error(format!(
                        "identifier not found: {}. use := to declare",
                        name.value
                    )));
                }
                let tc = env.borrow().get_type_constraint(&name.value);
                if tc.is_none() {
                    return Rc::new(Object::Error(format!(
                        "= requires typed variable, use := for '{}'",
                        name.value
                    )));
                }
                // Immutable check: = cannot reassign an immutable binding
                if env.borrow().is_immutable(&name.value) {
                    return Rc::new(Object::Error(format!(
                        "cannot reassign immutable variable '{}'. use := to override",
                        name.value
                    )));
                }
                let target_type = tc.unwrap();
                let val = self.eval_expression(value, Rc::clone(&env));
                if val.is_error() {
                    return val;
                }
                if val.type_name() != target_type {
                    return Rc::new(Object::Error(format!(
                        "type mismatch: expected {}, got {}",
                        target_type,
                        val.type_name()
                    )));
                }
                env.borrow_mut().update(&name.value, val.clone());
                val
            }
            Statement::Skip => Rc::new(Object::Skip),
            Statement::Stop => Rc::new(Object::Stop),
        }
    }

    pub fn eval_expression(&mut self, expr: &Expression, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        match expr {
            Expression::Int { value, .. } => Rc::new(Object::Integer(*value)),
            Expression::Float { value, .. } => Rc::new(Object::Float(*value)),
            Expression::Char { value, .. } => Rc::new(Object::Char(*value)),
            Expression::Str { value, .. } => Rc::new(Object::String(value.clone())),
            Expression::Boolean { value, .. } => Rc::new(Object::Boolean(*value)),
            Expression::NoneExpr { .. } => Rc::new(Object::None),

            Expression::Array { elements, .. } => {
                let elems = self.eval_expressions(elements, Rc::clone(&env));
                if elems.len() == 1 && elems[0].is_error() {
                    return Rc::clone(&elems[0]);
                }
                Rc::new(Object::Array(elems))
            }

            Expression::Ident(ident) => self.eval_identifier(ident, env),

            Expression::Prefix {
                operator, right, ..
            } => {
                let right_val = self.eval_expression(right, env);
                if right_val.is_error() {
                    return right_val;
                }
                self.eval_prefix_expression(operator, right_val)
            }

            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => {
                let left_val = self.eval_expression(left, Rc::clone(&env));
                if left_val.is_error() {
                    return left_val;
                }
                let right_val = self.eval_expression(right, env);
                if right_val.is_error() {
                    return right_val;
                }
                self.eval_infix_expression(operator, left_val, right_val)
            }

            Expression::Postfix {
                operator, left, ..
            } => self.eval_postfix_expression(operator, left, env),

            Expression::Call { function, args, .. } => {
                // is_mut() needs the variable name, not its value
                if let Expression::Ident(ident) = function.as_ref() {
                    if ident.value == "is_mut" {
                        return self.eval_is_mut(args, env);
                    }
                    if ident.value == "is_type_mut" {
                        return self.eval_is_type_mut(args, env);
                    }
                }

                let func = self.eval_expression(function, Rc::clone(&env));
                if func.is_error() {
                    return func;
                }
                let arguments = self.eval_expressions(args, Rc::clone(&env));
                if arguments.len() == 1 && arguments[0].is_error() {
                    return Rc::clone(&arguments[0]);
                }
                self.apply_function(func, arguments, env)
            }

            Expression::Index { left, index, .. } => {
                let left_val = self.eval_expression(left, Rc::clone(&env));
                if left_val.is_error() {
                    return left_val;
                }
                let index_val = self.eval_expression(index, env);
                if index_val.is_error() {
                    return index_val;
                }
                self.eval_index_expression(left_val, index_val)
            }

            Expression::Grouped(inner) => self.eval_expression(inner, env),
        }
    }

    fn eval_identifier(&self, ident: &Identifier, env: Rc<RefCell<Environment>>) -> Rc<Object> {
        // First check environment
        if let Some(val) = env.borrow().get(&ident.value) {
            return val;
        }

        // Then check builtins
        if let Some(builtin) = self.builtins.get(&ident.value) {
            return Rc::clone(builtin);
        }

        Rc::new(Object::Error(format!(
            "identifier not found: {}",
            ident.value
        )))
    }

    fn eval_prefix_expression(&self, operator: &str, right: Rc<Object>) -> Rc<Object> {
        match operator {
            "!" | "not" => self.eval_bang_operator(right),
            "-" => self.eval_minus_prefix(right),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: {}{}",
                operator,
                right.type_name()
            ))),
        }
    }

    fn eval_bang_operator(&self, right: Rc<Object>) -> Rc<Object> {
        Rc::new(Object::Boolean(!right.is_truthy()))
    }

    fn eval_minus_prefix(&self, right: Rc<Object>) -> Rc<Object> {
        match right.as_ref() {
            Object::Integer(n) => Rc::new(Object::Integer(-n)),
            Object::Float(n) => Rc::new(Object::Float(-n)),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: -{}",
                right.type_name()
            ))),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: &str,
        left: Rc<Object>,
        right: Rc<Object>,
    ) -> Rc<Object> {
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix(operator, *l, *r)
            }
            (Object::Float(l), Object::Float(r)) => {
                self.eval_float_infix(operator, *l, *r)
            }
            (Object::Integer(l), Object::Float(r)) => {
                self.eval_float_infix(operator, *l as f64, *r)
            }
            (Object::Float(l), Object::Integer(r)) => {
                self.eval_float_infix(operator, *l, *r as f64)
            }
            (Object::String(l), Object::String(r)) => {
                self.eval_string_infix(operator, l, r)
            }
            (Object::Char(l), Object::Char(r)) => {
                self.eval_char_infix(operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                match operator {
                    "==" => Rc::new(Object::Boolean(l == r)),
                    "!=" => Rc::new(Object::Boolean(l != r)),
                    _ => Rc::new(Object::Error(format!(
                        "unknown operator: BOOLEAN {} BOOLEAN",
                        operator
                    ))),
                }
            }
            _ => {
                if operator == "==" {
                    Rc::new(Object::Boolean(left == right))
                } else if operator == "!=" {
                    Rc::new(Object::Boolean(left != right))
                } else {
                    Rc::new(Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left.type_name(),
                        operator,
                        right.type_name()
                    )))
                }
            }
        }
    }

    fn eval_integer_infix(&self, operator: &str, left: i64, right: i64) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::Integer(left + right)),
            "-" => Rc::new(Object::Integer(left - right)),
            "*" => Rc::new(Object::Integer(left * right)),
            "/" => {
                if right == 0 {
                    Rc::new(Object::Error("division by zero".to_string()))
                } else {
                    Rc::new(Object::Integer(left / right))
                }
            }
            "%" => {
                if right == 0 {
                    Rc::new(Object::Error("modulo by zero".to_string()))
                } else {
                    Rc::new(Object::Integer(left % right))
                }
            }
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" | ">=" if operator == "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: INTEGER {} INTEGER",
                operator
            ))),
        }
    }

    fn eval_float_infix(&self, operator: &str, left: f64, right: f64) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::Float(left + right)),
            "-" => Rc::new(Object::Float(left - right)),
            "*" => Rc::new(Object::Float(left * right)),
            "/" => Rc::new(Object::Float(left / right)),
            "%" => Rc::new(Object::Float(left % right)),
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: FLOAT {} FLOAT",
                operator
            ))),
        }
    }

    fn eval_string_infix(&self, operator: &str, left: &str, right: &str) -> Rc<Object> {
        match operator {
            "+" => Rc::new(Object::String(format!("{}{}", left, right))),
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: STRING {} STRING",
                operator
            ))),
        }
    }

    fn eval_char_infix(&self, operator: &str, left: char, right: char) -> Rc<Object> {
        match operator {
            "==" => Rc::new(Object::Boolean(left == right)),
            "!=" => Rc::new(Object::Boolean(left != right)),
            "<" => Rc::new(Object::Boolean(left < right)),
            ">" => Rc::new(Object::Boolean(left > right)),
            "<=" => Rc::new(Object::Boolean(left <= right)),
            ">=" => Rc::new(Object::Boolean(left >= right)),
            _ => Rc::new(Object::Error(format!(
                "unknown operator: CHAR {} CHAR",
                operator
            ))),
        }
    }

    fn eval_postfix_expression(
        &mut self,
        operator: &str,
        left: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        // Get the identifier name for postfix operations
        let ident_name = match left {
            Expression::Ident(ident) => ident.value.clone(),
            _ => {
                return Rc::new(Object::Error(
                    "postfix operator requires identifier".to_string(),
                ))
            }
        };

        // Get current value
        let current = env.borrow().get(&ident_name);
        let current = match current {
            Some(val) => val,
            None => {
                return Rc::new(Object::Error(format!(
                    "identifier not found: {}",
                    ident_name
                )))
            }
        };

        // Immutable check: ++/-- cannot mutate immutable variables
        if env.borrow().is_immutable(&ident_name) {
            return Rc::new(Object::Error(format!(
                "cannot mutate immutable variable '{}'. use := to override",
                ident_name
            )));
        }

        // Calculate new value and return original
        let (new_val, return_val) = match (operator, current.as_ref()) {
            ("++", Object::Integer(n)) => (
                Rc::new(Object::Integer(n + 1)),
                Rc::new(Object::Integer(*n)),
            ),
            ("--", Object::Integer(n)) => (
                Rc::new(Object::Integer(n - 1)),
                Rc::new(Object::Integer(*n)),
            ),
            _ => {
                return Rc::new(Object::Error(format!(
                    "unknown postfix operator: {}{}",
                    current.type_name(),
                    operator
                )))
            }
        };

        // Update the variable in environment
        env.borrow_mut().update(&ident_name, new_val);

        return_val
    }

    fn eval_index_expression(&self, left: Rc<Object>, index: Rc<Object>) -> Rc<Object> {
        match (left.as_ref(), index.as_ref()) {
            (Object::Array(arr), Object::Integer(idx)) => {
                let idx = *idx as usize;
                if idx >= arr.len() {
                    Rc::new(Object::None)
                } else {
                    Rc::clone(&arr[idx])
                }
            }
            (Object::String(s), Object::Integer(idx)) => {
                let idx = *idx as usize;
                if idx >= s.len() {
                    Rc::new(Object::None)
                } else {
                    Rc::new(Object::String(s.chars().nth(idx).unwrap().to_string()))
                }
            }
            _ => Rc::new(Object::Error(format!(
                "index operator not supported: {}[{}]",
                left.type_name(),
                index.type_name()
            ))),
        }
    }

    fn eval_expressions(
        &mut self,
        exprs: &[Expression],
        env: Rc<RefCell<Environment>>,
    ) -> Vec<Rc<Object>> {
        let mut results = Vec::new();

        for expr in exprs {
            let evaluated = self.eval_expression(expr, Rc::clone(&env));
            if evaluated.is_error() {
                return vec![evaluated];
            }
            results.push(evaluated);
        }

        results
    }

    fn apply_function(
        &mut self,
        func: Rc<Object>,
        args: Vec<Rc<Object>>,
        _env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        match func.as_ref() {
            Object::Builtin(f) => f(args),
            Object::Function {
                parameters,
                body,
                env,
            } => {
                if parameters.len() != args.len() {
                    return Rc::new(Object::Error(format!(
                        "wrong number of arguments: expected {}, got {}",
                        parameters.len(),
                        args.len()
                    )));
                }

                let extended_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
                for (param, arg) in parameters.iter().zip(args.iter()) {
                    extended_env.borrow_mut().set(param.value.clone(), Rc::clone(arg));
                }

                let result = self.eval_block(body, extended_env);
                match result.as_ref() {
                    Object::Return(val) => Rc::clone(val),
                    _ => result,
                }
            }
            _ => Rc::new(Object::Error(format!(
                "not a function: {}",
                func.type_name()
            ))),
        }
    }

    fn eval_block(&mut self, statements: &[Statement], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        let mut result = Rc::new(Object::None);

        for stmt in statements {
            result = self.eval_statement(stmt, Rc::clone(&env));

            match result.as_ref() {
                Object::Return(_) | Object::Error(_) | Object::Skip | Object::Stop => return result,
                _ => {}
            }
        }

        result
    }

    // --- Control flow ---

    fn eval_each(
        &mut self,
        variable: &Identifier,
        iterable: &Expression,
        body: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        let iterable_val = self.eval_expression(iterable, Rc::clone(&env));
        if iterable_val.is_error() {
            return iterable_val;
        }

        let elements = match iterable_val.as_ref() {
            Object::Array(arr) => arr.clone(),
            Object::String(s) => s
                .chars()
                .map(|c| Rc::new(Object::String(c.to_string())))
                .collect(),
            _ => {
                return Rc::new(Object::Error(format!(
                    "cannot iterate over {}",
                    iterable_val.type_name()
                )))
            }
        };

        for element in elements {
            let loop_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env))));
            loop_env
                .borrow_mut()
                .set(variable.value.clone(), Rc::clone(&element));

            let result = self.eval_block(body, loop_env);

            match result.as_ref() {
                Object::Skip => continue,
                Object::Stop => break,
                Object::Error(_) => return result,
                _ => {}
            }
        }

        Rc::new(Object::None)
    }

    fn eval_repeat(
        &mut self,
        condition: &Expression,
        body: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        loop {
            let cond_val = self.eval_expression(condition, Rc::clone(&env));
            if cond_val.is_error() {
                return cond_val;
            }

            if !cond_val.is_truthy() {
                break;
            }

            let result = self.eval_block(body, Rc::clone(&env));

            match result.as_ref() {
                Object::Skip => continue,
                Object::Stop => break,
                Object::Error(_) => return result,
                _ => {}
            }
        }

        Rc::new(Object::None)
    }

    fn eval_choose(
        &mut self,
        subject: &Expression,
        arms: &[crate::ast::ChooseArm],
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        let subject_val = self.eval_expression(subject, Rc::clone(&env));
        if subject_val.is_error() {
            return subject_val;
        }

        for arm in arms {
            // Handle 'else' arm
            if arm.pattern_name == "else" {
                return self.eval_expression(&arm.body, Rc::clone(&env));
            }

            // Look up pattern
            let pattern = match self.patterns.get(&arm.pattern_name) {
                Some(p) => p.clone(),
                None => {
                    return Rc::new(Object::Error(format!(
                        "unknown pattern: {}",
                        arm.pattern_name
                    )))
                }
            };

            // Create environment with pattern parameter bound to subject
            let pattern_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env))));
            if !pattern.parameters.is_empty() {
                pattern_env
                    .borrow_mut()
                    .set(pattern.parameters[0].clone(), Rc::clone(&subject_val));
            }

            // Evaluate pattern condition
            let matches = self.eval_expression(&pattern.condition, Rc::clone(&pattern_env));
            if matches.is_error() {
                return matches;
            }

            if matches.is_truthy() {
                return self.eval_expression(&arm.body, Rc::clone(&env));
            }
        }

        Rc::new(Object::None)
    }

    fn eval_is_type_mut(&self, args: &[Expression], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        if args.len() != 1 {
            return Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }
        match &args[0] {
            Expression::Ident(ident) => {
                if env.borrow().get(&ident.value).is_none() {
                    return Rc::new(Object::Error(format!(
                        "identifier not found: {}",
                        ident.value
                    )));
                }
                // A variable's type is mutable only if it has no type constraint
                Rc::new(Object::Boolean(env.borrow().get_type_constraint(&ident.value).is_none()))
            }
            _ => Rc::new(Object::Error(
                "argument to `is_type_mut` must be a variable name".to_string(),
            )),
        }
    }

    fn eval_is_mut(&self, args: &[Expression], env: Rc<RefCell<Environment>>) -> Rc<Object> {
        if args.len() != 1 {
            return Rc::new(Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }
        match &args[0] {
            Expression::Ident(ident) => {
                if env.borrow().get(&ident.value).is_none() {
                    return Rc::new(Object::Error(format!(
                        "identifier not found: {}",
                        ident.value
                    )));
                }
                Rc::new(Object::Boolean(!env.borrow().is_immutable(&ident.value)))
            }
            _ => Rc::new(Object::Error(
                "argument to `is_mut` must be a variable name".to_string(),
            )),
        }
    }

    fn eval_if(
        &mut self,
        condition: &Expression,
        consequence: &[Statement],
        alternative: &Option<Vec<Statement>>,
        env: Rc<RefCell<Environment>>,
    ) -> Rc<Object> {
        let cond_val = self.eval_expression(condition, Rc::clone(&env));
        if cond_val.is_error() {
            return cond_val;
        }

        if cond_val.is_truthy() {
            self.eval_block(consequence, env)
        } else if let Some(alt) = alternative {
            self.eval_block(alt, env)
        } else {
            Rc::new(Object::None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Rc<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.error().is_empty() {
            panic!("Parser errors: {:?}", parser.error());
        }

        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::new();
        evaluator.eval_program(&program, env)
    }

    fn test_integer(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(val) => assert_eq!(*val, expected),
            _ => panic!("Expected Integer({}), got {:?}", expected, obj),
        }
    }

    fn test_float(obj: &Object, expected: f64) {
        match obj {
            Object::Float(val) => assert!((val - expected).abs() < f64::EPSILON),
            _ => panic!("Expected Float({}), got {:?}", expected, obj),
        }
    }

    fn test_boolean(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(val) => assert_eq!(*val, expected),
            _ => panic!("Expected Boolean({}), got {:?}", expected, obj),
        }
    }

    fn test_string(obj: &Object, expected: &str) {
        match obj {
            Object::String(val) => assert_eq!(val, expected),
            _ => panic!("Expected String({}), got {:?}", expected, obj),
        }
    }

    fn test_char(obj: &Object, expected: char) {
        match obj {
            Object::Char(val) => assert_eq!(*val, expected),
            _ => panic!("Expected Char({}), got {:?}", expected, obj),
        }
    }

    fn test_none(obj: &Object) {
        match obj {
            Object::None => {}
            _ => panic!("Expected None, got {:?}", obj),
        }
    }

    // ==================== INTEGER TESTS ====================

    #[test]
    fn test_integer_expressions() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5", 10),
            ("5 - 5", 0),
            ("5 * 5", 25),
            ("10 / 2", 5),
            ("10 % 3", 1),
            ("2 + 3 * 4", 14),
            ("(2 + 3) * 4", 20),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("50 / 2 * 2 + 10", 60),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_integer(&result, expected);
        }
    }

    // ==================== FLOAT TESTS ====================

    #[test]
    fn test_float_expressions() {
        let tests = vec![
            ("3.14", 3.14),
            ("2.5 + 1.5", 4.0),
            ("10.0 / 4.0", 2.5),
            ("3.14 * 2", 6.28),
            ("2 * 3.14", 6.28),
            ("-3.14", -3.14),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_float(&result, expected);
        }
    }

    // ==================== CHAR TESTS ====================

    #[test]
    fn test_char_expressions() {
        let tests = vec![
            ("`a`", 'a'),
            ("`Z`", 'Z'),
            ("`0`", '0'),
            ("`!`", '!'),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_char(&result, expected);
        }
    }

    #[test]
    fn test_char_comparisons() {
        let tests = vec![
            ("`a` == `a`", true),
            ("`a` == `b`", false),
            ("`a` != `b`", true),
            ("`a` < `b`", true),
            ("`b` > `a`", true),
            ("`a` <= `a`", true),
            ("`a` >= `a`", true),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_boolean(&result, expected);
        }
    }

    // ==================== STRING TESTS ====================

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            ("\"hello\"", "hello"),
            ("'world'", "world"),
            ("\"hello\" + \" \" + \"world\"", "hello world"),
            ("'single' + \" double\"", "single double"),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_string(&result, expected);
        }
    }

    #[test]
    fn test_string_comparisons() {
        let tests = vec![
            ("\"hello\" == \"hello\"", true),
            ("\"hello\" == \"world\"", false),
            ("\"hello\" != \"world\"", true),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_boolean(&result, expected);
        }
    }

    // ==================== BOOLEAN TESTS ====================

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            ("True", true),
            ("False", false),
            ("!True", false),
            ("!False", true),
            ("!!True", true),
            ("True == True", true),
            ("True == False", false),
            ("True != False", true),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_boolean(&result, expected);
        }
    }

    // ==================== COMPARISON TESTS ====================

    #[test]
    fn test_integer_comparisons() {
        let tests = vec![
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1 <= 2", true),
            ("2 >= 1", true),
            ("1 <= 1", true),
            ("1 >= 1", true),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_boolean(&result, expected);
        }
    }

    // ==================== ARRAY TESTS ====================

    #[test]
    fn test_array_literals() {
        let result = test_eval("[1, 2, 3]");
        match result.as_ref() {
            Object::Array(elements) => {
                assert_eq!(elements.len(), 3);
                test_integer(&elements[0], 1);
                test_integer(&elements[1], 2);
                test_integer(&elements[2], 3);
            }
            _ => panic!("Expected Array, got {:?}", result),
        }
    }

    #[test]
    fn test_array_index() {
        let tests = vec![
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("x := [5, 10, 15]\nx[1]", 10),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_integer(&result, expected);
        }
    }

    #[test]
    fn test_array_out_of_bounds() {
        let result = test_eval("[1, 2, 3][10]");
        test_none(&result);
    }

    // ==================== LET STATEMENT TESTS ====================

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("x := 5\nx", 5),
            ("x := 5 * 5\nx", 25),
            ("x := 5\ny := x\ny", 5),
            ("x := 5\ny := x\nz := x + y\nz", 10),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_integer(&result, expected);
        }
    }

    // ==================== POSTFIX TESTS ====================

    #[test]
    fn test_postfix_increment() {
        let result = test_eval("x := 5\nx++\nx");
        test_integer(&result, 6);
    }

    #[test]
    fn test_postfix_decrement() {
        let result = test_eval("x := 5\nx--\nx");
        test_integer(&result, 4);
    }

    #[test]
    fn test_postfix_returns_original() {
        // x++ should return original value
        let result = test_eval("x := 5\nx++");
        test_integer(&result, 5);
    }

    // ==================== IF STATEMENT TESTS ====================

    #[test]
    fn test_if_statements() {
        let tests = vec![
            ("if True { 10 }", Some(10)),
            ("if False { 10 }", None),
            ("if 1 < 2 { 10 }", Some(10)),
            ("if 1 > 2 { 10 }", None),
            ("if 1 > 2 { 10 } else { 20 }", Some(20)),
            ("if 1 < 2 { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            match expected {
                Some(val) => test_integer(&result, val),
                None => test_none(&result),
            }
        }
    }

    // ==================== EACH LOOP TESTS ====================

    #[test]
    fn test_each_loop_array() {
        // Sum elements using each loop
        let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                sum := sum + x
            }
            sum
        "#;
        let result = test_eval(input);
        test_integer(&result, 15);
    }

    #[test]
    fn test_each_loop_string() {
        // Count characters
        let input = r#"
            count := 0
            each c in chars("hello") {
                count := count + 1
            }
            count
        "#;
        let result = test_eval(input);
        test_integer(&result, 5);
    }

    #[test]
    fn test_each_with_stop() {
        let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                if x == 3 {
                    stop
                }
                sum := sum + x
            }
            sum
        "#;
        let result = test_eval(input);
        test_integer(&result, 3); // 1 + 2
    }

    #[test]
    fn test_each_with_skip() {
        let input = r#"
            sum := 0
            each x in [1, 2, 3, 4, 5] {
                if x == 3 {
                    skip
                }
                sum := sum + x
            }
            sum
        "#;
        let result = test_eval(input);
        test_integer(&result, 12); // 1 + 2 + 4 + 5
    }

    // ==================== REPEAT LOOP TESTS ====================

    #[test]
    fn test_repeat_loop() {
        let input = r#"
            x := 0
            repeat when x < 5 {
                x := x + 1
            }
            x
        "#;
        let result = test_eval(input);
        test_integer(&result, 5);
    }

    #[test]
    fn test_repeat_with_decrement() {
        let input = r#"
            x := 10
            count := 0
            repeat when x > 0 {
                x--
                count := count + 1
            }
            count
        "#;
        let result = test_eval(input);
        test_integer(&result, 10);
    }

    #[test]
    fn test_repeat_with_stop() {
        let input = r#"
            x := 0
            repeat when True {
                x := x + 1
                if x == 5 {
                    stop
                }
            }
            x
        "#;
        let result = test_eval(input);
        test_integer(&result, 5);
    }

    // ==================== PATTERN/CHOOSE TESTS ====================

    #[test]
    fn test_pattern_and_choose() {
        let input = r#"
            pattern is_ten(x) when x == 10
            pattern is_twenty(x) when x == 20

            y := 10
            choose y {
                is_ten -> 1,
                is_twenty -> 2,
                else -> 3
            }
        "#;
        let result = test_eval(input);
        test_integer(&result, 1);
    }

    #[test]
    fn test_choose_else() {
        let input = r#"
            pattern is_ten(x) when x == 10

            y := 5
            choose y {
                is_ten -> 1,
                else -> 99
            }
        "#;
        let result = test_eval(input);
        test_integer(&result, 99);
    }

    // ==================== BUILTIN FUNCTION TESTS ====================

    #[test]
    fn test_len_array() {
        let tests = vec![
            ("len([])", 0),
            ("len([1])", 1),
            ("len([1, 2, 3])", 3),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_integer(&result, expected);
        }
    }

    #[test]
    fn test_len_string() {
        let tests = vec![
            ("len(\"\")", 0),
            ("len(\"hello\")", 5),
            ("len('world')", 5),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);
            test_integer(&result, expected);
        }
    }

    #[test]
    fn test_first_last_rest() {
        test_integer(&test_eval("first([1, 2, 3])"), 1);
        test_integer(&test_eval("last([1, 2, 3])"), 3);

        let rest_result = test_eval("rest([1, 2, 3])");
        match rest_result.as_ref() {
            Object::Array(elements) => {
                assert_eq!(elements.len(), 2);
                test_integer(&elements[0], 2);
                test_integer(&elements[1], 3);
            }
            _ => panic!("Expected Array, got {:?}", rest_result),
        }
    }

    #[test]
    fn test_push() {
        let result = test_eval("push([1, 2], 3)");
        match result.as_ref() {
            Object::Array(elements) => {
                assert_eq!(elements.len(), 3);
                test_integer(&elements[0], 1);
                test_integer(&elements[1], 2);
                test_integer(&elements[2], 3);
            }
            _ => panic!("Expected Array, got {:?}", result),
        }
    }

    #[test]
    fn test_ord_chr() {
        test_integer(&test_eval("ord(`a`)"), 97);
        test_integer(&test_eval("ord(`A`)"), 65);
        test_integer(&test_eval("ord(`0`)"), 48);

        test_char(&test_eval("chr(97)"), 'a');
        test_char(&test_eval("chr(65)"), 'A');
        test_char(&test_eval("chr(48)"), '0');
    }

    #[test]
    fn test_str_builtin() {
        test_string(&test_eval("str(`a`)"), "a");
        test_string(&test_eval("str(123)"), "123");
        test_string(&test_eval("str(3.14)"), "3.14");
        test_string(&test_eval("str(True)"), "True");
    }

    #[test]
    fn test_chars_builtin() {
        let result = test_eval("chars(\"abc\")");
        match result.as_ref() {
            Object::Array(elements) => {
                assert_eq!(elements.len(), 3);
                test_char(&elements[0], 'a');
                test_char(&elements[1], 'b');
                test_char(&elements[2], 'c');
            }
            _ => panic!("Expected Array, got {:?}", result),
        }
    }

    #[test]
    fn test_type_builtin() {
        test_string(&test_eval("type(5)"), "INTEGER");
        test_string(&test_eval("type(3.14)"), "FLOAT");
        test_string(&test_eval("type(`a`)"), "CHAR");
        test_string(&test_eval("type(\"hello\")"), "STRING");
        test_string(&test_eval("type(True)"), "BOOLEAN");
        test_string(&test_eval("type([1, 2])"), "ARRAY");
        test_string(&test_eval("type(None)"), "NONE");
    }

    // ==================== ERROR HANDLING TESTS ====================

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + True", "type mismatch"),
            ("\"hello\" - \"world\"", "unknown operator"),
            ("foobar", "identifier not found"),
            ("-True", "unknown operator"),
            ("len(1)", "argument to `len` not supported"),
            ("len(\"one\", \"two\")", "wrong number of arguments"),
            ("first(1)", "argument to `first` must be ARRAY"),
        ];

        for (input, expected_msg) in tests {
            let result = test_eval(input);
            match result.as_ref() {
                Object::Error(msg) => {
                    assert!(
                        msg.contains(expected_msg),
                        "Expected error containing '{}', got '{}'",
                        expected_msg,
                        msg
                    );
                }
                _ => panic!("Expected error for '{}', got {:?}", input, result),
            }
        }
    }

    // ==================== NONE TESTS ====================

    #[test]
    fn test_none_literal() {
        test_none(&test_eval("None"));
    }

    #[test]
    fn test_none_is_falsy() {
        let result = test_eval("if None { 1 } else { 2 }");
        test_integer(&result, 2);
    }

    // ==================== TRUTHINESS TESTS ====================

    #[test]
    fn test_truthiness() {
        // Truthy values
        let truthy = vec!["1", "-1", "3.14", "`a`", "\"hello\"", "[1]", "True"];
        for input in truthy {
            let test_input = format!("if {} {{ 1 }} else {{ 0 }}", input);
            let result = test_eval(&test_input);
            test_integer(&result, 1);
        }

        // Falsy values
        let falsy = vec!["0", "0.0", "\"\"", "[]", "False", "None"];
        for input in falsy {
            let test_input = format!("if {} {{ 1 }} else {{ 0 }}", input);
            let result = test_eval(&test_input);
            test_integer(&result, 0);
        }
    }

    // ==================== COMPLEX INTEGRATION TESTS ====================

    #[test]
    fn test_fibonacci() {
        let input = r#"
            a := 0
            b := 1
            count := 0
            repeat when count < 10 {
                temp := a
                a := b
                b := temp + b
                count := count + 1
            }
            a
        "#;
        let result = test_eval(input);
        test_integer(&result, 55); // 10th Fibonacci number
    }

    #[test]
    fn test_factorial_iterative() {
        let input = r#"
            n := 5
            result := 1
            repeat when n > 0 {
                result := result * n
                n--
            }
            result
        "#;
        let result = test_eval(input);
        test_integer(&result, 120); // 5!
    }

    #[test]
    fn test_nested_loops() {
        let input = r#"
            sum := 0
            each i in [1, 2, 3] {
                each j in [10, 20] {
                    sum := sum + i * j
                }
            }
            sum
        "#;
        // 1*10 + 1*20 + 2*10 + 2*20 + 3*10 + 3*20 = 10+20+20+40+30+60 = 180
        let result = test_eval(input);
        test_integer(&result, 180);
    }

    // ==================== TYPED VARIABLE TESTS ====================

    // --- Typed declaration with = (immutable value + immutable type) ---

    #[test]
    fn test_typed_strict_declaration() {
        let result = test_eval("x <int> = 10\nx");
        test_integer(&result, 10);
    }

    #[test]
    fn test_typed_strict_rejects_wrong_type() {
        let result = test_eval("x <int> = \"hello\"");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    // --- Typed declaration with := (mutable value + immutable type, conversion on init) ---

    #[test]
    fn test_typed_walrus_declaration_converts_float_to_int() {
        let result = test_eval("x <int> := 3.9\nx");
        test_integer(&result, 3); // conversion at declaration
    }

    #[test]
    fn test_typed_walrus_declaration_converts_int_to_str() {
        let result = test_eval("x <str> := 42\nx");
        test_string(&result, "42");
    }

    #[test]
    fn test_typed_walrus_declaration_converts_str_to_int() {
        let result = test_eval("x <int> := \"123\"\nx");
        test_integer(&result, 123);
    }

    #[test]
    fn test_typed_walrus_declaration_converts_bool_to_int() {
        let result = test_eval("x <int> := True\nx");
        test_integer(&result, 1);
    }

    #[test]
    fn test_typed_walrus_declaration_converts_int_to_float() {
        let result = test_eval("x <float> := 5\nx");
        test_float(&result, 5.0);
    }

    #[test]
    fn test_typed_walrus_declaration_converts_int_to_bool() {
        let result = test_eval("x <bool> := 42\nx");
        test_boolean(&result, true);

        let result2 = test_eval("x <bool> := 0\nx");
        test_boolean(&result2, false);
    }

    #[test]
    fn test_typed_walrus_declaration_impossible_conversion() {
        let result = test_eval("x <array> := 5");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("cannot convert"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    // --- as-declare (mutable value + immutable type, zero value) ---

    #[test]
    fn test_typed_declare_zero_values() {
        test_integer(&test_eval("x as <int>\nx"), 0);
        test_float(&test_eval("x as <float>\nx"), 0.0);
        test_string(&test_eval("x as <str>\nx"), "");
        test_char(&test_eval("x as <char>\nx"), '\0');
        test_boolean(&test_eval("x as <bool>\nx"), false);

        let arr = test_eval("x as <array>\nx");
        match arr.as_ref() {
            Object::Array(elements) => assert!(elements.is_empty()),
            _ => panic!("Expected empty array, got {:?}", arr),
        }
    }

    #[test]
    fn test_as_declare_is_mutable() {
        let result = test_eval("x as <int>\nx = 5\nx");
        test_integer(&result, 5);
    }

    #[test]
    fn test_as_declare_postfix_works() {
        let result = test_eval("x as <int>\nx = 5\nx++\nx");
        test_integer(&result, 6);
    }

    // --- Immutable value: = blocked, ++/-- blocked ---

    #[test]
    fn test_immutable_assign_blocked() {
        let result = test_eval("x <int> = 10\nx = 20");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("cannot reassign immutable"), "got: {}", msg),
            _ => panic!("Expected immutability error, got {:?}", result),
        }
    }

    #[test]
    fn test_immutable_postfix_increment_blocked() {
        let result = test_eval("x <int> = 5\nx++");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("cannot mutate immutable"), "got: {}", msg),
            _ => panic!("Expected immutability error, got {:?}", result),
        }
    }

    #[test]
    fn test_immutable_postfix_decrement_blocked() {
        let result = test_eval("x <int> = 5\nx--");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("cannot mutate immutable"), "got: {}", msg),
            _ => panic!("Expected immutability error, got {:?}", result),
        }
    }

    // --- Walrus := overrides value immutability, but type stays locked ---

    #[test]
    fn test_walrus_overrides_immutable_value_same_type() {
        // := can change the value if the type matches
        let result = test_eval("x <int> = 10\nx := 20\nx");
        test_integer(&result, 20);
    }

    #[test]
    fn test_walrus_on_immutable_rejects_wrong_type() {
        // := overrides value immutability but type is still locked
        let result = test_eval("x <int> = 10\nx := \"hello\"");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("type mismatch") && msg.contains("locked"), "got: {}", msg),
            _ => panic!("Expected type locked error, got {:?}", result),
        }
    }

    #[test]
    fn test_walrus_on_immutable_rejects_float_no_conversion() {
        // No implicit conversion on walrus reassignment — type is locked
        let result = test_eval("x <int> = 10\nx := 3.7");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("type mismatch") && msg.contains("locked"), "got: {}", msg),
            _ => panic!("Expected type locked error, got {:?}", result),
        }
    }

    // --- Explicit re-declaration changes the type ---

    #[test]
    fn test_explicit_redeclare_changes_type() {
        // x <str> := "20" re-declares x with new type (conversion at declaration)
        let result = test_eval("x <int> = 10\nx <str> := \"20\"\nx");
        test_string(&result, "20");
    }

    #[test]
    fn test_explicit_redeclare_immutable_to_mutable() {
        // Re-declare as mutable, then = works
        let result = test_eval("x <int> = 10\nx <int> := 99\nx = 42\nx");
        test_integer(&result, 42);
    }

    // --- Mutable typed (:= declares mutable value) ---

    #[test]
    fn test_mutable_typed_assign_succeeds() {
        let result = test_eval("x <int> := 10\nx = 20\nx");
        test_integer(&result, 20);
    }

    #[test]
    fn test_mutable_typed_assign_rejects_wrong_type() {
        let result = test_eval("x <int> := 10\nx = \"hello\"");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("type mismatch"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    #[test]
    fn test_mutable_typed_postfix_increment() {
        let result = test_eval("x <int> := 5\nx++\nx");
        test_integer(&result, 6);
    }

    #[test]
    fn test_mutable_typed_postfix_decrement() {
        let result = test_eval("x <int> := 5\nx--\nx");
        test_integer(&result, 4);
    }

    #[test]
    fn test_mutable_typed_walrus_reassign_same_type() {
        // := reassignment on mutable typed: strict type check, no conversion
        let result = test_eval("x <int> := 10\nx := 42\nx");
        test_integer(&result, 42);
    }

    #[test]
    fn test_mutable_typed_walrus_reassign_wrong_type() {
        // Type is locked even on mutable typed variables
        let result = test_eval("x <int> := 10\nx := \"hello\"");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("type mismatch") && msg.contains("locked"), "got: {}", msg),
            _ => panic!("Expected type locked error, got {:?}", result),
        }
    }

    // --- Untyped / undeclared errors ---

    #[test]
    fn test_assign_untyped_variable_errors() {
        let result = test_eval("x := 10\nx = 20");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("= requires typed variable"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    #[test]
    fn test_assign_undeclared_variable_errors() {
        let result = test_eval("x = 20");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    // --- Shadowing in inner scope ---

    #[test]
    fn test_typed_shadowing_inner_scope() {
        let input = r#"
            x <int> = 10
            each i in [1] {
                x <str> = "hello"
            }
            x
        "#;
        let result = test_eval(input);
        test_integer(&result, 10);
    }

    #[test]
    fn test_immutable_shadowed_in_inner_scope_ok() {
        let input = r#"
            x <int> = 10
            each i in [1] {
                x <int> := 99
                x = 42
            }
            x
        "#;
        let result = test_eval(input);
        test_integer(&result, 10);
    }

    // --- Self-conversion: re-declare a variable with typed walrus to convert in place ---

    #[test]
    fn test_self_conversion_str_to_int() {
        // Simulates: user_input comes in as string, re-declare as int
        let input = r#"
            user_input := "42"
            user_input <int> := user_input
            user_input
        "#;
        let result = test_eval(input);
        test_integer(&result, 42);
    }

    #[test]
    fn test_self_conversion_str_to_float() {
        let input = r#"
            val := "3.14"
            val <float> := val
            val
        "#;
        let result = test_eval(input);
        test_float(&result, 3.14);
    }

    #[test]
    fn test_self_conversion_float_to_int() {
        let input = r#"
            x := 3.9
            x <int> := x
            x
        "#;
        let result = test_eval(input);
        test_integer(&result, 3);
    }

    #[test]
    fn test_self_conversion_int_to_str() {
        let input = r#"
            num := 100
            num <str> := num
            num
        "#;
        let result = test_eval(input);
        test_string(&result, "100");
    }

    #[test]
    fn test_self_conversion_bad_string_to_int_errors() {
        let input = r#"
            user_input := "hello"
            user_input <int> := user_input
        "#;
        let result = test_eval(input);
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("cannot convert"), "got: {}", msg),
            _ => panic!("Expected conversion error, got {:?}", result),
        }
    }

    #[test]
    fn test_self_conversion_locks_type() {
        // After re-declaration, type is locked
        let input = r#"
            x := "42"
            x <int> := x
            x := "hello"
        "#;
        let result = test_eval(input);
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("locked"), "got: {}", msg),
            _ => panic!("Expected type locked error, got {:?}", result),
        }
    }

    // ==================== IS_MUT TESTS ====================

    #[test]
    fn test_is_mut_untyped_variable() {
        let result = test_eval("x := 10\nis_mut(x)");
        test_boolean(&result, true);
    }

    #[test]
    fn test_is_mut_immutable_typed() {
        let result = test_eval("x <int> = 10\nis_mut(x)");
        test_boolean(&result, false);
    }

    #[test]
    fn test_is_mut_mutable_typed() {
        let result = test_eval("x <int> := 10\nis_mut(x)");
        test_boolean(&result, true);
    }

    #[test]
    fn test_is_mut_as_declare() {
        let result = test_eval("x as <int>\nis_mut(x)");
        test_boolean(&result, true);
    }

    #[test]
    fn test_is_mut_undeclared_errors() {
        let result = test_eval("is_mut(x)");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    #[test]
    fn test_is_mut_wrong_arg_count() {
        let result = test_eval("x := 1\ny := 2\nis_mut(x, y)");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("wrong number of arguments"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }

    // ==================== IS_TYPE_MUT TESTS ====================

    #[test]
    fn test_is_type_mut_untyped_variable() {
        let result = test_eval("x := 10\nis_type_mut(x)");
        test_boolean(&result, true);
    }

    #[test]
    fn test_is_type_mut_strict_typed() {
        let result = test_eval("x <int> = 10\nis_type_mut(x)");
        test_boolean(&result, false);
    }

    #[test]
    fn test_is_type_mut_walrus_typed() {
        let result = test_eval("x <int> := 10\nis_type_mut(x)");
        test_boolean(&result, false);
    }

    #[test]
    fn test_is_type_mut_as_declare() {
        let result = test_eval("x as <int>\nis_type_mut(x)");
        test_boolean(&result, false);
    }

    #[test]
    fn test_is_type_mut_undeclared_errors() {
        let result = test_eval("is_type_mut(x)");
        match result.as_ref() {
            Object::Error(msg) => assert!(msg.contains("identifier not found"), "got: {}", msg),
            _ => panic!("Expected error, got {:?}", result),
        }
    }
}
