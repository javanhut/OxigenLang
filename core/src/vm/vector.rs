//! `vec2`, `vec3` and `vec4`: small vectors of `f32` components for game
//! maths.
//!
//! A vector is one `Value::Vector(Rc<Vector>)`. It is an `Rc` rather than
//! inline because the JIT treats every value tag past `None` (except
//! `Builtin`) as an `Rc` it may refcount in place; an inline vector would
//! need those checks taught a new exception first. Vectors are values:
//! operators build new vectors, and components are read, never assigned.

use std::fmt;
use std::rc::Rc;

use crate::vm::value::{Value, rc_str};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Vector {
    len: u8,
    components: [f32; 4],
}

impl Vector {
    /// A vector of 2 to 4 components. Callers pass a slice of that length.
    pub fn new(components: &[f32]) -> Vector {
        debug_assert!((2..=4).contains(&components.len()));
        let mut all = [0.0; 4];
        all[..components.len()].copy_from_slice(components);
        Vector {
            len: components.len() as u8,
            components: all,
        }
    }

    pub fn components(&self) -> &[f32] {
        &self.components[..self.len as usize]
    }

    pub fn type_name(&self) -> &'static str {
        match self.len {
            2 => "VEC2",
            3 => "VEC3",
            _ => "VEC4",
        }
    }

    /// The constructor's name, which is also how the vector displays.
    pub fn constructor(&self) -> &'static str {
        match self.len {
            2 => "vec2",
            3 => "vec3",
            _ => "vec4",
        }
    }

    /// Component `name` (`x`, `y`, `z` or `w`), if this vector has it.
    pub fn component(&self, name: &str) -> Option<f32> {
        let index = match name {
            "x" => 0,
            "y" => 1,
            "z" => 2,
            "w" => 3,
            _ => return None,
        };
        self.components().get(index).copied()
    }

    pub fn map(&self, f: impl Fn(f32) -> f32) -> Vector {
        let mut out = *self;
        for c in &mut out.components[..self.len as usize] {
            *c = f(*c);
        }
        out
    }

    /// Combine with a vector of the same size, component by component.
    pub fn zip(&self, other: &Vector, f: impl Fn(f32, f32) -> f32) -> Option<Vector> {
        if self.len != other.len {
            return None;
        }
        let mut out = *self;
        for (c, o) in out.components[..self.len as usize]
            .iter_mut()
            .zip(other.components())
        {
            *c = f(*c, *o);
        }
        Some(out)
    }

    pub fn dot(&self, other: &Vector) -> Option<f64> {
        (self.len == other.len).then(|| {
            self.components()
                .iter()
                .zip(other.components())
                .map(|(a, b)| f64::from(*a) * f64::from(*b))
                .sum()
        })
    }

    pub fn length(&self) -> f64 {
        self.dot(self).unwrap_or(0.0).sqrt()
    }

    pub fn into_value(self) -> Value {
        Value::Vector(Rc::new(self))
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // f32's own Display, so `vec2(0.1, 0)` reads back as written.
        let parts: Vec<String> = self.components().iter().map(|c| c.to_string()).collect();
        write!(f, "{}({})", self.constructor(), parts.join(", "))
    }
}

/// A number as a component, or `None` for anything that is not a number.
pub fn scalar(value: &Value) -> Option<f32> {
    match value {
        Value::Integer(n) => Some(*n as f32),
        Value::Float(x) => Some(*x as f32),
        Value::Byte(b) => Some(f32::from(*b)),
        Value::Uint(n) => Some(*n as f32),
        _ => None,
    }
}

fn error(message: String) -> Value {
    Value::Error(rc_str(message))
}

fn construct(name: &str, size: usize, args: &[Value]) -> Value {
    let components: Option<Vec<f32>> = args.iter().map(scalar).collect();
    match components {
        Some(c) if c.len() == size => Vector::new(&c).into_value(),
        _ => error(format!("{name} takes {size} numbers")),
    }
}

pub(crate) fn builtin_vec2(args: &[Value]) -> Value {
    construct("vec2", 2, args)
}

pub(crate) fn builtin_vec3(args: &[Value]) -> Value {
    construct("vec3", 3, args)
}

pub(crate) fn builtin_vec4(args: &[Value]) -> Value {
    construct("vec4", 4, args)
}

fn two_vectors<'a>(name: &str, args: &'a [Value]) -> Result<(&'a Vector, &'a Vector), Value> {
    match args {
        [Value::Vector(a), Value::Vector(b)] if a.len == b.len => Ok((a, b)),
        _ => Err(error(format!("{name} needs two vectors of the same size"))),
    }
}

fn one_vector<'a>(name: &str, args: &'a [Value]) -> Result<&'a Vector, Value> {
    match args {
        [Value::Vector(v)] => Ok(v),
        _ => Err(error(format!("{name} needs one vector"))),
    }
}

pub(crate) fn builtin_vec_dot(args: &[Value]) -> Value {
    match two_vectors("dot", args) {
        Ok((a, b)) => Value::Float(a.dot(b).unwrap_or(0.0)),
        Err(e) => e,
    }
}

pub(crate) fn builtin_vec_cross(args: &[Value]) -> Value {
    match args {
        [Value::Vector(a), Value::Vector(b)] if a.len == 3 && b.len == 3 => {
            let [ax, ay, az] = [a.components[0], a.components[1], a.components[2]];
            let [bx, by, bz] = [b.components[0], b.components[1], b.components[2]];
            Vector::new(&[ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx]).into_value()
        }
        _ => error("cross needs two vec3".to_string()),
    }
}

pub(crate) fn builtin_vec_length(args: &[Value]) -> Value {
    match one_vector("length", args) {
        Ok(v) => Value::Float(v.length()),
        Err(e) => e,
    }
}

pub(crate) fn builtin_vec_distance(args: &[Value]) -> Value {
    match two_vectors("distance", args) {
        Ok((a, b)) => Value::Float(a.zip(b, |x, y| x - y).map_or(0.0, |d| d.length())),
        Err(e) => e,
    }
}

pub(crate) fn builtin_vec_normalize(args: &[Value]) -> Value {
    match one_vector("normalize", args) {
        Ok(v) => {
            let length = v.length();
            if length == 0.0 {
                return error("cannot normalize a zero-length vector".to_string());
            }
            v.map(|c| (f64::from(c) / length) as f32).into_value()
        }
        Err(e) => e,
    }
}

pub(crate) fn builtin_vec_lerp(args: &[Value]) -> Value {
    match args {
        [Value::Vector(a), Value::Vector(b), t] if a.len == b.len => match scalar(t) {
            Some(t) => a
                .zip(b, |x, y| x + (y - x) * t)
                .map_or(Value::None, Vector::into_value),
            None => error("lerp needs a number to blend by".to_string()),
        },
        _ => error("lerp needs two vectors of the same size and a number".to_string()),
    }
}

/// `a op b` when a vector is involved: `Some` with the result or an error
/// message, or `None` to let the caller report its usual type mismatch.
pub(crate) fn arithmetic(a: &Value, b: &Value, op: char) -> Option<Result<Value, String>> {
    let vector = match (a, b) {
        (Value::Vector(l), Value::Vector(r)) => match op {
            '+' => l.zip(r, |x, y| x + y),
            '-' => l.zip(r, |x, y| x - y),
            '*' => l.zip(r, |x, y| x * y),
            _ => None,
        },
        (Value::Vector(v), n) => {
            let s = scalar(n)?;
            match op {
                '*' => Some(v.map(|c| c * s)),
                '/' if s == 0.0 => return Some(Err("division by zero".to_string())),
                '/' => Some(v.map(|c| c / s)),
                _ => None,
            }
        }
        (n, Value::Vector(v)) if op == '*' => {
            let s = scalar(n)?;
            Some(v.map(|c| c * s))
        }
        _ => None,
    };
    vector.map(|v| Ok(v.into_value()))
}
