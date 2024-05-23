use crate::LoxFunction;

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    LoxFunction(LoxFunction), 
    //Class(LoxClass),
    //Instance(LoxInstance),
    Nil,
}

impl Object {
    pub fn to_num(&self) -> Option<f64> { 
        if let Object::Number(n) = &self {
            return Some(*n);
        }

        if let Object::String(s) = &self {
            return s.trim().parse::<f64>().ok();
        }

        if let Object::Boolean(b) = &self {
            return if b == &true { Some(1.0) } else { Some(0.0) };
        }

        return None;
    }

    pub fn as_string(&self) -> Option<String> {
        if let Object::String(s) = &self { return Some(s.clone()) } else { return None }
    }

    pub fn is_string(&self) -> bool {
        if let Object::String(_) = self {
            return true;
        }

        return false;
    }
    pub fn is_num(&self) -> bool {
        if let Object::Number(_) = self {
            return true;
        }

        return false;
    }

    pub fn is_nil(&self) -> bool {
        if let Object::Nil = self {
            return true;
        }

        return false;
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Object::Nil => true,
            Object::Boolean(b) => !*b,
            _ => true,
        }
    }

}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            //(Object::Callable(a), Object::Callable(b)) => a == b,
            (Object::Nil, Object::Nil) => true,
            _ => false,
        }
    }
}

impl Eq for Object {}


impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Number(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Nil => write!(f, "nil"),
            Object::LoxFunction(lf) => write!(f, "LF:{}", lf.name)
        }
    }
}