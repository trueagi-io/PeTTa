use crate::parser;

#[derive(Debug, Clone, PartialEq)]
pub enum MettaValue {
    Integer(String),
    Float(f64),
    Bool(bool),
    Atom(String),
    List(Vec<MettaValue>),
    Expression(String, Vec<MettaValue>),
}

impl MettaValue {
    pub fn parse(s: &str) -> Option<Self> {
        parser::parse_metta(s).ok()
    }

    pub fn as_integer(&self) -> Option<&str> {
        match self {
            MettaValue::Integer(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            MettaValue::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            MettaValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_atom(&self) -> Option<&str> {
        match self {
            MettaValue::Atom(s) | MettaValue::Integer(s) => Some(s),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MettaResult {
    pub value: String,
}

impl MettaResult {
    pub fn parsed_value(&self) -> Option<MettaValue> {
        MettaValue::parse(&self.value)
    }

    pub fn is_empty(&self) -> bool {
        self.value.trim().is_empty()
    }
}
