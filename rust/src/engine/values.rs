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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MettaResult {
    pub value: String,
}

impl MettaResult {
    pub fn parsed_value(&self) -> Option<MettaValue> {
        MettaValue::parse(&self.value)
    }
}
