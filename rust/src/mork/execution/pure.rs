use super::super::expr::SourceItem;

// Stub types for compilation when eval_ffi is not available
// These are only needed to compile the macro-generated FFI functions
// In a full setup these would come from eval_ffi crate
#[repr(C)]
pub struct ExprSource { _priv: () }
#[repr(C)]
pub struct ExprSink { _priv: () }
pub struct EvalError { msg: String }

impl EvalError {
    pub fn from(msg: &str) -> Self { EvalError { msg: msg.to_string() } }
}

impl ExprSource {
    pub fn consume_head_check(&mut self, _name: &[u8]) -> Result<usize, EvalError> { Ok(0) }
    pub fn read(&mut self) -> Result<SourceItem, EvalError> { Err(EvalError::from("stub")) }
    pub fn consume(&mut self) -> Result<SourceItem, EvalError> { Err(EvalError::from("stub")) }
}

impl ExprSink {
    pub fn write(&mut self, _item: SourceItem) -> Result<(), EvalError> { Err(EvalError::from("stub")) }
    pub fn extend_from_slice(&mut self, _data: &[u8]) -> Result<(), EvalError> { Err(EvalError::from("stub")) }
}
