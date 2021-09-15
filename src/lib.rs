#[macro_use]
extern crate pest_derive;

// #[macro_use]
// extern crate lazy_static;

use once_cell::sync::Lazy;
use std::sync::Mutex;

pub mod args;
pub mod ast;
pub mod codegen;
pub mod msg;
pub mod st;

pub use args::{EmitOpts, Opts};
pub use ast::{AstNode, BinaryOp, UnaryOp};
pub use codegen::CodeGen;
pub use msg::{LogMesg, MessageType};
pub use st::ST;

#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    UInt8,
    Int8,
    UInt16,
    Int16,
    UInt32,
    Int32,
    Int64,
    UInt64,
    Float32,
    Float64,
    Boolean,
    Array { inner: Box<VarType>, len: usize },
    /// Contains the name of the struct type
    Struct(String),
    // Contains the type it refers to
    Ref(Box<VarType>),
    Unknown,
}

pub struct GlobStatus {
    /// number of errors
    pub errors: usize,
    /// number of warnings
    pub warnings: usize,
}

pub static GLOBAL_STAT: Lazy<Mutex<GlobStatus>> = Lazy::new(|| {
    Mutex::new(GlobStatus {
        errors: 0,
        warnings: 0,
    })
});

impl VarType {
    /// Returns the "depth" of the array type. If `Self` is not of `Array` type, returns 0, else a
    /// number >= 1.
    pub fn array_depth(&self) -> usize {
        let mut depth = 0;
        let mut t = self;
        loop {
            match t {
                VarType::Array { inner, .. } => {
                    depth += 1;
                    t = &**inner;
                }
                _ => return depth,
            }
        }
    }
}
