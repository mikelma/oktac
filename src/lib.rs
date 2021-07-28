#[macro_use]
extern crate pest_derive;

// #[macro_use]
// extern crate lazy_static;

use std::sync::Mutex;
use once_cell::sync::Lazy;

pub mod ast;
pub mod codegen;
pub mod args;
pub mod msg;
pub mod st;

pub use ast::{AstNode, BinaryOp, UnaryOp};
pub use codegen::CodeGen;
pub use args::{Opts, EmitOpts};
pub use msg::{LogMesg, MessageType};

#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Int32,
    Boolean,
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
