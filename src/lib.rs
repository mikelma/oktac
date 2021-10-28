#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use std::sync::Mutex;


mod types;
pub mod args;
pub mod ast;
pub mod codegen;
pub mod msg;
pub mod st;

pub use types::VarType;
pub use args::{EmitOpts, Opts};
pub use ast::{AstNode, BinaryOp, UnaryOp, Visibility};
pub use codegen::CodeGen;
pub use msg::{LogMesg, MessageType};
pub use st::ST;

pub struct GlobalStatus {
    /// number of errors
    pub errors: usize,
    /// number of warnings
    pub warnings: usize,
}

pub static GLOBAL_STAT: Lazy<Mutex<GlobalStatus>> = Lazy::new(|| {
    Mutex::new(GlobalStatus {
        errors: 0,
        warnings: 0,
    })
});
