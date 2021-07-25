#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod codegen;
pub mod args;
pub mod msg;

pub use ast::{AstNode, BinaryOp, UnaryOp};
pub use codegen::CodeGen;
pub use args::{Opts, EmitOpts};
pub use msg::{LogMesg, MessageType};

#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Int32,
    Boolean,
}
