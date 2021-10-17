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
pub use ast::{AstNode, BinaryOp, UnaryOp, Visibility};
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
    /// Contains the name of the struct type.
    Struct(String),
    /// Contains the name of the enum type.
    Enum(String),
    // Contains the type it refers to.
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

    /// Returns the size of the type in bytes. If the type is `Unknown`, 
    ///
    /// **NOTE**: If the size in bits of the type is lower than 8, this function will return 1 as
    /// the size in bytes of the type.
    pub fn size(&self) -> usize {
        match &self {
            VarType::UInt8 | VarType::Int8 => 1,
            VarType::UInt16 | VarType::Int16 => 2,
            VarType::UInt32 | VarType::Int32 | VarType::Float32 => 4,
            VarType::Int64 | VarType::UInt64 | VarType::Float64 => 8,
            VarType::Boolean => 1,
            VarType::Array { inner, ..} => inner.size(),
            VarType::Ref(inner) => inner.size(),
            VarType::Struct(name) => todo!(),
            VarType::Enum(name) => todo!(),
            // variants.iter()
            // .map(|(_, fields)| fields.iter().map(|(_, ty)| ty.size()).sum()).max().unwrap_or(0),
            VarType::Unknown => 0,
        } 
    }

    /*
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
    */
}
