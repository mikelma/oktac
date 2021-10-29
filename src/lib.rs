#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use std::collections::HashMap;
use std::thread::ThreadId;

mod types;
pub mod args;
pub mod ast;
pub mod codegen;
pub mod msg;
pub mod st;

pub mod actions;

pub use types::VarType;
pub use args::{EmitOpts, Opts};
pub use ast::{AstNode, BinaryOp, UnaryOp, Visibility};
pub use codegen::CodeGen;
pub use msg::{LogMesg, MessageType};
// pub use st::ST;

#[derive(Default)]
pub struct GlobalStatus {
    pub units: HashMap<ThreadId, Mutex<CompUnitStatus>>,
}

// TODO: Consider using RwLock instead of Mutex
pub static GLOBAL_STAT: Lazy<Arc<Mutex<GlobalStatus>>> = Lazy::new(|| {
    Arc::new(Mutex::new(GlobalStatus::default()))
});

#[derive(Default)]
pub struct CompUnitStatus {
    /// compile unit filename
    pub filename: String,
    /// number of errors
    pub errors: Vec<LogMesg>,
    /// number of warnings
    pub warnings: Vec<LogMesg>,

    pub st: st::SymbolTableStack,

    pub protos: Vec<AstNode>,
    pub ast: AstNode,
}

impl CompUnitStatus {
    pub fn new(filename: &str) -> CompUnitStatus {
        CompUnitStatus { 
            filename: filename.into(), 
            ast: AstNode::Stmts(vec![]), 
            ..Default::default()
        }
    }
}

#[macro_export]
macro_rules! current_unit_status {
    () => {
        crate::GLOBAL_STAT
            .lock()
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
    };
}

#[macro_export]
macro_rules! current_unit_st {
    () => {
        crate::GLOBAL_STAT
            .lock() // first mutex (GlobalStatus)
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
            .lock() // second mutex (CompUnitStatus)
            .unwrap()
            .st
    };
}
