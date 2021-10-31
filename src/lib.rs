#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use std::collections::HashMap;
use std::thread::ThreadId;
use std::path::PathBuf;

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

    /// Path to this module (position of the 
    /// unit relative to the project's root path)
    pub path: PathBuf,

    /// number of errors
    pub errors: Vec<LogMesg>,
    /// number of warnings
    pub warnings: Vec<LogMesg>,

    pub st: st::SymbolTableStack,

    pub imports: Vec<AstNode>,
    pub protos: Arc<Vec<AstNode>>,
    pub ast: Arc<AstNode>,

    /// unique hash of the compilation unit, 
    /// based on it's AST
    pub hash: u64,
}

impl CompUnitStatus {
    pub fn new(filename: &str, path: PathBuf) -> CompUnitStatus {
        CompUnitStatus { 
            filename: filename.into(), 
            path,
            protos: Arc::new(vec![]), 
            ast: Arc::new(AstNode::Stmts(vec![])), 
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
