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

// TODO: Consider using RwLock instead of Mutex
pub static GLOBAL_STAT: Lazy<Arc<Mutex<GlobalStatus>>> = Lazy::new(|| {
    Arc::new(Mutex::new(GlobalStatus::default()))
});

#[derive(Default)]
pub struct GlobalStatus {
    pub units: HashMap<ThreadId, Arc<Mutex<CompUnitStatus>>>,
    pub units_by_path: HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>>,
}

impl GlobalStatus {
    pub fn inser_unit(&mut self, thread_id: ThreadId, unit: CompUnitStatus) {
        let path = unit.path.clone();
        let shared = Arc::new(Mutex::new(unit)); 

        self.units_by_path.insert(path, Arc::clone(&shared));
        self.units.insert(thread_id, shared);
    }
}

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

    pub imports: HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>>,

    pub protos: Arc<Vec<Arc<AstNode>>>,
    pub imported_protos: Arc<Vec<Arc<AstNode>>>,
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
