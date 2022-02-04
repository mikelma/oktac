use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::{st, AstNode, LogMesg};

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
    pub macros: Arc<Vec<Arc<AstNode>>>,
    pub imported_protos: Arc<Vec<Arc<AstNode>>>,
    pub const_vars: Arc<Vec<Arc<AstNode>>>,
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
