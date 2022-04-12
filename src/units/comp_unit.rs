use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::{st, AstNode, LogMesg, INTRINSICS_UNIT_NAME};

#[derive(Default)]
pub struct CompUnitStatus {
    /// Absolute and canonicalized path to this module
    pub path: PathBuf,
    // short version of `path` (only intented to be used in `msg` module)
    pub path_short: String,

    pub errors: Vec<LogMesg>,
    pub warnings: Vec<LogMesg>,

    pub st: st::SymbolTableStack,

    pub imports: HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>>,

    pub protos: Arc<Mutex<Vec<Arc<AstNode>>>>,
    pub macros: Arc<Vec<Arc<AstNode>>>,
    pub imported_protos: Arc<Vec<Arc<AstNode>>>,
    pub ast: Arc<Mutex<Vec<AstNode>>>,

    /// unique hash of the compilation unit,
    /// based on it's AST
    pub hash: u64,
}

impl CompUnitStatus {
    pub fn new(path: PathBuf) -> CompUnitStatus {
        CompUnitStatus {
            path_short: path.to_string_lossy().to_string(),
            // NOTE: This unwrap should not fail, as the path it's already ensured to exists when
            // reading the compilation unit's file
            path: if path.to_string_lossy() != INTRINSICS_UNIT_NAME {
                fs::canonicalize(path).unwrap()
            } else {
                path
            },
            protos: Arc::new(Mutex::new(vec![])),
            ast: Arc::new(Mutex::new(vec![])),
            ..Default::default()
        }
    }
}
