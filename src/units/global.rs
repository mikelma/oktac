use once_cell::sync::Lazy;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread::ThreadId;

use super::CompUnitStatus;

// TODO: Consider using RwLock instead of Mutex
pub static GLOBAL_STAT: Lazy<Arc<Mutex<GlobalStatus>>> =
    Lazy::new(|| Arc::new(Mutex::new(GlobalStatus::default())));

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
