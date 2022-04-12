use console::style;
use once_cell::sync::Lazy;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread::ThreadId;

use crate::LogMesg;

use super::CompUnitStatus;

// TODO: Consider using RwLock instead of Mutex
pub static GLOBAL_STAT: Lazy<Arc<Mutex<GlobalStatus>>> =
    Lazy::new(|| Arc::new(Mutex::new(GlobalStatus::default())));

#[derive(Default)]
pub struct GlobalStatus {
    pub units: HashMap<ThreadId, Arc<Mutex<CompUnitStatus>>>,
    pub units_by_path: HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>>,
    pub project_root_path: PathBuf,
    pub include_paths: Vec<PathBuf>,

    pub errors: Vec<LogMesg>,
    pub warnings: Vec<LogMesg>,
}

impl GlobalStatus {
    pub fn inser_unit(&mut self, thread_id: ThreadId, unit: CompUnitStatus) {
        let path = unit.path.clone();
        let shared = Arc::new(Mutex::new(unit));

        self.units_by_path.insert(path, Arc::clone(&shared));
        self.units.insert(thread_id, shared);
    }

    pub fn set_include_paths(paths: &[String]) {
        let canonicalized = paths
            .iter()
            .filter_map(|path| match fs::canonicalize(path) {
                Ok(p) if p.is_dir() => Some(p),
                Ok(p) => {
                    LogMesg::err()
                        .name("Invalid path")
                        .cause(format!(
                            "Include path {} is not a directory",
                            style(p.display()).italic()
                        ))
                        .help(format!("Include paths must point to a directory"))
                        .send()
                        .unwrap();
                    None
                }
                Err(e) => {
                    LogMesg::err()
                        .name("Invalid path")
                        .cause(format!(
                            "Invalid include path {}: {}",
                            style(path).italic(),
                            e
                        ))
                        .help(format!("Check if the path exisits"))
                        .send()
                        .unwrap();
                    None
                }
            })
            .collect::<Vec<PathBuf>>();

        // here conflicts between include paths are detected.
        // There is conflict between two paths if both end with the same filename
        // Example: `/foo/bar/egg` and `bbb/thing/nopnop/egg` (both end with `egg`)
        let correct_paths = canonicalized
            .iter()
            .filter(|p1| {
                let conflict = canonicalized
                    .iter()
                    .find(|p2| p1 != p2 && p2.ends_with(p1.file_name().unwrap()));

                if let Some(p2) = conflict {
                    LogMesg::err()
                        .name("Conflicting include paths")
                        .cause(format!(
                            "Include paths `{}` and `{}` conflict: paths have same ending",
                            style(p1.display()).italic(),
                            style(p2.display()).italic()
                        ))
                        .help("Consider changing the last component of one of the paths".into())
                        .send()
                        .unwrap();
                }

                conflict.is_none()
            })
            .cloned()
            .collect::<Vec<PathBuf>>();

        GLOBAL_STAT.lock().unwrap().include_paths = correct_paths;
    }
}
