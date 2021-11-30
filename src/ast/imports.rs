use console::style;
use pest::iterators::Pair;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use super::parser::Rule;
use crate::{current_unit_status, CompUnitStatus, LogMesg, GLOBAL_STAT};

pub fn parse_use_module(pair: Pair<Rule>) -> Vec<PathBuf> {
    let mut use_mods = vec![];

    for p in pair.into_inner() {
        use_mods.push(PathBuf::from(p.into_inner().as_str()));
    }

    use_mods
}

/// Given a list of imported paths and the path of the module that imports them, validate the
/// imported path list.
///
/// Checks are:
/// * Redundant import paths.
/// * Check if the imported path really exists.
/// *
pub fn validate_imports(imports: &[PathBuf], units_path: &PathBuf) {
    let mut names = vec![];

    for path in imports {
        // check for redundant import names
        let name = path.file_name().unwrap();
        if names.contains(&name) {
            LogMesg::err()
                .name("Invalid import")
                .cause(format!(
                    "Ambiguous imports with name {}",
                    style(name.to_str().unwrap()).italic()
                ))
                .lines(format!("use {}", path.display()).as_str())
                .send()
                .unwrap();
        } else {
            names.push(&name);
        }

        // check if the import path does exist
        let does_not_exist = GLOBAL_STAT
            .lock()
            .unwrap()
            .units_by_path
            .keys()
            .find(|p| p.starts_with(path))
            .is_none();

        if does_not_exist {
            LogMesg::err()
                .name("Invalid import")
                .cause(format!(
                    "There is no module with path {}",
                    style(path.display()).italic()
                ))
                .lines(format!("use {}", path.display()).as_str())
                .send()
                .unwrap();
        }

        // check if the imported path is in the same level as the unit's module.
        // if `parent` is `None`, the unit is located in the project's root,
        // thus all imports are allowed
        if let Some(parent) = units_path.parent() {
            if !path.starts_with(parent) {
                LogMesg::err()
                    .name("Invalid import")
                    .cause("Relative imports of upper level modules are not allowed".into())
                    .lines(format!("use {}", path.display()).as_str())
                    // TODO
                    .help(
                        "Try importing the module using an absolute path (TODO: Not supported)"
                            .into(),
                    )
                    .send()
                    .unwrap();
            }
        }
    }
}

/// This function takes a list of imported paths as input and returns a map, where keys are paths and
/// values are the compilation unit that corresponds to the key's path.
///
/// Some paths might no refer to a single compilation unit, but to a group of compilation units
/// that share such path as prefix. In this case, the path is expanded into the paths that
/// correspond to the compilation units that share this prefix.
///
/// # Example:
///
/// Given the following file structure:
///
/// ```
/// |_ main.ok
/// |_ lib/
///       |_ foo.ok
///       |_ bar.ok
/// ```
///
/// The path `lib` gets expanded to `lib/foo` and `lib/bar`.
pub fn imported_units_map(imports: &[PathBuf]) -> HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>> {
    let mut map = HashMap::new();

    let all_paths: Vec<PathBuf> = GLOBAL_STAT
        .lock()
        .unwrap()
        .units_by_path
        .keys()
        .cloned()
        .collect();

    for imported in imports {
        {
            if let Some(unit_arc) = GLOBAL_STAT.lock().unwrap().units_by_path.get(imported) {
                map.insert(imported.clone(), Arc::clone(&unit_arc));
                continue;
            }
        }

        for path in &all_paths {
            if path.starts_with(imported) {
                match GLOBAL_STAT.lock().unwrap().units_by_path.get(path) {
                    Some(unit_arc) => {
                        let _ = map.insert(imported.clone(), Arc::clone(&unit_arc));
                    }
                    None => unreachable!(),
                }
            }
        }
    }

    map
}

/// Push all public symbols of imported modules into the symbol table of the current
/// compilation unit.
pub fn import_extern_symbols() {
    let imports = current_unit_status!().lock().unwrap().imports.clone();

    for (path, unit_arc) in imports {
        // don't allow to import symbols from the current unit into the current unit
        if current_unit_status!().lock().unwrap().path == *path {
            continue;
        }

        let pub_symbols = unit_arc.lock().unwrap().st.export_symbols();
        current_unit_status!()
            .lock()
            .unwrap()
            .st
            .import_symbols(pub_symbols);
    }
}
