use console::style;
use pest::iterators::Pair;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use super::parser::Rule;
use crate::{
    current_unit_status, units::intrinsics::INTRINSICS_UNIT_NAME, CompUnitStatus, LogMesg,
    GLOBAL_STAT,
};

pub fn parse_use_module(pair: Pair<Rule>) -> Vec<PathBuf> {
    let mut use_mods = vec![];

    for p in pair.into_inner() {
        let path = p.as_str();
        use_mods.push(PathBuf::from(path));
    }

    use_mods
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
/// Considering we are located in `foo.ok`:
///
/// * Path `/lib/bar` == `bar` == `../lib/bar`
///
/// Considering we are located in `main.ok`:
///
/// * The path `lib` gets expanded to `lib/foo` and `lib/bar`.
pub fn imported_units_map(imports: &[PathBuf]) -> HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>> {
    let mut map = HashMap::new();

    let all_paths: Vec<PathBuf> = GLOBAL_STAT
        .lock()
        .unwrap()
        .units_by_path
        .keys()
        .cloned()
        .collect();

    let current_units_path = current_unit_status!().lock().unwrap().path.clone();
    let canonicalized_root_path = GLOBAL_STAT
        .lock()
        .unwrap()
        .project_root_path
        .canonicalize()
        .unwrap();

    for import in imports {
        let mut import_path = if import.has_root() {
            let mut abs_path = GLOBAL_STAT.lock().unwrap().project_root_path.clone();
            // remove the `/` at the begging of the `import` path
            abs_path.push(import.strip_prefix("/").unwrap());
            abs_path
        } else {
            let mut rel_path = current_units_path.clone();
            rel_path.pop();
            rel_path.push(import);
            rel_path
        };

        if !import_path.is_dir() {
            import_path.set_extension("ok");
        }

        let mut does_not_exist = match fs::canonicalize(&import_path) {
            Ok(normalized) => {
                import_path = normalized
                    .strip_prefix(&canonicalized_root_path)
                    .unwrap()
                    .to_path_buf();
                false
            }
            // error happend when the canonicalized path does not exist
            Err(_) => true,
        };

        // check if the import path does exist in the global modules list
        does_not_exist |= GLOBAL_STAT
            .lock()
            .unwrap()
            .units_by_path
            .keys()
            .find(|p| p.starts_with(&import_path))
            .is_none();

        if does_not_exist {
            LogMesg::err()
                .name("Invalid import")
                .cause(format!(
                    "There is no module with path {}",
                    style(import_path.display()).italic()
                ))
                .send()
                .unwrap();
            continue;
        }

        // be sure that we don't re-export ourselves or the `intrinsics` unit (again)
        if *import_path != current_units_path
            && import_path.to_str().unwrap() != INTRINSICS_UNIT_NAME
        {
            // check for an exact match
            if let Some(unit_arc) = GLOBAL_STAT.lock().unwrap().units_by_path.get(&import_path) {
                map.insert(import_path.clone(), Arc::clone(&unit_arc));
                continue;
            }
        }

        for path in &all_paths {
            if path.starts_with(&import_path) {
                match GLOBAL_STAT.lock().unwrap().units_by_path.get(path) {
                    Some(unit_arc) => {
                        // be sure that we don't re-export ourselves or the `intrinsics` unit (again)
                        if *path != current_units_path
                            && path.to_str().unwrap() != INTRINSICS_UNIT_NAME
                        {
                            let _ = map.insert(path.clone(), Arc::clone(&unit_arc));
                        }
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
