use console::style;
use pest::iterators::Pair;

use std::path::PathBuf;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use super::parser::Rule;
use crate::{CompUnitStatus, GLOBAL_STAT, LogMesg, current_unit_status};

pub fn parse_use_module(pair: Pair<Rule>) -> Vec<PathBuf> {
    let mut use_mods = vec![];

    for p in pair.into_inner() {
        use_mods.push(
            PathBuf::from(p.into_inner().as_str())
        );
    }

    use_mods
}

pub fn validate_imports(imports: &[PathBuf], units_path: &PathBuf) {
    let mut names = vec![];

    for path in imports {
        // check for redundant import names
        let name = path.file_name().unwrap();
        if names.contains(&name) {
            LogMesg::err()
                .name("Invalid import")
                .cause(format!("Ambiguous imports with name {}", 
                               style(name.to_str().unwrap()).italic()))
                .lines(format!("use {}", path.display()).as_str())
                .send()
                .unwrap();
        } else {
            names.push(&name);
        }
        
        // check if the import path does exist
        let does_not_exist = GLOBAL_STAT.lock().unwrap().units_by_path
            .keys()
            .find(|p| p.starts_with(path)).is_none();

        if does_not_exist {
            LogMesg::err()
                .name("Invalid import")
                .cause(format!("There is no module with path {}", style(path.display()).italic()))
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
                .help("Try importing the module using an absolute path (TODO: Not supported)".into())
                .send()
                .unwrap();
            }
        }  
    }
}

pub fn imported_units_map(imports: &[PathBuf]) -> HashMap<PathBuf, Arc<Mutex<CompUnitStatus>>> {
    let mut map = HashMap::new();

    let all_paths: Vec<PathBuf> = GLOBAL_STAT.lock().unwrap()
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
                    },
                    None => unreachable!(),
                }
            }
        }
    }

    map
}

pub fn import_extern_symbols() {
    let imports = current_unit_status!().lock().unwrap().imports.clone();

    for (path, unit_arc) in imports {
        
        // don't allow to import symbols from the current unit into the current unit
        if current_unit_status!().lock().unwrap().path == *path {
            continue;
        }

        let pub_symbols = unit_arc.lock().unwrap().st.export_symbols();
        /*
        println!("{} Import to {}: {:?}", 
                 i,
                 current_unit_status!().lock().unwrap().path.display(),
                 pub_symbols,
        );
        */
        current_unit_status!().lock().unwrap().st.import_symbols(pub_symbols);
    }
}

/*
pub fn resolve_imports(use_mods: &[AstNode]) -> Vec<AstNode> {
    let mut imported_protos = vec![];

    println!("Entering resolve");

    {
        let units = &GLOBAL_STAT.lock().unwrap().units;
        println!("Units locked");

        for node in use_mods {
            let modules = match node {
                AstNode::UseModule(v) => v,
                _ => unreachable!(),
            };
            println!("modules: {:?}", modules);

            for p in modules { // for every module to import
                let import_path = PathBuf::from(p);

                for (_, unit_mutex) in units {
                    let units_path = &unit_mutex.lock().unwrap().path;
                    println!("units_path: {}", units_path.display());

                    if import_path == *units_path {

                        let protos = Arc::clone(&unit_mutex.lock().unwrap().protos);

                        for proto in &*protos {
                            let vis = match proto {
                                AstNode::FuncProto {visibility, ..} 
                                    | AstNode::StructProto {visibility, ..} 
                                    | AstNode::EnumProto {visibility, ..} => visibility,
                                _ => continue,
                            };

                            if *vis == Visibility::Pub && !imported_protos.contains(proto) {
                                println!("Importing proto: {:?}", proto);
                                imported_protos.push(proto.clone());
                                match proto {
                                    AstNode::FuncProto {name, ret_type, params, visibility} => {
                                        let args = params.iter().map(|p| p.1.clone()).collect();
                                        current_unit_st!().record_func(name, 
                                                                       ret_type.clone(), 
                                                                       args, 
                                                                       visibility.clone()).unwrap()
                                    },
                                    AstNode::StructProto {name, members, visibility} => {
                                        current_unit_st!().record_struct(name, 
                                                                         members.clone(), 
                                                                         visibility.clone()).unwrap()
                                    },
                                    AstNode::EnumProto {name, variants, visibility, ..} => {
                                        current_unit_st!().record_enum(name, 
                                                                         variants.clone(), 
                                                                         visibility.clone()).unwrap()
                                    },
                                    _ => (),
                                }; 
                            }
                        }
                    }
                    println!("En of for");
                }
                println!("End of units for");
            }
        }
    }
    println!("donee");

    imported_protos
}
*/
