use pest::iterators::Pair;
use super::{AstNode, parser::Rule};
use crate::{GLOBAL_STAT, current_unit_st};

use std::path::PathBuf;

pub fn parse_use_module(pair: Pair<Rule>) -> Vec<AstNode> {
    let mut use_mods = vec![];

    for p in pair.into_inner() {
        let mut path = vec![]; 

        for m in p.into_inner() {
            path.push(m.as_str().into());
        }

        use_mods.push(AstNode::UseModule(path));
    }

    use_mods
}

/*
pub fn resolve_imports(use_mods: &[AstNode]) {
    // let mut imported_protos = vec![];
    let units = &GLOBAL_STAT.lock().unwrap().units;

    for node in use_mods {
        let modules = match node {
            AstNode::UseModule(v) => v,
            _ => unreachable!(),
        };

        for p in modules { // for every module to import
            let import_path = PathBuf::from(p);

            for (_, unit_mutex) in units {
                let units_path = &unit_mutex.lock().unwrap().path;
                if import_path == *units_path {

                }
            }
        }
    }
}
*/
