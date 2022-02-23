use console::style;

use std::collections::hash_map::DefaultHasher;
use std::hash::*;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

use super::*;
use crate::{ast, current_unit_protos, current_unit_st, current_unit_status, AstNode};

pub const INTRINSICS_UNIT_PATH: &'static str = "internal:intrinsics";
pub const INTRINSICS_UNIT_NAME: &'static str = "intrinsics";

pub fn intrinsics_unit() -> Result<Arc<Mutex<CompUnitStatus>>, String> {
    let src = include_str!("intrinsics.ok");

    let syntax_tree = match ast::parse_syntax_tree(&src) {
        Ok(p) => p,
        Err(err) => {
            let msg = format!(
                "{} syntax error in intrinsics unit\n{}",
                style("Internal compiler error:").red().bold(),
                err
            );
            return Err(msg);
        }
    };

    // create the compilation unit and insert it in `GlobalStatus`
    let unit = CompUnitStatus::new(INTRINSICS_UNIT_NAME, PathBuf::from(INTRINSICS_UNIT_PATH));
    GLOBAL_STAT
        .lock()
        .unwrap()
        .inser_unit(thread::current().id(), unit);

    current_unit_st!().push_table();

    let mut hasher = DefaultHasher::new();

    let imports = ast::rec_types_and_parse_imports_and_macros(syntax_tree.clone());
    imports.hash(&mut hasher);

    let protos = ast::generate_protos(syntax_tree.clone());
    protos.hash(&mut hasher);

    *current_unit_protos!().lock().unwrap() = protos
        .into_iter()
        .map(|v| Arc::new(v))
        .collect::<Vec<Arc<AstNode>>>();

    ast::validate_protos();

    let mut ast = ast::generate_ast(syntax_tree);
    ast.hash(&mut hasher);

    current_unit_status!()
        .lock()
        .unwrap()
        .ast
        .lock()
        .unwrap()
        .append(&mut ast);
    current_unit_status!().lock().unwrap().hash = hasher.finish();

    Ok(Arc::clone(&current_unit_status!()))
}
