use console::style;

use std::path::PathBuf;
use std::thread;
use std::sync::{Arc, Mutex};
use std::collections::hash_map::DefaultHasher;
use std::hash::*;

use crate::{ 
    ast, 
    AstNode,
    current_unit_st,
    current_unit_status,
};
use super::*;

pub const INTRINSICS_UNIT_PATH: &'static str = "internal:intrinsics";
pub const INTRINSICS_UNIT_NAME: &'static str = "intrinsics";

pub fn intrinsics_unit() -> Result<Arc<Mutex<CompUnitStatus>>, String> {
    let src = include_str!("intrinsics.ok");

    let syntax_tree = match ast::parse_syntax_tree(&src) {
        Ok(p) => p,
        Err(err) => {
            let msg = format!("{} syntax error in intrinsics unit\n{}", 
                              style("Internal compiler error:").red().bold(), 
                              err);
            return Err(msg);
        }
    };

    // create the compilation unit and insert it in `GlobalStatus`
    let unit = CompUnitStatus::new(INTRINSICS_UNIT_NAME, 
                                   PathBuf::from(INTRINSICS_UNIT_PATH));
    GLOBAL_STAT
        .lock()
        .unwrap()
        .inser_unit(thread::current().id(), unit);

    current_unit_st!().push_table();

    let mut hasher = DefaultHasher::new();

    let imports = ast::rec_types_and_parse_imports(syntax_tree.clone());
    imports.hash(&mut hasher);

    let protos = ast::generate_protos(syntax_tree.clone());
    protos.hash(&mut hasher);

    current_unit_status!().lock().unwrap().protos = Arc::new(
        protos
            .into_iter()
            .map(|v| Arc::new(v))
            .collect::<Vec<Arc<AstNode>>>(),
    );
    
    ast::validate_protos();

    // TODO: Constant variables are unsupported in the intrinsics module for now
    let (_, ast) = ast::generate_ast(syntax_tree);
    ast.hash(&mut hasher);

    current_unit_status!().lock().unwrap().ast = Arc::new(ast);
    current_unit_status!().lock().unwrap().hash = hasher.finish();

    Ok(Arc::clone(&current_unit_status!()))
}
