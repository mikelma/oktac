mod check;
mod expr;
mod func;
mod misc;
mod parser;
mod stmts;
mod strct;
mod tree;
mod ty;
mod ty_enum;

pub use parser::{
    print_fancy_parse_err, parse_input, 
    generate_ast, generate_protos
};
pub use tree::*;
