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

pub use parser::{parse, print_fancy_parse_err};
pub use tree::*;
