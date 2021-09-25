mod check;
mod tree;
mod parser;
mod func;
mod strct;
mod stmts;
mod expr;
mod ty;
mod misc;

pub use parser::{parse, print_fancy_parse_err};
pub use tree::*;
