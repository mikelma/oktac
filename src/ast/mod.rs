mod check;
mod expr;
mod func;
mod parser;
mod stmts;
mod tree;
mod strct;
mod ty;

pub use parser::{parse, print_fancy_parse_err};
pub use tree::*;
