mod check;
mod expr;
mod func;
mod parser;
mod stmts;
mod tree;
mod strct;

pub use parser::{parse, print_fancy_parse_err};
pub use tree::*;
