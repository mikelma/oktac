use pest::Parser;

use crate::{
    ast::{self, check, parser::*},
    AstNode, LogMesg, VarType,
};

pub fn quote(input: &str) -> Vec<AstNode> {
    let source = format!("{{ {} }}", input); // to match stmts rule (TODO: Improve)

    let mut parsed = match PestParser::parse(Rule::stmts, &source) {
        Ok(v) => v,
        Err(e) => {
            dbg!(e);
            todo!()
        }
    };

    let syntax_tree = parsed.next().unwrap();

    let mut stmts = vec![];
    for pair in syntax_tree.into_inner() {
        stmts.push(ast::stmts::parse_stmt(pair));
    }

    stmts
}

pub fn get_node_type(node: AstNode) -> String {
    check::node_type(node, None)
        .1
        .unwrap_or(VarType::Unknown)
        .to_string()
}

pub fn compiler_error(macro_id: String, cause: &str, lines: &str) {
    LogMesg::err()
        .name(format!("Macro error: {}", macro_id).as_str())
        .cause(cause.to_string())
        .lines(&lines)
        .send()
        .unwrap()
}
