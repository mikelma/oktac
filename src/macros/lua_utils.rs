use pest::Parser;

use crate::{
    ast::{self, check, parser::*},
    AstNode, LogMesg, VarType,
};

pub fn quote(source: &str) -> Vec<AstNode> {
    // let source = format!("{{ {} }}", input); // to match stmts rule (TODO: Improve)

    let mut parsed = match PestParser::parse(Rule::macroQuote, &source) {
        Ok(v) => v,
        Err(e) => {
            dbg!(e);
            todo!()
        }
    };

    let syntax_tree = parsed.next().unwrap();

    let mut result = vec![];

    let pair = syntax_tree.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::stmts => {
            for stmt_pair in pair.into_inner() {
                result.push(ast::stmts::parse_stmt(stmt_pair));
            }
        }
        Rule::stmt => result.push(ast::stmts::parse_stmt(pair)),
        _ => result.push(ast::expr::parse_expr(pair)),
    }

    result
}

pub fn get_node_type(node: AstNode) -> String {
    check::node_type(node, None)
        .1
        .unwrap_or(VarType::Unknown)
        .to_string()
}

pub fn compiler_error(
    macro_id: String,
    location: usize,
    lines: String,
    cause: Option<String>,
    help: Option<String>,
) {
    let mut error = LogMesg::err().name(format!("Macro error: {}", macro_id).as_str());

    if let Some(v) = cause {
        error = error.cause(v);
    } else {
        error = error.cause("Unknown cause".into());
    }

    if let Some(v) = help {
        error = error.help(v);
    }

    error
        .lines(lines.as_str())
        .location(location)
        .send()
        .unwrap();
}
