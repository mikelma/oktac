use console::style;
use pest::error::Error as PestErr;
use pest::error::LineColLocation;
use pest::iterators::Pairs;
use pest::Parser;

use super::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PestParser;

/// Generates the Syntax Tree of the given source (this step is done by the `pest` crate).
/// The syntax tree is an untyped tree of the source, that can be semantically wrong, but it is
/// always syntactically correct (else, an error is returned).
pub fn parse_syntax_tree(source: &str) -> Result<Pairs<Rule>, PestErr<Rule>> {
    let mut parsed = PestParser::parse(Rule::main, source)?;

    Ok(parsed
        .next()
        .unwrap() // get `main` rule
        .into_inner())
}

pub fn generate_ast(main_pairs: Pairs<Rule>) -> AstNode {
    let mut subtrees = vec![];
    for pair in main_pairs {
        if pair.as_rule() == Rule::funcDecl {
            subtrees.push(func::parse_func_decl(pair));
        }
    }

    // all modules start with a stmts block, in other words,
    // the root node of all AST's is the `AstNode::Stmts` node
    AstNode::Stmts(subtrees)
}

// TODO: Use renamed rules (https://docs.rs/pest/latest/pest/error/struct.Error.html#method.renamed_rules)
pub fn print_fancy_parse_err(err: pest::error::Error<Rule>, path: &str) {
    let (err_line, err_col) = match err.line_col {
        LineColLocation::Pos((lin, col)) => (format!("{}", lin), format!("{}", col)),
        LineColLocation::Span((lin1, lin2), (col1, col2)) => {
            (format!("{}-{}", lin1, lin2), format!("{}-{}", col1, col2))
        }
    };
    eprintln!(
        "{} {}: line {}, column {}",
        style(format!("[[E] {}]", path)).red().bold(),
        style("Syntax error").bold(),
        err_line,
        err_col
    );
    eprintln!("{}", err);
}
