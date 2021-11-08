use pest::error::Error as PestErr;
use pest::error::LineColLocation;
use pest::iterators::Pairs;
use pest::Parser;

use super::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct PestParser;

/// Generates the Syntax Tree of the given source (this step is done by the `pest` crate). 
/// The syntax tree is an untyped tree of the source, that can be semantically wrong, but it is
/// always syntactically correct (else, an error is returned). The sytax tree is then converted to
/// an AST, that is typed and semantically correct.
pub fn parse_syntax_tree(source: &str) -> Result<Pairs<Rule>, PestErr<Rule>> {
    let mut parsed = PestParser::parse(Rule::main, source)?;

    Ok(parsed.next()
             .unwrap() // get `main` rule
             .into_inner())
}

pub fn generate_ast(main_pairs: Pairs<Rule>) -> AstNode {
    let mut subtrees = vec![];
    for pair in main_pairs {
        if let Rule::funcDecl = pair.as_rule() {
            subtrees.push(func::parse_func_decl(pair));
        }
    }

    // all modules start with a stmts block, in other words,
    // the root node of all AST's is the `AstNode::Stmts` node
    AstNode::Stmts(subtrees)
}

pub fn print_fancy_parse_err(err: pest::error::Error<Rule>) {
    let (err_line, err_col) = match err.line_col {
        LineColLocation::Pos((lin, col)) => (format!("{}", lin), format!("{}", col)),
        LineColLocation::Span((lin1, lin2), (col1, col2)) => {
            (format!("{}-{}", lin1, lin2), format!("{}-{}", col1, col2))
        }
    };
    eprintln!("[ERR] Syntax error in line: {}, col: {}", err_line, err_col);
    eprintln!("{}", err);
}
