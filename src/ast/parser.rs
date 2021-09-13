use pest::error::Error as PestErr;
use pest::error::LineColLocation;
use pest::Parser;

use super::*;
use crate::ST;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct PestParser;

pub fn parse(source: &str) -> Result<Vec<AstNode>, PestErr<Rule>> {
    let mut parsed = PestParser::parse(Rule::main, source)?;

    let mut main = parsed
        .next()
        .unwrap() // get `main` rule
        .into_inner();

    // create a table for the main scope
    ST.lock().unwrap().push_table();
    //dbg!(&main);

    let mut parsed = vec![];
    while let Some(pair) = main.next() {
        parsed.push(match pair.as_rule() {
            Rule::funcDecl => func::parse_func_decl(pair),
            Rule::externFunc => func::parse_extern_func(pair),
            Rule::structDef => strct::parse_struct_decl(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        });
    }

    // destroy the table for the main scope
    ST.lock().unwrap().pop_table();

    // programs always starts with a stmts block
    Ok(parsed)
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
