use pest::iterators::Pair;

use super::{parser::*, *};

use crate::LogMesg;

pub fn parse_stmts(pair: Pair<Rule>) -> AstNode {
    let mut exprs = vec![]; // list of expressions inside the stmts block
    let mut stop_analyzing = false;
    let mut terminator = "";
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                if stop_analyzing {
                    // do not analyze more expressions after return statement
                    LogMesg::warn()
                        .name("Unreachable code")
                        .cause(format!("Code after `{}` instruction", terminator).as_ref())
                        .location(pair.as_span().start_pos().line_col().0)
                        .lines(pair.as_str())
                        .help(format!("Remove code after `{}` instruction", terminator).as_ref())
                        .send()
                        .unwrap();
                    break;
                }

                let expr = expr::parse_expr(pair);

                match expr {
                    AstNode::ReturnExpr(_) => {
                        stop_analyzing = true;
                        terminator = "return";
                    }
                    AstNode::BreakExpr => {
                        stop_analyzing = true;
                        terminator = "break";
                    }
                    _ => (),
                }

                exprs.push(expr);
            }
            _ => unreachable!(),
        }
    }
    AstNode::Stmts(exprs)
}
