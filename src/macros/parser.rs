use console::style;
use pest::iterators::Pair;

use crate::{
    ast::{comp_ops, misc, parser::Rule, CompOpts, Visibility},
    current_unit_st, current_unit_status, AstNode, LogMesg,
};

use std::fs;

pub fn parse_macro(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut inner = pair.into_inner();

    let next = inner.next().unwrap();
    let (comp_ops, next) = if next.as_rule() == Rule::compOpts {
        (
            comp_ops::parse_comp_ops(next, comp_ops::SymbolType::Macro),
            inner.next().unwrap(),
        )
    } else {
        (CompOpts::default(comp_ops::SymbolType::Macro), next)
    };

    let (visibility, id) = match next.as_rule() {
        Rule::visibility => (
            misc::parse_visibility(next),
            inner.next().unwrap().as_str().to_string(),
        ),
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        _ => unreachable!(),
    };

    // check if the macro's code is here in the okta source file or in an extern lua file
    let next_rule = inner.next();

    let code = if let Some(next) = next_rule {
        // check if the `path` compilation option is given
        if comp_ops.get_option("path").into_optional().is_some() {
            LogMesg::err()
                .name("Invalid compilation option")
                .cause("Multiple sources given from the same macro".into())
                .help(format!(
                    "Consider removing the {} complation option",
                    style("path").italic()
                ))
                .lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
        }

        next.as_str().to_string()
    } else {
        // macro code must be in an extern lua file
        let default_code_src = "return {}"; // value to return in case of failure

        match comp_ops.get_option("path").into_optional() {
            Some(val) => {
                let path = current_unit_status!()
                    .lock()
                    .unwrap()
                    .path     // get unit's path
                    .parent() // remove unit's name
                    .unwrap()
                    .join(val.into_string());

                match fs::read_to_string(&path) {
                    Ok(code) => code,
                    Err(e) => {
                        LogMesg::err()
                            .name("Cannot open macro source")
                            .cause(format!("IO error: {}", e))
                            .lines(pair_str)
                            .location(pair_loc)
                            .send()
                            .unwrap();
                        default_code_src.to_string()
                    }
                }
            }
            None => {
                LogMesg::err()
                    .name("Missing macro source")
                    .cause(format!(
                        "There is no source for {} macro",
                        style(&id).bold()
                    ))
                    .help(format!(
                        "Consider adding a source string or {} complation option",
                        style("path").italic()
                    ))
                    .lines(pair_str)
                    .location(pair_loc)
                    .send()
                    .unwrap();
                default_code_src.to_string()
            }
        }
    };

    // record macro name and it's code in the symbol table of the current unit
    let res = current_unit_st!().record_macro(&id, code, visibility.clone());
    if let Err(err) = res {
        err.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::Macro {
        id: id.into(),
        visibility,
    }
}
