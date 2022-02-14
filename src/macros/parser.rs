use pest::iterators::Pair;

use crate::{
    ast::{misc, parser::Rule, Visibility, comp_ops, CompOpts},
    current_unit_st, AstNode,
};

pub fn parse_macro(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut inner = pair.into_inner();

    let next = inner.next().unwrap();
    let (_comp_ops, next) = if next.as_rule() == Rule::compOpts {
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

    let code = inner.next().unwrap().as_str();

    // record macro name and it's code in the symbol table of the current unit
    let res = current_unit_st!().record_macro(&id, code.to_string(), visibility.clone());
    if let Err(err) = res {
        err.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::Macro {
        id: id.into(),
        visibility,
    }
}
