use pest::iterators::Pair;

use super::{parser::*, *};
use crate::current_unit_st;

pub fn parse_visibility(pair: Pair<Rule>) -> Visibility {
    match pair.as_str() {
        "pub" => Visibility::Pub,
        _ => unreachable!(),
    }
}

pub fn parse_alias(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.into_inner();

    let next = inner.next().unwrap();
    let (visibility, name) = match next.as_rule() {
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        Rule::visibility => (
            misc::parse_visibility(next),
            inner.next().unwrap().as_str().to_string(),
        ),
        _ => unreachable!(),
    };

    let ty = ty::parse_ty_or_default(inner.next().unwrap(), Some((pair_str, pair_loc)));

    let res = current_unit_st!().record_alias(&name, ty.clone(), visibility.clone());
    if let Err(e) = res {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::AliasProto {
        name,
        visibility,
        ty,
    }
}
