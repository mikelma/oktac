use pest::iterators::Pair;

use crate::{VarType, current_unit_st};

use super::{parser::*, *};

pub fn parse_visibility(pair: Pair<Rule>) -> Visibility {
    match pair.as_str() {
        "pub" => Visibility::Pub,
        _ => unreachable!(),
    }
}

// TODO: Check if the constant value depends on itself, in other words handle 
// reculrsive definitions with a proper error message
pub fn parse_const_var(pair: Pair<Rule>) -> (AstNode, AstNode) {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();
    let (vis, id) = match next.as_rule() {
        Rule::visibility => {
            let vis = parse_visibility(next);
            let id = pairs.next().unwrap().as_str().to_string();
            (vis, id)
        },
        // Rule::id
        _ => (Visibility::Priv, next.as_str().to_string()),
    };

    let mut ty = ty::parse_ty_or_default(
        pairs.next().unwrap(), Some((pair_str, pair_loc))
    );

    let value_pair = pairs.next().unwrap();
    let value = match value_pair.as_rule() {
        Rule::id => AstNode::Identifyer(value_pair.as_str().to_string()),
        _ => expr::parse_expr(value_pair),
    };

    // get the type of the right hand value
    let (value, val_ty) = match check::node_type(value, Some(ty.clone())) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(err)) => {
            err.lines(pair_str).location(pair_loc).send().unwrap();
            (node, VarType::Unknown)
        },
    };

    // check if the defined type and the type of the right value match
    if let Err(err) = check::expect_type(ty.clone(), &val_ty) {
        err.lines(pair_str).location(pair_loc).send().unwrap();
        ty = VarType::Unknown;
    }

    let res = current_unit_st!().record_const_var(&id, ty.clone(), vis.clone());
    if let Err(err) = res {
        err.lines(pair_str)
           .location(pair_loc)
           .send()
           .unwrap();
    }

    (
        AstNode::ConstVarProto {
        name: id.clone(),
        visibility: vis.clone(),
        ty: ty.clone(),
        },
        AstNode::ConstVarDecl {
            name: id,
            visibility: vis,
            value: Box::new(value),
            ty,
        }
    )
}
