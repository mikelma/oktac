use pest::iterators::Pair;

use super::{parser::*, *};
use crate::{VarType, current_unit_st};

pub fn parse_func_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    // get the visibilit and name of the function
    let next = pairs.next().unwrap();

    let (visibility, name) = match next.as_rule() {
        Rule::visibility => (
            misc::parse_visibility(next),
            pairs.next().unwrap().as_str().to_string(),
        ),
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        _ => unreachable!(),
    };

    // parse parameter definitions
    let params = parse_params_decl(pairs.next().unwrap());

    let next = pairs.next().unwrap();
    let ret_type = match next.as_rule() {
        Rule::retType => match ty::parse_var_type(next.into_inner().next().unwrap()) {
            Ok(t) => Some(t),
            Err(e) => {
                e.lines(pair_str).location(pair_loc).send().unwrap();
                Some(VarType::Unknown)
            }
        },
        Rule::stmts => todo!(),
        _ => unreachable!(),
    };

    // register the function in the symbol table
    let arg_types = params.iter().map(|x| x.1.clone()).collect();

    let res = current_unit_st!().record_func(&name, 
                                             ret_type.clone(), 
                                             arg_types, 
                                             visibility.clone());

    if let Err(e) = res {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::FuncProto {
        name,
        visibility,
        ret_type,
        params,
    }
}

pub fn parse_func_decl(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();

    let (visibility, name) = match next.as_rule() {
        Rule::visibility => (
            misc::parse_visibility(next),
            pairs.next().unwrap().as_str().to_string(),
        ),
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        _ => unreachable!(),
    };

    // set current function's name
    current_unit_st!().curr_func_set(&name);

    // this cannot panic as the current function was just set in the line above
    let (ret_type, params_ty) = current_unit_st!().curr_func_info().unwrap();

    // get parameter names and zip them with their respective types
    let mut params = vec![];
    for (param, ty) in pairs.next().unwrap().into_inner().zip(params_ty) {
        // get the second (index 1) pair of the `parameter` rule: the `id` of the parameter
        let id = param.into_inner().nth(1).unwrap().as_str().to_string();
        params.push((id, ty));
    }

    // create a new table for the function's scope
    current_unit_st!().push_table();

    // register the parameters in the function's scope
    params.iter().for_each(|(name, ty)| {
        if let Err(e) = current_unit_st!().record_var(name, ty.clone()) {
            e.lines(pair_str).location(pair_loc).send().unwrap();
        }
    });

    // parse statements block of the function
    let next = pairs.next().unwrap();
    let next = match next.as_rule() {
        Rule::retType => pairs.next().unwrap(),
        Rule::stmts => next,
        _ => unreachable!(),
    };

    let stmts = Box::new(stmts::parse_stmts(next));

    // pop function's scope symbol table
    current_unit_st!().pop_table();

    // restore current function's value
    current_unit_st!().curr_func_restore();

    AstNode::FuncDecl {
        name,
        visibility,
        params,
        ret_type,
        stmts,
    }
}

fn parse_params_decl(pair: Pair<Rule>) -> Vec<(String, VarType)> {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut params = vec![];

    for decl in pair.into_inner() {
        let mut inner = decl.into_inner();
        let var_type = ty::parse_ty_or_default(inner.next().unwrap(), Some((pair_str, pair_loc)));
        let id = inner.next().unwrap().as_str().to_string();
        params.push((id, var_type));
    }
    params
}

pub fn parse_extern_func_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();

    // get function name and visibility
    let (visibility, name) = match next.as_rule() {
        Rule::visibility => (
            misc::parse_visibility(next),
            pairs.next().unwrap().as_str().to_string(),
        ),
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        _ => unreachable!(),
    };

    // parse parameter types
    let mut param_types = vec![];
    for ty in pairs.next().unwrap().into_inner() {
        param_types.push(match ty::parse_var_type(ty) {
            Ok(t) => t,
            Err(e) => {
                e.lines(pair_str).location(pair_loc).send().unwrap();
                VarType::Unknown
            }
        });
    }

    // get the return type
    let ret_type = pairs.next().map(|ret_rule| {
        ty::parse_ty_or_default(
            ret_rule.into_inner().next().unwrap(),
            Some((pair_str, pair_loc)),
        )
    });

    // let rule = pairs.next();
    // let ret_type = match rule {
    //     Some(rt) => match ty::parse_var_type(rt.into_inner().next().unwrap()) {
    //         Ok(ty) => Some(ty),
    //         Err(e) => {
    //             e
    //              .location(pair_loc)
    //              .lines(pair_str)
    //              .send().unwrap();
    //             Some(VarType::Unknown)
    //         },
    //     },
    //     None => None,
    // };

    // register the function in the unit's symbol table
    let res = current_unit_st!().record_func(
        &name,
        ret_type.clone(),
        param_types.clone(),
        visibility.clone(),
    );

    if let Err(e) = res {
        e.send().unwrap();
    }

    AstNode::ExternFuncProto {
        name,
        param_types,
        ret_type,
        visibility,
    }
}
