use pest::iterators::Pair;

use super::{parser::*, *};
use crate::{current_unit_st, macros, st::SymbolTableStack, VarType};

pub fn parse_func_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();
    let (comp_ops, next) = if next.as_rule() == Rule::compOpts {
        (
            comp_ops::parse_comp_ops(next, comp_ops::SymbolType::Function),
            pairs.next().unwrap(),
        )
    } else {
        (CompOpts::default(comp_ops::SymbolType::Function), next)
    };

    // get the visibilit and name of the function
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
        Rule::stmts => None,
        _ => unreachable!(),
    };

    // register the function in the symbol table
    let arg_types = params.iter().map(|x| x.1.clone()).collect();

    let res = current_unit_st!().record_func(
        &name,
        ret_type.clone(),
        arg_types,
        visibility.clone(),
        false,
    );

    if let Err(e) = res {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    let func_proto = AstNode::FuncProto {
        name,
        visibility,
        ret_type,
        params,
        inline: comp_ops.get_option("inline").into_bool(),
    };

    // expand all derive macros
    for derive_macro in comp_ops.get_option("derive").into_vec() {
        if let Err(err) = macros::expand::macro_expand(
            &derive_macro.into_string(),
            &[func_proto.clone()],
            pair_loc,
            pair_str,
        ) {
            // lines and location get already set in `macro_expand`
            err.send().unwrap();
        }
    }

    func_proto
}

pub fn parse_func_decl(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let mut next = pairs.next().unwrap();
    let comp_ops = if next.as_rule() == Rule::compOpts {
        let v = comp_ops::parse_comp_ops(next, comp_ops::SymbolType::Function);
        next = pairs.next().unwrap();
        v
    } else {
        CompOpts::default(comp_ops::SymbolType::Function)
    };

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
        // get the second first pair of the `paramDecl` rule: the `id` of the parameter
        let id = param.into_inner().next().unwrap().as_str().to_string();
        params.push((id, ty));
    }

    // create a new table for the function's scope
    current_unit_st!().push_table();

    // register the parameters in the function's scope
    params.iter().for_each(|(name, ty)| {
        let res = current_unit_st!().record_var(name, ty.clone());
        if let Err(e) = res {
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

    let func_decl = AstNode::FuncDecl {
        name,
        visibility,
        params,
        ret_type,
        stmts,
    };

    // expand all derive macros
    for derive_macro in comp_ops.get_option("derive").into_vec() {
        if let Err(err) = macros::expand::macro_expand(
            &derive_macro.into_string(),
            &[func_decl.clone()],
            pair_loc,
            pair_str,
        ) {
            // lines and location get already set in `macro_expand`
            err.send().unwrap();
        }
    }

    func_decl
}

fn parse_params_decl(pair: Pair<Rule>) -> Vec<(String, VarType)> {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut params = vec![];

    for decl in pair.into_inner() {
        let mut inner = decl.into_inner();
        let id = inner.next().unwrap().as_str().to_string();
        let var_type = ty::parse_ty_or_default(inner.next().unwrap(), Some((pair_str, pair_loc)));
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
    let mut variadic = false;
    for p in pairs.next().unwrap().into_inner() {
        match p.as_rule() {
            Rule::varType => {
                param_types.push(match ty::parse_var_type(p) {
                    Ok(t) => t,
                    Err(e) => {
                        e.lines(pair_str).location(pair_loc).send().unwrap();
                        VarType::Unknown
                    }
                });
            }
            Rule::variadic => {
                variadic = true;
            }
            _ => unreachable!(),
        }
    }

    // get the return type
    let ret_type = pairs.next().map(|ret_rule| {
        ty::parse_ty_or_default(
            ret_rule.into_inner().next().unwrap(),
            Some((pair_str, pair_loc)),
        )
    });

    // register the function in the unit's symbol table
    let res = current_unit_st!().record_func(
        &name,
        ret_type.clone(),
        param_types.clone(),
        visibility.clone(),
        variadic,
    );

    if let Err(e) = res {
        e.send().unwrap();
    }

    AstNode::ExternFuncProto {
        name,
        param_types,
        ret_type,
        variadic,
        visibility,
    }
}

pub fn parse_lambda(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    // parse parameter definitions
    let params = parse_params_decl(pairs.next().unwrap());
    let arg_types: Vec<VarType> = params.iter().map(|x| x.1.clone()).collect();

    // parse return type (if some)
    let mut next = pairs.next().unwrap();
    let ret_ty = match next.as_rule() {
        Rule::retType => match ty::parse_var_type(next.clone().into_inner().next().unwrap()) {
            Ok(t) => {
                next = pairs.next().unwrap();
                Some(t)
            }
            Err(e) => {
                e.lines(pair_str).location(pair_loc).send().unwrap();
                Some(VarType::Unknown)
            }
        },
        Rule::stmts => None,
        _ => unreachable!(),
    };

    let mut name = SymbolTableStack::gen_unique_name();
    name.push_str(".lambda");

    let res =
        current_unit_st!().record_func(&name, ret_ty.clone(), arg_types, Visibility::Pub, false);

    if let Err(e) = res {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    // create a new table for the function's scope
    current_unit_st!().push_table();

    // register the parameters in the function's scope
    params.iter().for_each(|(name, ty)| {
        let res = current_unit_st!().record_var(name, ty.clone());
        if let Err(e) = res {
            e.lines(pair_str).location(pair_loc).send().unwrap();
        }
    });

    // get the name of the function where the lambda is being declared.
    // this never panics as lambdas must be always declared inside another function.
    let parent_func_name = current_unit_st!().curr_func().unwrap().to_string();

    // set the current function to parse in the symbol table
    current_unit_st!().curr_func_set(&name);

    // parse statements block of the function
    let stmts = Box::new(stmts::parse_stmts(next));

    // pop function's scope symbol table
    current_unit_st!().pop_table();

    // restore current function's value to the parent function
    current_unit_st!().curr_func_set(&parent_func_name);

    // TODO
    println!("***TODO***: Captured variables in lambdas");
    let captured_vars = vec![];

    AstNode::Lambda {
        name,
        ret_ty,
        params,
        stmts,
        captured_vars,
    }
}
