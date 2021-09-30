use pest::iterators::Pair;

use super::{parser::*, *};
use crate::{VarType, ST};

pub fn parse_func_decl(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();

    let (visibility, name) = match next.as_rule() {
        Rule::visibility => (misc::parse_visibility(next), 
                             pairs.next().unwrap().as_str().to_string()),
        Rule::id => (Visibility::Priv, next.as_str().to_string()), 
        _ => unreachable!(),
    };

    let params = parse_params_decl(pairs.next().unwrap());

    let next = pairs.next().unwrap();
    let (ret_type, next) = match next.as_rule() {
        Rule::retType => {
            let var_ty = ty::parse_var_type(next.into_inner().next().unwrap());
            (Some(var_ty), pairs.next().unwrap())
        }
        Rule::stmts => (None, next),
        _ => unreachable!(),
    };

    // register the function in the symbol table
    let arg_types = params.iter().map(|x| x.1.clone()).collect();
    let res = ST
        .lock()
        .unwrap()
        .record_func(&name, ret_type.clone(), arg_types, visibility.clone());
    if let Err(e) = res {
        e.send().unwrap();
    }

    // add a new table to the stack and register function parameters
    ST.lock().unwrap().push_table();
    for (name, ty) in &params {
        if let Err(e) = ST.lock().unwrap().record_var(name, ty) {
            e.send().unwrap();
        }
    }

    let stmts = Box::new(stmts::parse_stmts(next));

    // pop function's scope symbol table
    ST.lock().unwrap().pop_table();

    AstNode::FuncDecl {
        name,
        visibility,
        params,
        ret_type,
        stmts,
    }
}

fn parse_params_decl(pair: Pair<Rule>) -> Vec<(String, VarType)> {
    let mut params = vec![];
    for decl in pair.into_inner() {
        let mut inner = decl.into_inner();
        let var_type = ty::parse_var_type(inner.next().unwrap());
        let id = inner.next().unwrap().as_str().to_string();
        params.push((id, var_type));
    }
    params
}

pub fn parse_extern_func(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    // get function name
    let name = pairs.next().unwrap().as_str().to_string();

    // parse parameter types
    let mut param_types = vec![];
    for ty in pairs.next().unwrap().into_inner() {
        param_types.push(ty::parse_var_type(ty));
    }

    // get the return type
    let ret_type = pairs.next().map(|ret_rule| 
                                    ty::parse_var_type(
                                        ret_rule.into_inner().next().unwrap()));

    // TODO: Variable visibility of external functions (for now all external functions are public)
    let res = ST
        .lock()
        .unwrap()
        .record_func(&name, ret_type.clone(), param_types.clone(), Visibility::Pub);

    if let Err(e) = res {
        e.send().unwrap();
    }

    AstNode::ExternFunc {
        name,
        param_types,
        ret_type,
    }
}
