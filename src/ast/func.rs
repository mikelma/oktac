use pest::iterators::Pair;

use super::{parser::*, *};

use crate::{VarType, ST};

pub fn parse_func_decl(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let params = parse_params_decl(pairs.next().unwrap());

    let next = pairs.next().unwrap();
    let (ret_type, next) = match next.as_rule() {
        Rule::retType => {
            let var_ty = expr::parse_var_type(next.into_inner().next().unwrap());
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
        .record_func(&name, ret_type.clone(), arg_types);
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
        params,
        ret_type,
        stmts,
    }
}

fn parse_params_decl(pair: Pair<Rule>) -> Vec<(String, VarType)> {
    let mut pairs = pair.into_inner();
    let mut params = vec![];
    while let Some(decl) = pairs.next() {
        let mut inner = decl.into_inner();
        let var_type = expr::parse_var_type(inner.next().unwrap());
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
    let mut args = pairs.next().unwrap().into_inner();
    let mut param_types = vec![];
    while let Some(ty) = args.next() {
        param_types.push(expr::parse_var_type(ty));
    }

    // get the return type
    let ret_type = match pairs.next() {
        Some(ret_rule) => Some(expr::parse_var_type(ret_rule.into_inner().next().unwrap())),
        None => None,
    };

    let res = ST
        .lock()
        .unwrap()
        .record_func(&name, ret_type.clone(), param_types.clone());
    if let Err(e) = res {
        e.send().unwrap();
    }

    AstNode::ExternFunc {
        name,
        param_types,
        ret_type,
    }
}
