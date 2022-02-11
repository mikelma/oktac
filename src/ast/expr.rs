use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::*;
use snailquote;

use super::*;
use crate::{current_unit_st, macros, LogMesg, VarType};
use parser::*;

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    use Assoc::*;
    use Rule::*;

    // reference: https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
    PrecClimber::new(vec![
        // -------------- 1 -------------- //
        Operator::new(or, Left),
        // -------------- 2 -------------- //
        Operator::new(and, Left),
        // -------------- 3 -------------- //
        Operator::new(binaryAnd, Left),
        // -------------- 4 -------------- //
        Operator::new(binaryOr, Left),
        // -------------- 5 -------------- //
        Operator::new(binaryXor, Left),
        // -------------- 6 -------------- //
        Operator::new(eq, Left) | Operator::new(ne, Left),
        // -------------- 7 -------------- //
        Operator::new(lt, Left)
            | Operator::new(gt, Left)
            | Operator::new(leq, Left)
            | Operator::new(geq, Left),
        // -------------- 8 -------------- //
        Operator::new(shiftRight, Left) | Operator::new(shiftLeft, Left),
        // -------------- 9 -------------- //
        Operator::new(add, Left) | Operator::new(subtract, Left),
        // -------------- 10 -------------- //
        Operator::new(multiply, Left) | Operator::new(divide, Left) | Operator::new(modulo, Left),
    ])
});

pub fn parse_expr(expr: Pair<Rule>) -> AstNode {
    // let expr = pairs.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::unaryExpr => parse_unary_expr(expr),
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::value => parse_value(expr),
        Rule::funCallExpr => parse_func_call(expr),
        Rule::membAccessExpr => parse_memb_access_expr(expr),
        // Rule::id => AstNode::Identifyer(expr.as_str().to_string()),
        _ => panic!("Expected valued expression: {:#?}", expr.as_rule()),
    }
}

pub fn parse_unary_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();
    let op = match pairs.next().unwrap().as_rule() {
        Rule::minus => UnaryOp::Minus,
        Rule::not => UnaryOp::Not,
        Rule::reference => UnaryOp::Reference,
        Rule::deref => UnaryOp::Deref,
        Rule::binaryNot => UnaryOp::BinaryNot,
        _ => unreachable!("{:?}", pair.as_rule()),
    };

    let rval = pairs.next().unwrap();

    let value = parse_expr(rval);

    let (value, var_ty) = match check::node_type(value, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    let mut expr_ty = VarType::Unknown;
    if var_ty != VarType::Unknown {
        match check::unop_resolve_type(&var_ty, &op) {
            Ok(t) => expr_ty = t,
            Err(err) => {
                err.lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
            }
        };
    }

    AstNode::UnaryExpr {
        op,
        value: Box::new(value),
        expr_ty,
        var_ty,
    }
}

fn parse_binary_expr(pair: Pair<Rule>) -> AstNode {
    PREC_CLIMBER.climb(
        pair.clone().into_inner(),
        parse_expr,
        |lhs: AstNode, operator: Pair<Rule>, rhs: AstNode| {
            let (lhs, tmp_lty) = match check::node_type(lhs, None) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };
            let (rhs, rty) = match check::node_type(rhs, Some(tmp_lty)) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };
            let (lhs, lty) = match check::node_type(lhs, Some(rty.clone())) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };

            let op = match operator.as_rule() {
                Rule::add => BinaryOp::Add,
                Rule::subtract => BinaryOp::Subtract,
                Rule::multiply => BinaryOp::Multiply,
                Rule::divide => BinaryOp::Divide,
                Rule::and => BinaryOp::And,
                Rule::or => BinaryOp::Or,
                Rule::eq => BinaryOp::Eq,
                Rule::ne => BinaryOp::Ne,
                Rule::lt => BinaryOp::Lt,
                Rule::gt => BinaryOp::Gt,
                Rule::leq => BinaryOp::Leq,
                Rule::geq => BinaryOp::Geq,
                Rule::binaryAnd => BinaryOp::BinaryAnd,
                Rule::binaryOr => BinaryOp::BinaryOr,
                Rule::binaryXor => BinaryOp::BinaryXor,
                Rule::shiftLeft => BinaryOp::ShiftLeft,
                Rule::shiftRight => BinaryOp::ShiftRight,
                Rule::modulo => BinaryOp::Modulo,
                _ => unreachable!(),
            };
            AstNode::BinaryExpr {
                left: Box::new(lhs),
                right: Box::new(rhs),
                expr_ty: match check::binop_resolve_types(&lty, &rty, &op) {
                    Ok(val) => val,
                    Err(err) => {
                        err.lines(pair.as_str())
                            .location(pair.as_span().start_pos().line_col().0)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    }
                },
                op,
                vars_ty: lty, // left and right values have the same type
            }
        },
    )
}

/// This function parses: function calls, builtin function calls and calls to macros.
pub fn parse_func_call(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    // get function's name and if it is a call to a builtin function, to a macro or a function
    let name_rule = pairs.next().unwrap();
    let name = name_rule.as_str().to_string();
    let is_builtin = name_rule.as_rule() == Rule::builtinFuncId;

    let is_macro = current_unit_st!().is_macro(&name);

    let mut ret_ty = None; // return type of the called function

    // parse call's parameters
    let (mut call_params, had_error) = match parse_parameters(pairs.next().unwrap(), is_builtin) {
        Ok(v) => (v, false),
        Err(err) => {
            err.lines(pair_str).location(pair_loc).send().unwrap();
            (vec![], true)
        }
    };

    // if the function is a builtin function, call to it's specific check function
    if is_builtin {
        if let Err(err) = builtin::check_builtin_fun_call(&name, &call_params) {
            err.lines(pair_str).location(pair_loc).send().unwrap();
            ret_ty = Some(VarType::Unknown);
        } else {
            // set the return type of the called builtin function
            ret_ty = builtin::builtin_func_return_ty(&name, &call_params);
        }
    } else if is_macro {
        let res = current_unit_st!().search_macro(&name);
        let code = match res {
            Ok(c) => c,
            Err(err) => {
                err.location(pair_loc).lines(pair_str).send().unwrap();
                // return a default value on error
                return AstNode::MacroResult {
                    id: name,
                    stmts: vec![],
                };
            }
        };

        match macros::expand::macro_expand(
            name.clone(),
            pair_loc,
            pair_str.into(),
            &*code,
            &call_params,
        ) {
            Ok(ast) => return ast,
            Err(lua_err) => {
                LogMesg::err()
                    .name("Macro failed")
                    .cause(format!("Failed to run {} macro", name))
                    .lines(lua_err.to_string().as_str())
                    .location(pair_loc)
                    .send()
                    .unwrap();
                // return a default value on error
                return AstNode::MacroResult {
                    id: name,
                    stmts: vec![],
                };
            }
        }
    }

    // check if the parameters that the function takes and the parameters that the call provides
    // are compatible
    if !had_error && !is_builtin && !is_macro {
        // do not check parameters if there was an error parsing them
        let fn_info = current_unit_st!().search_fun(&name);
        match fn_info {
            Ok((ty, real_params, variadic)) => {
                // set the return type of the called function
                ret_ty = ty;

                if let Err(err) = check::check_function_call_arguments(
                    &name,
                    &mut call_params,
                    &real_params,
                    variadic,
                ) {
                    err.lines(pair_str).location(pair_loc).send().unwrap();
                }
            }
            Err(err) => {
                err.lines(pair_str).location(pair_loc).send().unwrap();
            }
        }
    }

    AstNode::FunCall {
        name,
        params: call_params,
        ret_ty,
        builtin: is_builtin,
    }
}

pub fn parse_parameters(pair: Pair<Rule>, builtin_fn: bool) -> Result<Vec<AstNode>, LogMesg> {
    let mut params = vec![];

    if pair.as_rule() == Rule::params {
        if let Some(p) = pair.into_inner().next() {
            return parse_parameters(p, builtin_fn);
        }
    } else {
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::param => params.append(&mut parse_parameters(p, builtin_fn)?),
                // otherwise, is Rule::valueOrType
                _ => params.push(ty::parse_value_or_type(p)),
            }
        }
    }
    Ok(params)
}

pub fn parse_value(pair: Pair<Rule>) -> AstNode {
    let value = pair.clone().into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Int64(value.as_str().parse().unwrap()),
        Rule::float => AstNode::Float64(value.as_str().parse().unwrap()),
        Rule::id => AstNode::Identifyer(value.as_str().to_string()),
        Rule::boolean => AstNode::Boolean(value.as_str().parse().unwrap()),
        Rule::array => {
            // parse all the values inside the array
            let values: Vec<AstNode> = value.into_inner().map(parse_expr).collect();

            // determine if all the elemets in the array initialization are constants
            // NOTE: this can be done before type inference as before type inference constant
            // values continue to be constant
            let is_const = values.iter().all(|node| node.is_const());

            // get the value of the elements inside the array
            let ty = if values.is_empty() {
                // if the array is empty it's type cannot be known right now, later it's real type
                // will be inferred, for now, return an `Unknown` type
                VarType::Unknown
            } else {
                // get the type of the first element of the array
                match check::node_type(values[0].clone(), None).1 {
                    Ok(t) => t,
                    Err(e) => {
                        e.lines(pair.as_str())
                            .location(pair.as_span().start_pos().line_col().0)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    }
                }
            };

            AstNode::Array {
                values,
                ty,
                is_const,
            }
        }
        Rule::strct => strct::parse_struct_value(value),
        Rule::enm => ty_enum::parse_enum_value(value, false),
        Rule::str => {
            let str_val = snailquote::unescape(value.as_str()).unwrap();
            // let bytes = str_val.as_bytes().iter().map(|b| AstNode::UInt8(*b));

            let mut bytes = str_val.as_bytes().to_vec();
            bytes.push(0x00); // all strings terminate with null byte

            AstNode::String(bytes)
        }
        _ => unreachable!(),
    }
}

pub fn parse_memb_access_expr(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    // parse the root node
    let root_rule = inner.next().unwrap();
    let root = match root_rule.as_rule() {
        Rule::unaryExpr => parse_unary_expr(root_rule),
        Rule::id => AstNode::Identifyer(root_rule.as_str().to_string()),
        Rule::funCallExpr => parse_func_call(root_rule),
        _ => unreachable!(),
    };

    // get the `VarType` of the root node
    let (root, root_ty) = match check::node_type(root, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            (node, VarType::Unknown)
        }
    };

    let mut members = vec![];
    let mut next_ty = vec![root_ty.clone()];

    for rule in inner {
        match rule.as_rule() {
            Rule::member => {
                let member_name = rule.into_inner().next().unwrap().as_str();
                let (index_node, ty) = strct::parse_strct_member_access(
                    next_ty.last().unwrap().clone(),
                    member_name,
                    pair_str,
                    pair_loc,
                );

                members.push(MemberAccess::MemberId(index_node as u32));

                next_ty.push(ty);
            }
            Rule::indice => {
                next_ty.push(match next_ty.last().unwrap().resolve_alias() {
                    VarType::Unknown => VarType::Unknown,
                    VarType::Str => VarType::UInt8,
                    VarType::Array { inner, .. } | VarType::Slice(inner) => *inner.clone(),
                    other => {
                        LogMesg::err()
                            .name("Invalid operation")
                            .cause(format!("Only arrays and slices can be indexed {}", other))
                            .lines(pair_str)
                            .location(pair_loc)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    }
                });

                let index_node = parse_indice(rule);
                members.push(MemberAccess::Index(index_node));
            }
            Rule::range => {
                next_ty.push(match next_ty.last().unwrap().resolve_alias() {
                    VarType::Unknown => VarType::Unknown,
                    VarType::Str => VarType::Str,
                    VarType::Array { inner, .. } | VarType::Slice(inner) => {
                        VarType::Slice(Box::new(*inner.clone()))
                    }
                    other => {
                        LogMesg::err()
                            .name("Invalid operation")
                            .cause(format!("Only arrays and slices can be sliced {}", other))
                            .lines(pair_str)
                            .location(pair_loc)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    }
                });

                let mut range_inner = rule.into_inner();
                let start = match range_inner.next().unwrap().into_inner().next() {
                    Some(v) => parse_expr(v),
                    None => AstNode::UInt64(0),
                };

                let end = range_inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .map(|v| parse_expr(v));

                members.push(MemberAccess::Range { start, end });
            }
            _ => unreachable!(),
        }
    }

    // remove the first type from the list, as it contains the
    // type of the parent, and we already have it stored in `root_ty`
    next_ty.remove(0);

    AstNode::MemberAccessExpr {
        parent: Box::new(root),
        parent_ty: root_ty,
        members,
        access_types: next_ty,
    }
}

/// Parses the index of an indexation operation, returning it's AstNode
fn parse_indice(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.into_inner();

    // parse the index
    let node_index = parse_expr(inner.next().unwrap());

    // get the type of the index
    match check::node_type(node_index, Some(VarType::UInt64)) {
        (node, Ok(ty)) => {
            // check if the type is the expected
            if let Err(err) = check::expect_type(VarType::UInt64, &ty) {
                err.lines(pair_str).location(pair_loc).send().unwrap();
            }

            node
        }
        (node, Err(e)) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            node
        }
    }
}
