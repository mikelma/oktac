use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::*;

use super::parser::*;
use super::*;
use crate::{LogMesg, VarType, current_unit_st};

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    use Assoc::*;
    use Rule::*;

    PrecClimber::new(vec![
        Operator::new(and, Left) | Operator::new(or, Left),
        Operator::new(lt, Left)
            | Operator::new(gt, Left)
            | Operator::new(leq, Left)
            | Operator::new(geq, Left)
            | Operator::new(eq, Left)
            | Operator::new(ne, Left),
        Operator::new(add, Left) | Operator::new(subtract, Left),
        Operator::new(multiply, Left) | Operator::new(divide, Left),
        // Operator::new(power, Right)
    ])
});

// pub fn parse_expr(pair: Pair<Rule>) -> AstNode {
//     let expr = pair.into_inner().next().unwrap();
//     match expr.as_rule() {
//         Rule::binaryExpr => parse_binary_expr(expr),
//         Rule::unaryExpr => parse_unary_expr(expr),
//         Rule::funCallExpr => parse_func_call(expr),
//         Rule::value => parse_value(expr),
//         _ => unimplemented!(),
//     }
// }

pub fn parse_expr(expr: Pair<Rule>) -> AstNode {
    // let expr = pairs.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::unaryExpr => parse_unary_expr(expr),
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::value => parse_value(expr),
        Rule::funCallExpr => parse_func_call(expr),
        Rule::membAccessExpr => parse_memb_access_expr(expr),
        Rule::id => AstNode::Identifyer(expr.as_str().to_string()),
        _ => panic!("Expected valued expression: {:#?}", expr.as_rule()),
    }
}

pub fn parse_unary_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();
    let op = match pairs.next().unwrap().as_rule() {
        Rule::not => UnaryOp::Not,
        Rule::reference => UnaryOp::Reference,
        Rule::deref => UnaryOp::Deref,
        _ => unreachable!(),
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

    // in case of the reference operation, check if the rval is an identifier
    if expr_ty != VarType::Unknown && op == UnaryOp::Reference {
        match value {
            AstNode::Identifyer(_) => (),
            _ => {
                LogMesg::err()
                    .name("Invalid operation")
                    .cause("Cannot take address of rvalue, only variables can be referenced".into())
                    .help("Store the rvalue in a variable and then reference it".into())
                    .lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                expr_ty = VarType::Unknown;
            }
        }
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

pub fn parse_func_call(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    // get function's name
    let name = pairs.next().unwrap().as_str().to_string();
    // parse call's parameters
    let mut call_params = parse_parameters(pairs.next().unwrap());

    let fn_info = current_unit_st!().search_fun(&name);
    match fn_info {
        Ok(Some((_, fn_args))) => {
            if fn_args.len() < call_params.len() {
                LogMesg::err()
                    .name("Too many parameters".into())
                    .cause(format!("Too many arguments for `{}` function call", name))
                    .lines(pair.as_str())
                    .send()
                    .unwrap();
            }
            for (i, arg_ty) in fn_args.iter().enumerate() {
                // get the type of the i-th function call parameter
                let param = call_params.get(i).cloned();
                let call_param_ty = match param {
                    Some(p) => match check::node_type(p, Some(arg_ty.clone())) {
                        (node, Ok(ty)) => {
                            call_params[i] = node;
                            ty
                        }
                        (_, Err(e)) => {
                            e.lines(pair.as_str())
                                .location(pair.as_span().start_pos().line_col().0)
                                .send()
                                .unwrap();
                            VarType::Unknown
                        }
                    },
                    None => {
                        // there are missing parameters
                        LogMesg::err()
                            .name("Missing parameters".into())
                            .cause(format!(
                                "Parameter {:?} is missing for function {}",
                                arg_ty, name
                            ))
                            .lines(pair.as_str())
                            .location(pair.as_span().start_pos().line_col().0)
                            .send()
                            .unwrap();
                        continue;
                    }
                };

                // check if the function call parameter's type and the
                // actual function argument type match
                if let Err(err) = check::expect_type(arg_ty.clone(), &call_param_ty) {
                    err.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                }
            }
        }
        Ok(None) => (),
        Err(err) => {
            err.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
        }
    };

    AstNode::FunCall {
        name,
        params: call_params,
    }
}

pub fn parse_parameters(pair: Pair<Rule>) -> Vec<AstNode> {
    let mut params = vec![];

    if pair.as_rule() == Rule::params {
        if let Some(p) = pair.into_inner().next() {
            return parse_parameters(p);
        }
    } else {
        for p in pair.into_inner() {
            if p.as_rule() == Rule::param {
                params.append(&mut parse_parameters(p));
            } else {
                params.push(parse_expr(p));
            }
        }
    }
    params
}

pub fn parse_value(pair: Pair<Rule>) -> AstNode {
    let value = pair.clone().into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Int64(value.as_str().parse().unwrap()),
        Rule::float => AstNode::Float32(value.as_str().parse().unwrap()),
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
        Rule::derefVar => parse_unary_expr(root_rule),
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
    let mut base_ty = root_ty.clone();

    for rule in inner {
        match rule.as_rule() {
            Rule::member => {
                let member_name = rule.into_inner().next().unwrap().as_str();
                let (index_node, ty) =
                    strct::parse_strct_member_access(base_ty, member_name, pair_str, pair_loc);

                members.push(AstNode::UInt32(index_node as u32));

                base_ty = ty;
            }
            Rule::indice => {
                base_ty = match base_ty {
                    VarType::Unknown => VarType::Unknown,
                    VarType::Array { inner, .. } => *inner,
                    other => {
                        LogMesg::err()
                            .name("Invalid operation")
                            .cause(format!("Cannot index non Array type {:?}", other))
                            .lines(pair_str)
                            .location(pair_loc)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    }
                };

                let index_node = parse_indice(rule);
                members.push(index_node);
            }
            _ => unreachable!(),
        }
    }

    AstNode::MemberAccessExpr {
        parent: Box::new(root),
        parent_ty: root_ty,
        members,
        member_ty: base_ty,
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
