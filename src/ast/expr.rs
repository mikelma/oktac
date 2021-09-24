use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::*;

use super::parser::*;
use super::*;
use crate::{LogMesg, VarType, ST};

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

pub fn parse_expr(pair: Pair<Rule>) -> AstNode {
    let expr = pair.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::varDeclExpr => parse_vardecl_expr(expr),
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::unaryExpr => parse_unary_expr(expr),
        Rule::funCallExpr => parse_func_call(expr),
        Rule::assignExpr => parse_assign_expr(expr),
        Rule::ifExpr => parse_if_expr(expr),
        Rule::returnExpr => parse_return_expr(expr),
        Rule::loopExpr => parse_loop_expr(expr),
        Rule::breakExpr => AstNode::BreakExpr,
        Rule::whileExpr => parse_while_expr(expr),
        Rule::value => parse_value(expr),
        _ => unimplemented!(),
    }
}

pub fn parse_valued_expr(pairs: Pair<Rule>) -> AstNode {
    match pairs.as_rule() {
        Rule::unaryExpr => parse_unary_expr(pairs),
        Rule::binaryExpr => parse_binary_expr(pairs),
        Rule::value => parse_value(pairs),
        Rule::funCallExpr => parse_func_call(pairs),
        Rule::membAccessExpr => parse_memb_access_expr(pairs),
        _ => panic!("Expected valued expression: {:?}", pairs.as_rule()),
    }
}

pub fn parse_vardecl_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let var_type = parse_var_type(pairs.next().unwrap());
    let id = pairs.next().unwrap().as_str().to_string();

    let rval = parse_valued_expr(pairs.next().unwrap());
    let (rval, rty) = match check::node_type(rval, Some(var_type.clone())) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    // check if the type of the varible and the type of the right value do not conflict
    if let Err(err) = check::expect_type(var_type.clone(), &rty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    // register the variable in the symbol table
    if let Err(e) = ST.lock().unwrap().record_var(&id, &var_type) {
        e.send().unwrap();
    }

    AstNode::VarDeclExpr {
        id,
        var_type,
        value: Box::new(rval),
    }
}

pub fn parse_var_type(pair: Pair<Rule>) -> VarType {
    let inner = pair.clone().into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::simpleType => match pair.as_str() {
            "i8" => VarType::Int8,
            "u8" => VarType::UInt8,
            "i16" => VarType::Int16,
            "u16" => VarType::UInt16,
            "i32" => VarType::Int32,
            "u32" => VarType::UInt32,
            "i64" => VarType::Int64,
            "u64" => VarType::UInt64,
            "bool" => VarType::Boolean,
            "f32" => VarType::Float32,
            "f64" => VarType::Float64,
            name => match ST.lock().unwrap().symbol_type(name) {
                Ok(t) => t,
                Err(e) => {
                    e.lines(pair.as_str())
                     .location(pair.as_span().start_pos().line_col().0)
                     .send()
                     .unwrap();
                    VarType::Unknown
                },
            },
        },
        Rule::arrayType => {
            let mut inner = inner.into_inner();
            VarType::Array {
                inner: Box::new(parse_var_type(inner.next().unwrap())),
                len: match inner.next().unwrap().as_str().parse() {
                    Ok(v) => v,
                    Err(_) => {
                        LogMesg::err()
                            .name("Wrong value")
                            .cause("Invalid length for array, only natural numbers are allowed")
                            .lines(pair.as_str())
                            .send()
                            .unwrap();
                        0
                    }
                },
            }
        }
        Rule::refType => {
            VarType::Ref(Box::new(parse_var_type(inner.into_inner().next().unwrap())))
        },
        _ => unreachable!(),
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

    let value = parse_valued_expr(rval);

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
                    .cause("Cannot take address of rvalue, only variables can be referenced")
                    .help("Store the rvalue in a variable and then reference it")
                    .lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                expr_ty = VarType::Unknown;
            },
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
        parse_valued_expr,
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

    let fn_info = ST.lock().unwrap().search_fun(&name);
    match fn_info {
        Ok((_, fn_args)) => {
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
                params.push(parse_valued_expr(p));
            }
        }
    }
    params
}

pub fn parse_if_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    // get the type of the expr,while checking if the type is boolean
    let is_expr_bool = |expr| {
        let (expr, cond_ty_res) = check::node_type(expr, Some(VarType::Boolean));
        (expr, 
         match cond_ty_res {
            Ok(ty) => ty,
            Err(e) => {
                e.lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                VarType::Unknown
            }
        })
    };

    // parse the `if` block
    let mut if_pairs = inner.next().unwrap().into_inner();  
    let cond_rule = if_pairs.next().unwrap();
    let (cond, _cond_ty) = is_expr_bool(parse_valued_expr(cond_rule));
    let then_b = stmts::parse_stmts(if_pairs.next().unwrap());

    let n = inner.clone().count();
    let else_pairs = inner.clone().last();
    let elif_pairs = inner.clone().take(if n == 0 {0} else {n-1});

    // parse `elif` blocks 
    let mut elif_b = vec![];
    for pairs in elif_pairs {
        let mut inner = pairs.into_inner();

        let (cond, _cond_ty) = is_expr_bool(parse_valued_expr(inner.next().unwrap()));
        let elif_stmts = stmts::parse_stmts(inner.next().unwrap());
         
        elif_b.push((cond, elif_stmts));
    }
    
    // parse `else` block
    let else_b = match else_pairs {
        Some(else_pairs) => {
            let mut inner = else_pairs.into_inner();
            Some(Box::new(stmts::parse_stmts(inner.next().unwrap())))
        },
        None => None,
    };

    AstNode::IfExpr {
        cond: Box::new(cond),
        then_b: Box::new(then_b),
        elif_b,
        else_b,
    }
}

pub fn parse_assign_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        // Rule::indexationExpr => parse_indexation_expr(lhs),
        Rule::membAccessExpr => parse_memb_access_expr(lhs),
        _ => unreachable!(),
    };

    let rval = parse_valued_expr(pairs.next().unwrap());

    let (lval, lty) = match check::node_type(lval, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    let (rval, rty) = match check::node_type(rval, Some(lty.clone())) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    // check for type errors (ignore if type is Unknown)
    if let Err(err) = check::expect_type(lty, &rty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    AstNode::AssignExpr {
        left: Box::new(lval),
        right: Box::new(rval),
    }
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
            let values: Vec<AstNode> = value.into_inner().map(parse_valued_expr).collect();

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

            AstNode::Array { values, ty, is_const }
        }
        Rule::strct => strct::parse_struct_value(value),
        _ => unreachable!(),
    }
}

pub fn parse_return_expr(pair: Pair<Rule>) -> AstNode {
    let fn_ret_ty = ST.lock().unwrap().curr_func().unwrap().0.clone();
    let inner = pair.clone().into_inner().next().unwrap();
    let (ret_value, ret_ty_res) = check::node_type(parse_valued_expr(inner), fn_ret_ty.clone());

    let ret_ty = match ret_ty_res {
        Ok(ty) => ty,
        Err(e) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            VarType::Unknown
        }
    };

    if let Err(err) = check::expect_type(fn_ret_ty.unwrap(), &ret_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    AstNode::ReturnExpr(Box::new(ret_value))
}

pub fn parse_loop_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let stmts = stmts::parse_stmts(inner);
    AstNode::LoopExpr(Box::new(stmts))
}

// NOTE: While expressions are expanded to loop+if expressions, thus there is no `AstNode` for
// `while` expressions.
pub fn parse_while_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let cond = parse_valued_expr(inner.next().unwrap());
    let (cond, cond_ty_res) = check::node_type(cond, Some(VarType::Boolean));
    let cond_ty = match cond_ty_res {
        Ok(ty) => ty,
        Err(e) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            VarType::Unknown
        }
    };

    // check if condition is boolean type
    if let Err(err) = check::expect_type(VarType::Boolean, &cond_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    let mut stmts_list = match stmts::parse_stmts(inner.next().unwrap()) {
        AstNode::Stmts(list) => list,
        _ => unreachable!(),
    };

    let mut loop_body = vec![
        AstNode::IfExpr {
            cond: Box::new(AstNode::UnaryExpr{ 
                op: UnaryOp::Not, 
                value: Box::new(cond),
                expr_ty: VarType::Boolean,
                var_ty: VarType::Boolean,
            }),
            then_b: Box::new(AstNode::Stmts(vec![AstNode::BreakExpr])),
            elif_b: vec![],
            else_b: None,
    }];

    loop_body.append(&mut stmts_list);

    AstNode::LoopExpr(Box::new(AstNode::Stmts(loop_body)))
}

/*
pub fn parse_indexation_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();
    let root_rule = inner.next().unwrap();
    let root = match root_rule.as_rule() {
        Rule::id => AstNode::Identifyer(root_rule.as_str().to_string()),
        Rule::funCallExpr => expr::parse_func_call(root_rule),
        Rule::unaryExpr => expr::parse_unary_expr(root_rule),
        _ => unreachable!(),
    };

    parse_indices(root, inner.next().unwrap(), 
                  pair.as_str(), pair.as_span().start_pos().line_col().0)
}

pub fn parse_indices(root_node: AstNode, indices_pair: Pair<Rule>, pair_str: &str, pair_loc: usize) -> AstNode {
    let mut inner = indices_pair.into_inner();

    let array_inner_ty = |node_ty: VarType| match node_ty {
        VarType::Array { inner, .. } => Some(inner),
        _ => {
            LogMesg::err()
                .name("Unexpected type".into())
                .cause(format!("{:?} type cannot be indexed", node_ty))
                .lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
            None
        }
    };

    // check if the index has the correct type (u64)
    let check_index_type = |index_ty: &VarType| {
        if let Err(err) = check::expect_type(VarType::UInt64, index_ty) {
            err.lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
        }
    };

    // get the root varibale's type
    let (value, value_ty) = match check::node_type(root_node, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair_str)
             .location(pair_loc)
             .send()
             .unwrap();
            (node, VarType::Unknown)
        },
    };

    let (index, index_ty) = match check::node_type(
        parse_valued_expr(inner.next().unwrap()),
        Some(VarType::UInt64),
    ) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    check_index_type(&index_ty);

    let value_ty = match array_inner_ty(value_ty) {
        Some(t) => *t.clone(),
        None => VarType::Unknown,
    };

    let mut indexes = vec![index];
    let mut ty = value_ty;

    while let Some(next_index) = inner.next() {
        // get the index value
        let (index, index_ty) =
            match check::node_type(parse_valued_expr(next_index), Some(VarType::UInt64)) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair_str)
                        .location(pair_loc)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };

        check_index_type(&index_ty);

        ty = match array_inner_ty(ty) {
            Some(t) => *t.clone(),
            None => VarType::Unknown,
        };

        indexes.push(index);
    }

    AstNode::IndexationExpr {
        value: Box::new(value),
        indexes,
        ty,
    }
}
*/

fn parse_memb_access_expr(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    // parse the root node 
    let root_rule = inner.next().unwrap();
    let root = match root_rule.as_rule() {
        Rule::id => AstNode::Identifyer(root_rule.as_str().to_string()),
        Rule::funCallExpr => parse_func_call(root_rule),
        _ => unreachable!(),
    };

    // get the `VarType` of the root node
    let (root, root_ty) = match check::node_type(root, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    let mut members = vec![];
    let mut base_ty = root_ty.clone();

    for rule in inner {
        match rule.as_rule() {
            Rule::member => {
                let member_name = rule.into_inner().next().unwrap().as_str();
                let (index_node, ty) = strct::parse_strct_member(base_ty , member_name, 
                                                                 pair_str, pair_loc);

                members.push(AstNode::UInt32(index_node as u32));

                base_ty = ty;
            },
            Rule::indice => {
                base_ty = match base_ty {
                    VarType::Unknown => VarType::Unknown,
                    VarType::Array{ inner, .. } => *inner,
                    other => {
                        LogMesg::err()
                            .name("Invalid operation")
                            .cause(format!("Cannot index non Array type {:?}", other).as_str())
                            .lines(pair_str)
                            .location(pair_loc)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    },
                };

                let index_node = parse_indice(rule);
                members.push(index_node);
            },
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
    let node_index = parse_valued_expr(inner.next().unwrap());

    // get the type of the index
    match check::node_type(node_index, Some(VarType::UInt64)) {
        (node, Ok(ty)) => {
            // check if the type is the expected
            if let Err(err) = check::expect_type(VarType::UInt64, &ty) {
                err.lines(pair_str)
                    .location(pair_loc)
                    .send()
                    .unwrap();
            } 

            node
        },
        (node, Err(e)) => {
            e.lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
            node
        }
    }
}
