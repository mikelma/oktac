use pest::iterators::Pair;

use super::{parser::*, *};

use crate::{current_unit_st, LogMesg, VarType};

pub fn parse_stmts(pair: Pair<Rule>) -> AstNode {
    let mut stmts = vec![]; // list of statements inside the stmts block
    let mut stop_analyzing = false;
    let mut terminator = "";

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::stmt => {
                if stop_analyzing {
                    // do not analyze more statements after return statement
                    LogMesg::warn()
                        .name("Unreachable code")
                        .cause(format!("Code after `{}` instruction", terminator))
                        .location(pair.as_span().start_pos().line_col().0)
                        .lines(pair.as_str())
                        .help(format!("Remove code after `{}` instruction", terminator))
                        .send()
                        .unwrap();
                    break;
                }

                let stmt = parse_stmt(pair);

                match stmt {
                    AstNode::ReturnStmt(_) => {
                        stop_analyzing = true;
                        terminator = "return";
                    }
                    AstNode::BreakStmt => {
                        stop_analyzing = true;
                        terminator = "break";
                    }
                    _ => (),
                }

                stmts.push(stmt);
            }
            _ => unreachable!(),
        }
    }

    AstNode::Stmts(stmts)
}

pub fn parse_stmt(pair: Pair<Rule>) -> AstNode {
    let stmt = pair.into_inner().next().unwrap();
    match stmt.as_rule() {
        Rule::loopStmt => parse_loop_stmt(stmt),
        Rule::assignStmt => parse_assign_stmt(stmt),
        Rule::ifLetStmt => parse_if_let_stmt(stmt),
        Rule::ifStmt => parse_if_stmt(stmt),
        Rule::returnStmt => parse_return_stmt(stmt),
        Rule::breakStmt => AstNode::BreakStmt,
        Rule::whileStmt => parse_while_stmt(stmt),
        Rule::forStmt => parse_for_stmt(stmt),
        Rule::varDeclStmt => parse_vardecl_stmt(stmt),

        Rule::funCallExpr => expr::parse_expr(stmt), // for function call statements

        _ => unreachable!("{:#?}", stmt),
    }
}

fn parse_loop_stmt(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();

    current_unit_st!().push_table();

    let stmts = stmts::parse_stmts(inner);

    current_unit_st!().pop_table();

    AstNode::LoopStmt(Box::new(stmts))
}

pub fn parse_vardecl_stmt(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let id = pairs.next().unwrap().as_str().to_string();

    let next = pairs.next().unwrap();

    let (next, var_type) = match next.as_rule() {
        Rule::varType => (
            pairs.next().unwrap(),
            Some(ty::parse_ty_or_default(next, None)),
        ),
        _ => (next, None),
    };

    let rval = expr::parse_expr(next);
    let (rval, rty) = match check::node_type(rval, var_type.clone()) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            (node, VarType::Unknown)
        }
    };

    // check if the type of the varible and the type of the right value do not conflict
    if let Some(ty) = &var_type {
        if let Err(err) = check::expect_type(ty.clone(), &rty) {
            err.lines(pair_str).location(pair_loc).send().unwrap();
        }
    }

    let var_type = match var_type {
        Some(t) => t,
        None => rty.clone(),
    };

    // register the variable in the symbol table
    let res = current_unit_st!().record_var(&id, var_type.clone());
    if let Err(err) = res {
        err.location(pair_loc).lines(pair_str).send().unwrap();
    }

    AstNode::VarDeclStmt {
        id,
        var_type,
        value: Box::new(rval),
    }
}

pub fn parse_if_stmt(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    // get the type of the expr,while checking if the type is boolean
    let is_expr_bool = |expr| {
        let (expr, cond_ty_res) = check::node_type(expr, Some(VarType::Boolean));
        (
            expr,
            match cond_ty_res {
                Ok(ty) => ty,
                Err(e) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    VarType::Unknown
                }
            },
        )
    };

    // parse the `if` block
    let mut if_pairs = inner.next().unwrap().into_inner();
    let cond_rule = if_pairs.next().unwrap();
    let (cond, _cond_ty) = is_expr_bool(expr::parse_expr(cond_rule));

    current_unit_st!().push_table();

    let then_b = stmts::parse_stmts(if_pairs.next().unwrap());

    current_unit_st!().pop_table();

    let mut elif_b = vec![];
    let mut else_b = None;

    for pair in inner {
        let rule = pair.as_rule();
        let mut pair_inner = pair.into_inner();

        current_unit_st!().push_table();

        match rule {
            Rule::elifBlock => {
                let (cond, _cond_ty) = is_expr_bool(expr::parse_expr(pair_inner.next().unwrap()));
                let elif_stmts = stmts::parse_stmts(pair_inner.next().unwrap());
                elif_b.push((cond, elif_stmts));
            }
            Rule::elseBlock => {
                let else_stmts = stmts::parse_stmts(pair_inner.next().unwrap());
                else_b = Some(Box::new(else_stmts));
            }
            _ => unreachable!(),
        }

        current_unit_st!().pop_table();
    }

    AstNode::IfStmt {
        cond: Box::new(cond),
        then_b: Box::new(then_b),
        elif_b,
        else_b,
    }
}

pub fn parse_if_let_stmt(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    // parse the left hand enum
    let enm = ty_enum::parse_enum_value(inner.next().unwrap(), true);
    let (enum_name, fields) = match &enm {
        AstNode::EnumVariant {
            enum_name, fields, ..
        } => (enum_name, fields),
        _ => unreachable!(),
    };

    // parse right hand expression (that must be another enum of the same type)
    let expr = expr::parse_expr(inner.next().unwrap());
    let (expr, expr_ty) = match check::node_type(expr, None) {
        (v, Ok(ty)) => (v, ty),
        (v, Err(e)) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            (v, VarType::Unknown)
        }
    };

    // check if the type of the enum and the right expression are equal
    if let Err(e) = check::expect_type(VarType::Enum(enum_name.into()), &expr_ty) {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    // parse `then` block statements
    current_unit_st!().push_table();

    for (_, ty, node) in fields {
        match node {
            AstNode::Identifyer(id) => {
                if let Err(e) = current_unit_st!().record_var(id, ty.clone()) {
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                }
            }
            _ => unreachable!(),
        }
    }

    let then_b = Box::new(parse_stmts(inner.next().unwrap()));

    current_unit_st!().pop_table();

    // parse `else` block (if some)
    let else_b = match inner.next() {
        Some(else_pair) => {
            current_unit_st!().push_table();

            let b = parse_stmts(else_pair);

            current_unit_st!().pop_table();

            Some(Box::new(b))
        }
        None => None,
    };

    AstNode::IfLetStmt {
        l_enum: Box::new(enm),
        r_expr: Box::new(expr),
        then_b,
        else_b,
    }
}

pub fn parse_assign_stmt(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.clone().into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        // Rule::indexationExpr => parse_indexation_expr(lhs),
        Rule::membAccessExpr => expr::parse_memb_access_expr(lhs),
        Rule::unaryExpr => expr::parse_unary_expr(lhs),
        _ => unreachable!(),
    };

    // check if the statement tries to assign a constant value
    if let Some(id) = check::check_depends_on_constant_value(&lval) {
        LogMesg::err()
            .name("Trying to assign constant")
            .cause(format!("Constant variable {} cannot be assigned", id))
            .location(pair_loc)
            .lines(pair_str)
            .send()
            .unwrap();
    }

    let rval = expr::parse_expr(pairs.next().unwrap());

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

    AstNode::AssignStmt {
        left: Box::new(lval),
        right: Box::new(rval),
    }
}

pub fn parse_return_stmt(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let fn_ret_ty = current_unit_st!().curr_func_info().unwrap().0;

    let inner = pair.into_inner().next().unwrap();
    let (ret_value, ret_ty_res) = check::node_type(expr::parse_expr(inner), fn_ret_ty.clone());

    let ret_ty = match ret_ty_res {
        Ok(ty) => ty,
        Err(e) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            VarType::Unknown
        }
    };

    match fn_ret_ty {
        Some(fn_ret) => {
            if let Err(err) = check::expect_type(fn_ret, &ret_ty) {
                err.lines(pair_str).location(pair_loc).send().unwrap();
            }
        }
        None => LogMesg::err()
            .name("Invalid return statement")
            .cause(
                "Return statement inside a function \
                   that does not have a return type"
                    .into(),
            )
            .help(format!(
                "Consider adding {:?} as the \
                          return type for the function",
                ret_ty
            ))
            .lines(pair_str)
            .location(pair_loc)
            .send()
            .unwrap(),
    }

    AstNode::ReturnStmt(Box::new(ret_value))
}

// NOTE: While statements are expanded to loop+if statements, thus there is no `AstNode` for
// `while` statements.
fn parse_while_stmt(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let cond = expr::parse_expr(inner.next().unwrap());
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

    current_unit_st!().push_table();

    let mut stmts_list = match stmts::parse_stmts(inner.next().unwrap()) {
        AstNode::Stmts(list) => list,
        _ => unreachable!(),
    };

    current_unit_st!().pop_table();

    let mut loop_body = vec![AstNode::IfStmt {
        cond: Box::new(AstNode::UnaryExpr {
            op: UnaryOp::Not,
            value: Box::new(cond),
            expr_ty: VarType::Boolean,
            var_ty: VarType::Boolean,
        }),
        then_b: Box::new(AstNode::Stmts(vec![AstNode::BreakStmt])),
        elif_b: vec![],
        else_b: None,
    }];

    loop_body.append(&mut stmts_list);

    AstNode::LoopStmt(Box::new(AstNode::Stmts(loop_body)))
}

// NOTE: For statements are expanded to stmt+loop+if statemets, thus there is no explicit
// `AstNode` for `for` statements.
fn parse_for_stmt(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let first_rule = inner.next().unwrap();

    current_unit_st!().push_table(); // start of the for loop's scope

    let (cond, first_stmt) = match first_rule.as_rule() {
        Rule::forValidStmt => {
            let s = parse_stmt(first_rule);
            let e = expr::parse_expr(inner.next().unwrap());
            (e, Some(s))
        }
        // otherwise, the rule is an expression
        _ => (expr::parse_expr(first_rule), None),
    };

    // get the type of the condition expression
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

    // check if the condition is of boolean type
    if let Err(err) = check::expect_type(VarType::Boolean, &cond_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    let mut next_rule = inner.next().unwrap();

    let second_stmt = match next_rule.as_rule() {
        Rule::forValidStmt => {
            let s = parse_stmt(next_rule);
            next_rule = inner.next().unwrap();
            Some(s)
        }
        _ => None,
    };

    let mut stmts_list = match stmts::parse_stmts(next_rule) {
        AstNode::Stmts(list) => list,
        _ => unreachable!(),
    };

    current_unit_st!().pop_table(); // end of the for loop's scope

    // the whole `for` statement will be translated to this statements block
    let mut main_stmts = vec![];

    // add the first statement of the `for` to the main statements block, if some
    if let Some(block) = first_stmt {
        main_stmts.push(block);
    }

    // create the main loop
    let mut loop_body = vec![AstNode::IfStmt {
        cond: Box::new(AstNode::UnaryExpr {
            op: UnaryOp::Not,
            value: Box::new(cond),
            expr_ty: VarType::Boolean,
            var_ty: VarType::Boolean,
        }),
        then_b: Box::new(AstNode::Stmts(vec![AstNode::BreakStmt])),
        elif_b: vec![],
        else_b: None,
    }];

    // add `for` loop's statemets block to the body of the loop
    loop_body.append(&mut stmts_list);

    // add the scond statement of the `for` to the end of the loop
    if let Some(block) = second_stmt {
        loop_body.push(block);
    }

    let loop_node = AstNode::LoopStmt(Box::new(AstNode::Stmts(loop_body)));
    main_stmts.push(loop_node);

    AstNode::Stmts(main_stmts)
}
