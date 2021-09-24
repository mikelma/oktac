use pest::iterators::Pair;

use super::{parser::*, *};

use crate::{LogMesg, VarType, ST};

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
                        .cause(format!("Code after `{}` instruction", terminator).as_ref())
                        .location(pair.as_span().start_pos().line_col().0)
                        .lines(pair.as_str())
                        .help(format!("Remove code after `{}` instruction", terminator).as_ref())
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

fn parse_stmt(pair: Pair<Rule>) -> AstNode {
    let stmt = pair.into_inner().next().unwrap();
    match stmt.as_rule() {
        Rule::loopStmt => parse_loop_stmt(stmt),
        Rule::assignStmt => parse_assign_stmt(stmt),
        Rule::ifStmt => parse_if_stmt(stmt),
        Rule::returnStmt => parse_return_stmt(stmt),
        Rule::breakStmt => AstNode::BreakStmt,
        Rule::whileStmt => parse_while_stmt(stmt),
        Rule::varDeclStmt => parse_vardecl_stmt(stmt),

        Rule::funCallExpr => expr::parse_expr(stmt),

        _ => unreachable!("{:#?}", stmt),
    }
}

fn parse_loop_stmt(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let stmts = stmts::parse_stmts(inner);
    AstNode::LoopStmt(Box::new(stmts))
}

pub fn parse_vardecl_stmt(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let var_type = ty::parse_var_type(pairs.next().unwrap());
    let id = pairs.next().unwrap().as_str().to_string();

    let rval = expr::parse_expr(pairs.next().unwrap());
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
    let (cond, _cond_ty) = is_expr_bool(expr::parse_expr(cond_rule));
    let then_b = stmts::parse_stmts(if_pairs.next().unwrap());

    let n = inner.clone().count();
    let else_pairs = inner.clone().last();
    let elif_pairs = inner.clone().take(if n == 0 {0} else {n-1});

    // parse `elif` blocks 
    let mut elif_b = vec![];
    for pairs in elif_pairs {
        let mut inner = pairs.into_inner();

        let (cond, _cond_ty) = is_expr_bool(expr::parse_expr(inner.next().unwrap()));
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

    AstNode::IfStmt {
        cond: Box::new(cond),
        then_b: Box::new(then_b),
        elif_b,
        else_b,
    }
}

pub fn parse_assign_stmt(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        // Rule::indexationExpr => parse_indexation_expr(lhs),
        Rule::membAccessExpr => expr::parse_memb_access_expr(lhs),
        _ => unreachable!(),
    };

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
    let fn_ret_ty = ST.lock().unwrap().curr_func().unwrap().0.clone();
    let inner = pair.clone().into_inner().next().unwrap();
    let (ret_value, ret_ty_res) = check::node_type(expr::parse_expr(inner), fn_ret_ty.clone());

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

    AstNode::ReturnStmt(Box::new(ret_value))
}

// NOTE: While statements are expanded to loop+if expressions, thus there is no `AstNode` for
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

    let mut stmts_list = match stmts::parse_stmts(inner.next().unwrap()) {
        AstNode::Stmts(list) => list,
        _ => unreachable!(),
    };

    let mut loop_body = vec![
        AstNode::IfStmt {
            cond: Box::new(AstNode::UnaryExpr{ 
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
