use once_cell::sync::Lazy;
use pest::error::Error as PestErr;
use pest::iterators::Pair;
use pest::{error::LineColLocation, prec_climber::*, Parser};

use std::convert::TryInto;
use std::time::Instant;

use super::{LogMesg, VarType, ST};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TestParser;

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

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    FuncDecl {
        name: String,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,
        stmts: Box<AstNode>,
    },
    ExternFunc {
        name: String,
        ret_type: Option<VarType>,
        param_types: Vec<VarType>,
    },

    Stmts(Vec<AstNode>),

    // expressions
    VarDeclExpr {
        id: String,
        var_type: VarType,
        value: Box<AstNode>,
    },
    AssignExpr {
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    IfElseExpr {
        cond: Box<AstNode>,
        true_b: Box<AstNode>,
        false_b: Box<AstNode>,
    },
    ReturnExpr(Box<AstNode>),
    LoopExpr(Box<AstNode>), // contains an AstNode::Stmts variant inside
    BreakExpr,
    // valued expressions
    BinaryExpr {
        left: Box<AstNode>,
        op: BinaryOp,
        right: Box<AstNode>,
        expr_ty: VarType,
        vars_ty: VarType,
    },
    UnaryExpr {
        op: UnaryOp,
        value: Box<AstNode>,
        expr_ty: VarType,
        var_ty: VarType,
    },
    FunCall {
        name: String,
        params: Vec<AstNode>,
    },
    IndexationExpr {
        value: Box<AstNode>,
        indexes: Vec<AstNode>,
        ty: VarType,
    },

    // terminals
    Identifyer(String),
    Int8(i8),
    UInt8(u8),
    Int16(i16),
    UInt16(u16),
    Int32(i32),
    UInt32(u32),
    Int64(i64),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    Boolean(bool),
    /// The `Array` variant contains `values`, the elements of the array (can be an empty array)
    /// and `ty`, the `VarType` of the elements inside the array.
    Array {
        values: Vec<AstNode>,
        ty: VarType,
    },
}

impl AstNode {
    pub fn is_literal(&self) -> bool {
        match self {
            AstNode::Int32(_) | AstNode::UInt32(_) | AstNode::Boolean(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Gt,
    Leq,
    Geq,
}

impl BinaryOp {
    pub fn is_bool(&self) -> bool {
        match self {
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Leq
            | BinaryOp::Geq => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Iter {
    IntRange(i64, i64),
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, PestErr<Rule>> {
    let mut parsed = TestParser::parse(Rule::main, source)?;

    let mut main = parsed
        .next()
        .unwrap() // get `main` rule
        .into_inner();

    // create a table for the main scope
    ST.lock().unwrap().push_table();

    let mut parsed = vec![];
    while let Some(pair) = main.next() {
        parsed.push(match pair.as_rule() {
            Rule::funcDecl => parse_func_decl(pair),
            Rule::externFunc => parse_extern_func(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        });
    }

    // destroy the table for the main scope
    ST.lock().unwrap().pop_table();

    // programs always starts with a stmts block
    Ok(parsed)
}

fn parse_func_decl(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let params = parse_params_decl(pairs.next().unwrap());

    let next = pairs.next().unwrap();
    let (ret_type, next) = match next.as_rule() {
        Rule::retType => {
            let var_ty = parse_var_type(next.into_inner().next().unwrap());
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

    let stmts = Box::new(parse_stmts(next));

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
        let var_type = parse_var_type(inner.next().unwrap());
        let id = inner.next().unwrap().as_str().to_string();
        params.push((id, var_type));
    }
    params
}

fn parse_stmts(pair: Pair<Rule>) -> AstNode {
    let mut exprs = vec![]; // list of expressions inside the stmts block
    let mut stop_analyzing = false;
    let mut terminator = "";
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                if stop_analyzing {
                    // do not analyze more expressions after return statement
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

                let expr = parse_expr(pair);

                match expr {
                    AstNode::ReturnExpr(_) => {
                        stop_analyzing = true;
                        terminator = "return";
                    }
                    AstNode::BreakExpr => {
                        stop_analyzing = true;
                        terminator = "break";
                    }
                    _ => (),
                }

                exprs.push(expr);
            }
            _ => unreachable!(),
        }
    }
    AstNode::Stmts(exprs)
}

fn parse_extern_func(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    // get function name
    let name = pairs.next().unwrap().as_str().to_string();

    // parse parameter types
    let mut args = pairs.next().unwrap().into_inner();
    let mut param_types = vec![];
    while let Some(ty) = args.next() {
        param_types.push(parse_var_type(ty));
    }

    // get the return type
    let ret_type = match pairs.next() {
        Some(ret_rule) => Some(parse_var_type(ret_rule.into_inner().next().unwrap())),
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

fn parse_expr(pair: Pair<Rule>) -> AstNode {
    let expr = pair.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::varDeclExpr => parse_vardecl_expr(expr),
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::unaryExpr => parse_unary_expr(expr),
        Rule::funCallExpr => parse_func_call(expr),
        Rule::assignExpr => parse_assign_expr(expr),
        Rule::ifElseExpr => parse_ifelse_expr(expr),
        Rule::returnExpr => parse_return_expr(expr),
        Rule::loopExpr => parse_loop_expr(expr),
        Rule::breakExpr => AstNode::BreakExpr,
        Rule::whileExpr => parse_while_expr(expr),
        Rule::value => parse_value(expr),
        _ => unimplemented!(),
    }
}

fn parse_valued_expr(pairs: Pair<Rule>) -> AstNode {
    match pairs.as_rule() {
        Rule::unaryExpr => parse_unary_expr(pairs),
        Rule::binaryExpr => parse_binary_expr(pairs),
        Rule::value => parse_value(pairs),
        Rule::funCallExpr => parse_func_call(pairs),
        Rule::indexationExpr => parse_indexation_expr(pairs),
        _ => panic!("Expected valued expression"),
    }
}

fn parse_vardecl_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let var_type = parse_var_type(pairs.next().unwrap());
    let id = pairs.next().unwrap().as_str().to_string();

    let rval = parse_valued_expr(pairs.next().unwrap());
    let (rval, rty) = match node_type(rval, Some(var_type.clone())) {
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
    if let Err(err) = expect_type(var_type.clone(), &rty) {
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

fn parse_var_type(pair: Pair<Rule>) -> VarType {
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
            _ => unreachable!(),
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
        _ => unreachable!(),
    }
}

fn parse_unary_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();
    let op = match pairs.next().unwrap().as_rule() {
        Rule::not => UnaryOp::Not,
        _ => unreachable!(),
    };

    let value = parse_valued_expr(pairs.next().unwrap());
    let (value, var_ty) = match node_type(value, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    let expr_ty = match unop_resolve_type(&var_ty, &op) {
        Ok(val) => val,
        Err(err) => {
            err.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            VarType::Unknown
        }
    };

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
        |pair: Pair<Rule>| parse_valued_expr(pair),
        |lhs: AstNode, operator: Pair<Rule>, rhs: AstNode| {
            let (lhs, tmp_lty) = match node_type(lhs, None) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };
            let (rhs, rty) = match node_type(rhs, Some(tmp_lty)) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
                        .send()
                        .unwrap();
                    (node, VarType::Unknown)
                }
            };
            let (lhs, lty) = match node_type(lhs, Some(rty.clone())) {
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
                expr_ty: match binop_resolve_types(&lty, &rty, &op) {
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

fn parse_func_call(pair: Pair<Rule>) -> AstNode {
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
                    Some(p) => match node_type(p, Some(arg_ty.clone())) {
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
                if let Err(err) = expect_type(arg_ty.clone(), &call_param_ty) {
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

fn parse_parameters(pair: Pair<Rule>) -> Vec<AstNode> {
    let mut params = vec![];

    if pair.as_rule() == Rule::params {
        if let Some(p) = pair.into_inner().next() {
            return parse_parameters(p);
        }
    } else {
        // rule is `param`
        let mut pairs = pair.into_inner();
        while let Some(p) = pairs.next() {
            if p.as_rule() == Rule::param {
                params.append(&mut parse_parameters(p));
            } else {
                params.push(parse_valued_expr(p));
            }
        }
    }
    params
}

fn parse_ifelse_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let cond_rule = inner.next().unwrap();
    let cond = parse_valued_expr(cond_rule);
    let (cond, cond_ty_res) = node_type(cond, Some(VarType::Boolean));
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
    if let Err(err) = expect_type(VarType::Boolean, &cond_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    let true_b = parse_stmts(inner.next().unwrap());
    let false_b = parse_stmts(inner.next().unwrap());

    AstNode::IfElseExpr {
        cond: Box::new(cond),
        true_b: Box::new(true_b),
        false_b: Box::new(false_b),
    }
}

fn parse_assign_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        _ => unreachable!(),
    };

    let rval = parse_valued_expr(pairs.next().unwrap());

    let (lval, lty) = match node_type(lval, None) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            (node, VarType::Unknown)
        }
    };

    let (rval, rty) = match node_type(rval, Some(lty.clone())) {
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
    if let Err(err) = expect_type(lty, &rty) {
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

fn parse_value(pair: Pair<Rule>) -> AstNode {
    let value = pair.clone().into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Int64(value.as_str().parse().unwrap()),
        Rule::float => AstNode::Float32(value.as_str().parse().unwrap()),
        Rule::id => AstNode::Identifyer(value.as_str().to_string()),
        Rule::boolean => AstNode::Boolean(value.as_str().parse().unwrap()),
        Rule::array => {
            // parse all the values inside the array
            let values: Vec<AstNode> = value.into_inner().map(|v| parse_value(v)).collect();

            // get the value of the elements inside the array
            let ty = if values.is_empty() {
                // if the array is empty it's type cannot be known right now, later it's real type
                // will be inferred, for now, return an `Unknown` type
                VarType::Unknown
            } else {
                // get the type of the first element of the array
                match node_type(values[0].clone(), None).1 {
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

            AstNode::Array { values, ty }
        }
        _ => unreachable!(),
    }
}

fn parse_return_expr(pair: Pair<Rule>) -> AstNode {
    let fn_ret_ty = ST.lock().unwrap().curr_func().unwrap().0.clone();
    let inner = pair.clone().into_inner().next().unwrap();
    let (ret_value, ret_ty_res) = node_type(parse_valued_expr(inner), fn_ret_ty.clone());

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
    if let Err(err) = expect_type(fn_ret_ty.unwrap(), &ret_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    AstNode::ReturnExpr(Box::new(ret_value))
}

fn parse_loop_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let stmts = parse_stmts(inner);
    AstNode::LoopExpr(Box::new(stmts))
}

// NOTE: While expressions are expanded to loop+if expressions, thus there is no `AstNode` for
// `while` expressions.
fn parse_while_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let cond = parse_valued_expr(inner.next().unwrap());
    let (cond, cond_ty_res) = node_type(cond, Some(VarType::Boolean));
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
    if let Err(err) = expect_type(VarType::Boolean, &cond_ty) {
        err.lines(pair.as_str())
            .location(pair.as_span().start_pos().line_col().0)
            .send()
            .unwrap();
    }

    let mut stmts_list = match parse_stmts(inner.next().unwrap()) {
        AstNode::Stmts(list) => list,
        _ => unreachable!(),
    };

    let mut loop_body = vec![AstNode::IfElseExpr {
        cond: Box::new(cond),
        true_b: Box::new(AstNode::Stmts(vec![])),
        false_b: Box::new(AstNode::Stmts(vec![AstNode::BreakExpr])),
    }];

    loop_body.append(&mut stmts_list);

    AstNode::LoopExpr(Box::new(AstNode::Stmts(loop_body)))
}

fn parse_indexation_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let array_inner_ty = |node_ty: VarType| match node_ty {
        VarType::Array { inner, .. } => Some(inner),
        _ => {
            LogMesg::err()
                .name("Unexpected type".into())
                .cause(format!("{:?} type cannot be indexed", node_ty))
                .lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            None
        }
    };

    // check if the index has the correct type (u64)
    let check_index_type = |index_ty: &VarType| {
        if let Err(err) = expect_type(VarType::UInt64, index_ty) {
            err.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
        }
    };

    // get the varibale ident. and type
    let id = inner.next().unwrap();
    let (value, value_ty) = match id.as_rule() {
        Rule::id => match node_type(AstNode::Identifyer(id.as_str().to_string()), None) {
            (node, Ok(ty)) => (node, ty),
            (node, Err(e)) => {
                e.lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                (node, VarType::Unknown)
            }
        },
        _ => unreachable!(),
    };

    let (index, index_ty) = match node_type(
        parse_valued_expr(inner.next().unwrap()),
        Some(VarType::UInt64),
    ) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
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

    // let mut node = AstNode::IndexationExpr {
    //     indexes: vec![index], // dummy value
    //     value: Box::new(value),
    //     ty: value_ty.clone(),
    // };

    let mut indexes = vec![index];
    let mut ty = value_ty;

    while let Some(next_index) = inner.next() {
        // get the index value
        let (index, index_ty) =
            match node_type(parse_valued_expr(next_index), Some(VarType::UInt64)) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                        .location(pair.as_span().start_pos().line_col().0)
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

        // node = AstNode::IndexationExpr {
        //     index: Box::new(index), // dummy value
        //     value: Box::new(node),
        //     ty: value_ty.clone(),
        // };
    }
    AstNode::IndexationExpr {
        value: Box::new(value),
        indexes,
        ty,
    }
}

pub fn print_fancy_parse_err(err: pest::error::Error<Rule>) {
    let (err_line, err_col) = match err.line_col {
        LineColLocation::Pos((lin, col)) => (format!("{}", lin), format!("{}", col)),
        LineColLocation::Span((lin1, lin2), (col1, col2)) => {
            (format!("{}-{}", lin1, lin2), format!("{}-{}", col1, col2))
        }
    };
    eprintln!("[ERR] Syntax error in line: {}, col: {}", err_line, err_col);
    eprintln!("{}", err);
}

/// Checks if the left and right types are compatible considering the binary operator.
/// If types are not compatible, the function returns an error containing the name and
/// the cause of the error.
fn binop_resolve_types(
    l: &VarType,
    r: &VarType,
    op: &BinaryOp,
) -> Result<VarType, LogMesg<String>> {
    if *l == VarType::Unknown || *r == VarType::Unknown {
        return Ok(VarType::Unknown);
    }

    // check for boolean operations
    if op.is_bool() {
        // only values of the same type can be conpared
        if l == r {
            Ok(VarType::Boolean)
        } else {
            Err(LogMesg::err()
                .name("Mismatched types".to_string())
                .cause(format!(
                    "values of different types cannot be compared, left is {:?} and right is {:?}",
                    l, r
                )))
        }
    } else {
        // arithmetic operations
        // TODO: Replace `match` with `if`
        match (l, r) {
            (_, VarType::Unknown) => Ok(VarType::Unknown),
            (VarType::Unknown, _) => Ok(VarType::Unknown),
            (VarType::Int8, VarType::Int8) => Ok(VarType::Int8),
            (VarType::UInt8, VarType::UInt8) => Ok(VarType::UInt8),
            (VarType::Int16, VarType::Int16) => Ok(VarType::Int16),
            (VarType::UInt16, VarType::UInt16) => Ok(VarType::UInt16),
            (VarType::Int32, VarType::Int32) => Ok(VarType::Int32),
            (VarType::UInt32, VarType::UInt32) => Ok(VarType::UInt32),
            (VarType::Int64, VarType::Int64) => Ok(VarType::Int64),
            (VarType::UInt64, VarType::UInt64) => Ok(VarType::UInt64),
            (VarType::Float32, VarType::Float32) => Ok(VarType::Float32),
            (VarType::Float64, VarType::Float64) => Ok(VarType::Float64),
            (VarType::Boolean, VarType::Boolean) => Err(LogMesg::err()
                .name("Mismatched types".into())
                .cause(format!("cannot apply operator {:?} to booleans", op))),
            _ => Err(LogMesg::err()
                .name("Mismatched types".into())
                .cause(format!("left is {:?} and right is {:?}", l, r))),
        }
    }
}

/// Calculates the type of the unary operation. The function returns an error if the operation and
/// the type the operation is applied to are not compatible.
fn unop_resolve_type(ty: &VarType, op: &UnaryOp) -> Result<VarType, LogMesg<String>> {
    match op {
        UnaryOp::Not => match ty {
            VarType::Boolean => Ok(VarType::Boolean),
            _ => Err(LogMesg::err()
                .name("Mismatched types".into())
                .cause(format!("Cannot apply {:?} operator to {:?} type", op, ty))),
        },
    }
}

/// Checks if the given types are equal, if not, it returns a `LogMesg` error containing the name
/// of the error and the cause. If the given type is `VarType::Unknown` the function returns an
/// `Ok(())` in order to avoid cascading errors.
fn expect_type(expected: VarType, ty: &VarType) -> Result<(), LogMesg<String>> {
    if expected == *ty || *ty == VarType::Unknown {
        Ok(())
    } else {
        Err(LogMesg::err()
            .name("Mismatched types".to_string())
            .cause(format!(
                "Expected {:?} type, got {:?} type instead",
                expected, ty
            )))
    }
}

/// Given an `AstNode` returns it's `VarType`. However, if there is an expected type for the node,
/// and the node is a literal value, automatic type conversions can be applied to tranform the
/// literal to the expected type value.
fn node_type(
    node: AstNode,
    expect: Option<VarType>,
) -> (AstNode, Result<VarType, LogMesg<String>>) {
    let node_ty = match get_node_type_no_autoconv(&node) {
        Ok(ty) => ty,
        Err(e) => return (node, Err(e)),
    };
    // if some type was expected and the node is a literal value, try to convert the literal to the
    // expected type. If conversio is not sucsessfull, return the original type of the node
    if let Some(expected) = expect {
        match expected {
            VarType::Array {
                inner: ref exp_inner_ty,
                ..
            } => match &node {
                AstNode::Array {
                    values: arr_elems,
                    ty: _,
                } => {
                    let inner_ty = match node_ty {
                        VarType::Array { inner, .. } => inner,
                        _ => unreachable!(),
                    };
                    match *inner_ty {
                        // this case occurs when the array is declred empty (see `parse_value`)
                        VarType::Unknown => (
                            AstNode::Array {
                                values: vec![],
                                ty: *exp_inner_ty.clone(),
                            },
                            Ok(VarType::Array {
                                inner: exp_inner_ty.clone(),
                                len: 0,
                            }),
                        ),
                        _ => {
                            // if the array contains one or more values
                            // extract the type of the first element
                            let (first, new_ty) = match node_type(
                                arr_elems[0].clone(),
                                Some(*exp_inner_ty.clone()),
                            ) {
                                (f, Ok(t)) => (f, t),
                                (_, Err(e)) => {
                                    return (
                                        AstNode::Array {
                                            values: arr_elems.to_vec(),
                                            ty: VarType::Unknown,
                                        },
                                        Err(e),
                                    );
                                }
                            };

                            let mut new_elems = vec![first]; // list of the new elements of the array
                                                             // skip the first element, as it's type has already been checked
                            for elem in arr_elems.iter().skip(1).cloned() {
                                // try to get the type of the element node
                                new_elems.push(match node_type(elem, Some(new_ty.clone())) {
                                    // all the elements inside the array must have the same type
                                    (elem, Ok(e_ty)) => if e_ty == new_ty {
                                        elem
                                    } else {
                                        return (AstNode::Array { values: arr_elems.to_vec(), ty: VarType::Unknown },
                                            Err(LogMesg::err()
                                            .name("Mismatched types".into())
                                            .cause("All array elements must have the same type".into())));
                                    },
                                    (_, Err(e)) => {
                                        return (AstNode::Array { values: arr_elems.to_vec(), ty: VarType::Unknown }, Err(e));
                                    },
                                });
                            }
                            let len = new_elems.len();
                            (
                                AstNode::Array {
                                    values: new_elems,
                                    ty: new_ty.clone(),
                                },
                                Ok(VarType::Array {
                                    inner: Box::new(new_ty),
                                    len,
                                }),
                            )
                        }
                    }
                }
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt8 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int8 => match node {
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int32 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt32 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int64 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt64 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int16 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt16 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Float64 => match node {
                AstNode::Float32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Float64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Float32)),
                },
                _ => (node, Ok(node_ty)),
            },
            _ => (node, Ok(node_ty)),
        }
    } else {
        (node, Ok(node_ty))
    }
}

/// Extracts the `VarType` of a given `AstNode`.
///
/// NOTE: This function does not apply any automatic literal type conversion,
/// you might want to call `node_type` function instead.
fn get_node_type_no_autoconv(node: &AstNode) -> Result<VarType, LogMesg<String>> {
    match node {
        AstNode::BinaryExpr { expr_ty, .. } => Ok(expr_ty.clone()),
        AstNode::UnaryExpr { expr_ty, .. } => Ok(expr_ty.clone()),
        AstNode::Int8(_) => Ok(VarType::Int8),
        AstNode::UInt8(_) => Ok(VarType::UInt8),
        AstNode::Int16(_) => Ok(VarType::Int16),
        AstNode::UInt16(_) => Ok(VarType::UInt16),
        AstNode::Int32(_) => Ok(VarType::Int32),
        AstNode::Int64(_) => Ok(VarType::Int64),
        AstNode::UInt32(_) => Ok(VarType::UInt32),
        AstNode::Float32(_) => Ok(VarType::Float32),
        AstNode::Float64(_) => Ok(VarType::Float64),
        AstNode::Boolean(_) => Ok(VarType::Boolean),
        AstNode::Array { values, ty } => Ok(VarType::Array {
            inner: Box::new(ty.clone()),
            len: values.len(),
        }),
        AstNode::Identifyer(id) => match ST.lock().unwrap().search_var(id) {
            Ok(ty) => Ok(ty.clone()),
            Err(e) => Err(e),
        },
        AstNode::FunCall { name, .. } => match ST.lock().unwrap().search_fun(name) {
            Ok((ty, _)) => match ty {
                Some(t) => Ok(t.clone()),
                None => todo!(),
            },
            Err(e) => Err(e),
        },
        AstNode::IndexationExpr { ty, .. } => Ok(ty.clone()),
        _ => {
            println!("Panic was caused by: {:?}", node);
            unreachable!();
        }
    }
}
