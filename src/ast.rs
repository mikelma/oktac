use pest::iterators::Pair;
use pest::{
    Parser,
    prec_climber::*,
    error::LineColLocation,
};
use pest::error::Error as PestErr;
use once_cell::sync::Lazy;

use std::convert::TryInto;

use super::{VarType, LogMesg, ST};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TestParser;

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    use Rule::*;
    use Assoc::*;

    PrecClimber::new(vec![
        Operator::new(and, Left) | Operator::new(or, Left) ,
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
    FuncDecl { name: String, ret_type: Option<VarType>, 
        params: Vec<(String, VarType)>, stmts: Box<AstNode> },

    Stmts(Vec<AstNode>),

    // expressions
    VarDeclExpr { id: String, var_type: VarType, value: Box<AstNode> },
    BinaryExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>, expr_ty: VarType, vars_ty: VarType},
    UnaryExpr  { op: UnaryOp, value: Box<AstNode>, expr_ty: VarType, var_ty: VarType},
    AssignExpr { left: Box<AstNode>, right: Box<AstNode>},
    FunCall { name: String, params: Vec<AstNode> },
    IfElseExpr { cond: Box<AstNode>, true_b: Box<AstNode>, false_b: Box<AstNode> },
    ReturnExpr(Box<AstNode>),
    LoopExpr(Box<AstNode>), // contains an AstNode::Stmts variant inside
    BreakExpr,

    // terminals
    Identifyer(String),
    Int32(i32),
    UInt32(u32),
    Boolean(bool),
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
    IntRange(i64, i64) 
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, PestErr<Rule>> {
    let mut parsed = TestParser::parse(Rule::main, source)?;

    let mut main = parsed.next().unwrap() // get `main` rule
        .into_inner();

    // create a table for the main scope
    ST.lock().unwrap().push_table();

    let mut parsed = vec![];
    while let Some(pair) = main.next() {
        parsed.push(match pair.as_rule() {
            Rule::funcDecl => parse_func_decl(pair),
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
        },
        Rule::stmts => (None, next),
        _ => unreachable!(),
    };

    // register the function in the symbol table
    let args = params.clone();
    let res = ST.lock().unwrap().record_func(&name, ret_type.clone(), args);

    if let Err(e) = res {
        e.send().unwrap();
    }

    // add a new table to the stack and register function parameters
    ST.lock().unwrap().push_table();
    for (name, ty) in &params {
        if let Err(e) =ST.lock().unwrap().record_var(name, ty) {
            e.send().unwrap();
        }
    }

    let stmts = Box::new(parse_stmts(next));

    // pop function's scope symbol table
    ST.lock().unwrap().pop_table();

    AstNode::FuncDecl {
        name, params, ret_type, stmts
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
    let mut exprs = vec![];  // list of expressions inside the stmts block
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
                    },
                    AstNode::BreakExpr => {
                        stop_analyzing = true;
                        terminator = "break";
                    },
                    _ => (),
                }

                exprs.push(expr);
            }
            _ => unreachable!(),
        }
    }
    AstNode::Stmts(exprs)
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
        },
    };

    // check if condition is boolean type
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

    AstNode::VarDeclExpr { id, var_type, value: Box::new(rval) }
}

fn parse_var_type(pair: Pair<Rule>) -> VarType {
    match pair.as_str() {
        "i32" => VarType::Int32,
        "u32" => VarType::UInt32,
        "bool" => VarType::Boolean,
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
        },
    };

    let expr_ty = match unop_resolve_type(&var_ty, &op) {
        Ok(val) => val,
        Err(err) =>  {
            err.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
            VarType::Unknown
        },
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
                },
            };
            let (rhs, rty) = match node_type(rhs, Some(tmp_lty)) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                    (node, VarType::Unknown)
                },
            };
            let (lhs, lty) = match node_type(lhs, Some(rty.clone())) {
                (node, Ok(ty)) => (node, ty),
                (node, Err(e)) => {
                    e.lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .send()
                    .unwrap();
                    (node, VarType::Unknown)
                },
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
                    Err(err) =>  {
                        err.lines(pair.as_str())
                            .location(pair.as_span().start_pos().line_col().0)
                            .send()
                            .unwrap();
                        VarType::Unknown
                    },
                },
                op,
                vars_ty: lty, // left and right values have the same type
            }
        }
    )
}

fn parse_func_call(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.clone().into_inner();

    // get function's name
    let name = pairs.next().unwrap().as_str().to_string();
    // parse call's parameters
    let call_params = parse_parameters(pairs.next().unwrap());

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
            for (i, (arg_name, arg_ty)) in fn_args.iter().enumerate() {
                // get the type of the i-th function call parameter
                let call_param_ty = match call_params.get(i).cloned() {
                    Some(p) => match node_type(p, Some(arg_ty.clone())).1 {
                        Ok(ty) => ty,
                        Err(e) => {
                            e.lines(pair.as_str())
                             .location(pair.as_span().start_pos().line_col().0)
                             .send()
                             .unwrap();
                            VarType::Unknown
                        }
                    },
                    None => { // there are missing parameters
                        LogMesg::err()
                            .name("Missing parameters".into())
                            .cause(format!("Parameter {:?} {} is missing for function {}", 
                                           arg_ty, arg_name, name))
                            .lines(pair.as_str())
                            .location(pair.as_span().start_pos().line_col().0)
                            .send()
                            .unwrap();
                            continue;
                    },
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
        },
        Err(err) => {
            err.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
        },
    };
    
    AstNode::FunCall { name, params: call_params }
}

fn parse_parameters(pair: Pair<Rule>) -> Vec<AstNode> {
    let mut params = vec![];

    if pair.as_rule() == Rule::params {
        if let Some(p) = pair.into_inner().next() {
            return parse_parameters(p);
        }
    } else { // rule is `param`
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
        },
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
        },
    };

    let (rval, rty) = match node_type(rval, Some(lty.clone())) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(e)) => {
            e.lines(pair.as_str())
             .location(pair.as_span().start_pos().line_col().0)
             .send()
             .unwrap();
            (node, VarType::Unknown)
        },
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
    let value = pair.into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Int32(value.as_str().parse().unwrap()),
        Rule::id => AstNode::Identifyer(value.as_str().to_string()),
        Rule::boolean => AstNode::Boolean(value.as_str().parse().unwrap()),
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
        },
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
        },
    };

    // check if condition is boolean type
    if let Err(err) = expect_type(VarType::Boolean, &cond_ty) {
        err.lines(pair.as_str())
           .location(pair.as_span().start_pos().line_col().0)
           .send()
           .unwrap();
    }

    let mut stmts_list = match parse_stmts(inner.next().unwrap()) {
        AstNode::Stmts(list)  => list,
        _ => unreachable!(),
    };

    let mut loop_body = vec![AstNode::IfElseExpr {
        cond: Box::new(cond),
        true_b: Box::new(AstNode::Stmts(vec![])),
        false_b: Box::new(AstNode::Stmts(vec![AstNode::BreakExpr])),
    }];

    loop_body.append(&mut stmts_list);
    
    AstNode::LoopExpr(
        Box::new(
            AstNode::Stmts(loop_body)
        )
    ) 
}

pub fn print_fancy_parse_err(err: pest::error::Error<Rule>) {
    let (err_line, err_col) = match err.line_col {
        LineColLocation::Pos((lin, col)) => (format!("{}", lin), format!("{}", col)),
        LineColLocation::Span((lin1, lin2), (col1, col2)) => (
            format!("{}-{}", lin1, lin2), format!("{}-{}", col1, col2)),
    };
    eprintln!("[ERR] Syntax error in line: {}, col: {}",
                        err_line, err_col); 
    eprintln!("{}", err);
}

/// Checks if the left and right types are compatible considering the binary operator.
/// If types are not compatible, the function returns an error containing the name and 
/// the cause of the error.
fn binop_resolve_types(l: &VarType, r: &VarType, 
                       op: &BinaryOp) -> Result<VarType, LogMesg<String>> {
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
                .cause(format!("values of different types cannot be compared, left is {:?} and right is {:?}", l, r)))
        }
    } else { // arithmetic operations
        match (l, r) {
            (_, VarType::Unknown) => Ok(VarType::Unknown),
            (VarType::Unknown, _) => Ok(VarType::Unknown),
            (VarType::Int32, VarType::Int32) => Ok(VarType::Int32),
            (VarType::UInt32, VarType::UInt32) => Ok(VarType::UInt32),
            (VarType::Boolean, VarType::Boolean) => Err(
                LogMesg::err()
                    .name("Mismatched types".into())
                    .cause(format!("cannot apply operator {:?} to booleans", op))),
            _ => Err( 
                LogMesg::err()
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
                     .cause(format!("Cannot apply {:?} operator to {:?} type", op, ty)))
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
            .cause(format!("Expected {:?} type, got {:?} type instead", expected, ty))
        )
    }
}

/// Given an `AstNode` returns it's `VarType`. However, if there is an expected type for the node,
/// and the node is a literal value, automatic type conversions can be applied to tranform the
/// literal to the expected type value.
fn node_type(node: AstNode, expected: Option<VarType>) -> (AstNode, Result<VarType, LogMesg<String>>) {
    let node_ty = match get_node_type_no_autoconv(&node) {
        Ok(ty) => ty,
        Err(e) => return (node, Err(e)),
    };
    // if some type was expected and the node is a literal value, try to convert the literal to the
    // expected type. If conversion is not sucsessfull, return the original type of the node
    if let Some(expected) = expected {
        match expected {
            VarType::Int32 => match node {
                AstNode::UInt32(v) => match v.try_into() {
                        Ok(val) => (AstNode::Int32(val), Ok(VarType::Int32)),
                        Err(_) => (node, Ok(VarType::UInt32)), 
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt32 => match node {
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(VarType::UInt32)),
                    Err(_) => (node, Ok(VarType::Int32)),
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
        AstNode::BinaryExpr { expr_ty, ..} => Ok(expr_ty.clone()),
        AstNode::UnaryExpr { expr_ty, .. } => Ok(expr_ty.clone()),
        AstNode::Int32(_)   => Ok(VarType::Int32),
        AstNode::UInt32(_)  => Ok(VarType::UInt32),
        AstNode::Boolean(_) => Ok(VarType::Boolean),
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
        _ => {
            println!("Panic was caused by: {:?}", node);
            unreachable!();
        },
    }
}

