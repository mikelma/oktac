use pest::iterators::Pair;
use pest::{
    Parser,
    prec_climber::*,
    error::LineColLocation,
};
use pest::error::Error as PestErr;

use super::VarType;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TestParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(and, Left) | Operator::new(or, Left) ,
            Operator::new(lt, Left) 
                | Operator::new(gt, Left) 
                | Operator::new(leq, Left) 
                | Operator::new(geq, Left) 
                | Operator::new(eq, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            // Operator::new(power, Right)
        ])
    };
}


#[derive(Debug, PartialEq)]
pub enum AstNode {
    Stmts(Vec<AstNode>),

    // expressions
    VarDeclExpr { id: Box<AstNode>, var_type: VarType, value: Box<AstNode> },
    BinaryExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>},
    UnaryExpr  { op: UnaryOp, value: Box<AstNode> },
    AssignExpr { left: Box<AstNode>, right: Box<AstNode>},
    FunCall { name: String, params: Vec<AstNode> },
    IfElseExpr { cond: Box<AstNode>, true_b: Box<AstNode>, false_b: Box<AstNode> },
    ReturnExpr(Box<AstNode>),
    ForExpr { pattern: Box<AstNode>, iter: Iter, block: Box<AstNode> },

    // terminals
    Identifyer(String),
    Integer(i32),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Eq,
    Lt,
    Gt,
    Leq,
    Geq,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Iter {
    IntRange(i64, i64) 
}

pub fn parse(source: &str) -> Result<AstNode, PestErr<Rule>> {
    let mut parsed = TestParser::parse(Rule::main, source)?;

    let stmts = parsed.next().unwrap() // get `main` rule
        .into_inner().next().unwrap(); // get stmts from `main` rule

    // programs always starts with a stmts block
    Ok(parse_stmts(stmts))
}

pub fn parse_stmts(pair: Pair<Rule>) -> AstNode {
    let mut exprs = vec![];  // list of expressions inside the stmts block
    let mut stop_analyzing = false;
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                if stop_analyzing {
                    // do not analyze more expressions after return statement
                    eprintln!("[WARN] Statements after `ret` expression: Unreachable code.");
                    eprintln!("     |\n {}  |     {}\n     |\n", pair.as_span().start_pos().line_col().0, pair.as_str());
                    break;
                }
                let expr =  parse_expr(pair);
                if let AstNode::ReturnExpr(_) = expr {
                    stop_analyzing = true;
                    exprs.push(expr);
                } else {
                    exprs.push(expr);
                }
            }
            _ => unreachable!(),
        }
    }
    AstNode::Stmts(exprs)
}

fn parse_expr(pair: Pair<Rule>) -> AstNode {
    let expr = pair.into_inner().next().unwrap();
    // println!("expr: {:?}", expr.as_rule());
    match expr.as_rule() {
        Rule::varDeclExpr => parse_vardecl_expr(expr),
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::unaryExpr => parse_unary_expr(expr),
        Rule::funCallExpr => parse_func_call(expr),
        Rule::assignExpr => parse_assign_expr(expr),
        Rule::ifElseExpr => parse_ifelse_expr(expr),
        Rule::returnExpr => parse_return_expr(expr),
        Rule::value => parse_value(expr),
        Rule::forExpr => parse_for_expr(expr),
        _ => unimplemented!(),
    }
}

fn parse_valued_expr(pairs: Pair<Rule>) -> AstNode {
    match pairs.as_rule() {
        Rule::unaryExpr => parse_unary_expr(pairs),
        Rule::binaryExpr => parse_binary_expr(pairs),
        Rule::value => parse_value(pairs),
        _ => panic!("Expected valued expression"),
    }
}

fn parse_vardecl_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();
    
    let var_type = parse_var_type(pairs.next().unwrap());

    let lhs = pairs.next().unwrap();
    let id = Box::new(match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        _ => unreachable!(),
    });

    let value = Box::new(parse_valued_expr(pairs.next().unwrap()));

    AstNode::VarDeclExpr { id, var_type, value }
}

fn parse_var_type(pair: Pair<Rule>) -> VarType {
    match pair.as_str() {
        "i32" => VarType::Int32,
        "bool" => VarType::Boolean,
        _ => unreachable!(),
    }
}

fn parse_unary_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let op = match pairs.next().unwrap().as_rule() {
        Rule::not => UnaryOp::Not,
        _ => unreachable!(),
    };

    let value = parse_valued_expr(pairs.next().unwrap());

    AstNode::UnaryExpr {
        op,
        value: Box::new(value),
    }
}

fn parse_binary_expr(pair: Pair<Rule>) -> AstNode {
    PREC_CLIMBER.climb(
        pair.into_inner(),
        |pair: Pair<Rule>| parse_valued_expr(pair),
        |lhs: AstNode, operator: Pair<Rule>, rhs: AstNode| AstNode::BinaryExpr {
            left: Box::new(lhs),
            op: match operator.as_rule() {
                Rule::add => BinaryOp::Add,
                Rule::subtract => BinaryOp::Subtract,
                Rule::multiply => BinaryOp::Multiply,
                Rule::divide => BinaryOp::Divide,
                Rule::and => BinaryOp::And,
                Rule::or => BinaryOp::Or,
                Rule::eq => BinaryOp::Eq,
                Rule::lt => BinaryOp::Lt,
                Rule::gt => BinaryOp::Gt,
                Rule::leq => BinaryOp::Leq,
                Rule::geq => BinaryOp::Geq,
                _ => unreachable!(),
            },
            right: Box::new(rhs),
        }
    )
}

fn parse_func_call(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();
     
    // get function's name
    let name = pairs.next().unwrap().as_str().to_string();
    // parse parameters
    let params = parse_parameters(pairs.next().unwrap());
    
    AstNode::FunCall { name, params }
}

fn parse_parameters(pair: Pair<Rule>) -> Vec<AstNode> {
    let mut pairs = pair.into_inner();
    let mut params = vec![];
    while let Some(rule) = pairs.next() {
        if Rule::params == rule.as_rule() {
            // parse other parameters
            let mut others = parse_parameters(rule);
            params.append(&mut others);

        } else { // else, the (rule) parameter is a valued expression
            params.push(parse_valued_expr(rule));
        }
    }
    params
}

fn parse_for_expr(pair: Pair<Rule>) -> AstNode {
    // println!("{}", pair);
    let mut inner = pair.into_inner();

    let var = AstNode::Identifyer(inner.next().unwrap().as_str().to_string()); 

    let iter = parse_iterator(inner.next().unwrap());

    let stmts = parse_stmts(inner.next().unwrap());

    AstNode::ForExpr {
        pattern: Box::new(var),
        iter,
        block: Box::new(stmts), 
    }
}

fn parse_iterator(pair: Pair<Rule>) -> Iter {
    let iter = pair.into_inner().next().unwrap();
    match iter.as_rule() {
        Rule::intRange => parse_int_range(iter),
        _ => unimplemented!(),
    }
}

fn parse_int_range(pair: Pair<Rule>) -> Iter {
    let mut inner = pair.into_inner();
    let start = inner.next().unwrap().as_str().parse().expect("Cannot parse range start integer"); 
    let end = inner.next().unwrap().as_str().parse().expect("Cannot parse range end integer"); 
    Iter::IntRange(start, end) 
}

fn parse_ifelse_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();

    let cond_rule = inner.next().unwrap();
    let cond = parse_valued_expr(cond_rule);
    // let cond = match cond_rule.as_rule() {
    //     Rule::mathExpr => parse_math_expr(cond_rule),
    //     Rule::value => parse_value(cond_rule),
    //     _ => unreachable!(),
    // };

    let true_b = parse_stmts(inner.next().unwrap());
    let false_b = parse_stmts(inner.next().unwrap());
    
    AstNode::IfElseExpr {
        cond: Box::new(cond),
        true_b: Box::new(true_b),
        false_b: Box::new(false_b),
    }
}


fn parse_assign_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        _ => unreachable!(),
    };

    let rhs = pairs.next().unwrap();
    let rval = parse_valued_expr(rhs);
    // let rval = match rhs.as_rule() {
    //     Rule::mathExpr => parse_math_expr(rhs),
    //     Rule::value => parse_value(rhs),
    //     _ => unreachable!(),
    // };

    AstNode::AssignExpr {
        left: Box::new(lval), 
        right: Box::new(rval),
    }
}


fn parse_value(pair: Pair<Rule>) -> AstNode {
    let value = pair.into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Integer(value.as_str().parse().unwrap()),
        Rule::id => AstNode::Identifyer(value.as_str().to_string()),
        Rule::boolean => AstNode::Boolean(value.as_str().parse().unwrap()),
        _ => unreachable!(),
    }
}

/*fn parse_math_op(pair: Pair<Rule>) -> MathOp {
    match pair.as_rule() {
        rule::add => mathop::add,
        rule::subtract => mathop::subtract,
        rule::multiply => mathop::multiply,
        rule::divide => mathop::divide,
        _ => unreachable!(),
    }
}*/

fn parse_return_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let ret_value = parse_valued_expr(inner);
    // let ret_value = match inner.as_rule() {
    //     Rule::mathExpr => parse_math_expr(inner),
    //     Rule::value => parse_value(inner),
    //     _ => unreachable!(),
    // };
    AstNode::ReturnExpr(Box::new(ret_value))
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
