use pest::iterators::Pair;
use pest::{
    Parser,
    prec_climber::*,
    error::LineColLocation,
};

use pest::error::Error as PestErr;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TestParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
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
    MathExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>},
    AssignExpr { left: Box<AstNode>, right: Box<AstNode>},
    PrintExpr(Box<AstNode>),
    IfElseExpr { cond: Box<AstNode>, true_b: Box<AstNode>, false_b: Box<AstNode> },
    ReturnExpr(Box<AstNode>),

    // terminals
    Identifyer(String),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                exprs.push(parse_expr(pair));
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
        Rule::mathExpr => parse_math_expr(expr),
        Rule::printExpr => parse_print_expr(expr),
        Rule::assignExpr => parse_assign_expr(expr),
        Rule::ifElseExpr => parse_ifelse_expr(expr),
        Rule::returnExpr => parse_return_expr(expr),
        Rule::value => parse_value(expr),
        _ => unimplemented!(),
    }
}

fn parse_ifelse_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();
    let cond_rule = inner.next().unwrap();
    let cond = match cond_rule.as_rule() {
        Rule::mathExpr => parse_math_expr(cond_rule),
        Rule::value => parse_value(cond_rule),
        _ => unreachable!(),
    };

    let true_b = parse_stmts(inner.next().unwrap());
    let false_b = parse_stmts(inner.next().unwrap());
    
    AstNode::IfElseExpr {
        cond: Box::new(cond),
        true_b: Box::new(true_b),
        false_b: Box::new(false_b),
    }
}

fn parse_math_expr(pair: Pair<Rule>) -> AstNode {
    PREC_CLIMBER.climb(
        pair.into_inner(),
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::value => parse_value(pair),
            Rule::mathExpr => parse_math_expr(pair),
            _ => unreachable!(),
        },
        |lhs: AstNode, operator: Pair<Rule>, rhs: AstNode| AstNode::MathExpr {
            left: Box::new(lhs),
            op: parse_math_op(operator),
            right: Box::new(rhs),
        }
    )
}

fn parse_assign_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();

    let lhs = pairs.next().unwrap();
    let lval = match lhs.as_rule() {
        Rule::id => AstNode::Identifyer(lhs.as_str().to_string()),
        _ => unreachable!(),
    };

    let rhs = pairs.next().unwrap();
    let rval = match rhs.as_rule() {
        Rule::mathExpr => parse_math_expr(rhs),
        Rule::value => parse_value(rhs),
        _ => unreachable!(),
    };

    AstNode::AssignExpr {
        left: Box::new(lval), 
        right: Box::new(rval),
    }
}

fn parse_print_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let inner_val = match inner.as_rule() {
        Rule::mathExpr => parse_math_expr(inner),
        Rule::value => parse_value(inner),
        _ => unreachable!(),
    };
    AstNode::PrintExpr(Box::new(inner_val))
}

fn parse_value(pair: Pair<Rule>) -> AstNode {
    let value = pair.into_inner().next().unwrap();
    match value.as_rule() {
        Rule::number => AstNode::Integer(value.as_str().parse().unwrap()),
        Rule::id => AstNode::Identifyer(value.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn parse_math_op(pair: Pair<Rule>) -> BinaryOp {
    match pair.as_rule() {
        Rule::add => BinaryOp::Add,
        Rule::subtract => BinaryOp::Subtract,
        Rule::multiply => BinaryOp::Multiply,
        Rule::divide => BinaryOp::Divide,
        _ => unreachable!(),
    }
}

fn parse_return_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let ret_value = match inner.as_rule() {
        Rule::mathExpr => parse_math_expr(inner),
        Rule::value => parse_value(inner),
        _ => unreachable!(),
    };
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
