use pest::iterators::Pair;
use pest::iterators::*;
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
    MathExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>},
    AssignExpr { left: Box<AstNode>, right: Box<AstNode>},
    PrintExpr(Box<AstNode>),

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

pub fn parse(source: &str) -> Result<Vec<AstNode>, PestErr<Rule>> {
    let mut parsed = TestParser::parse(Rule::main, source)?;

    let pairs = parsed.next().unwrap(); // as there is only a single main

    let mut ast = vec![];

    for pair in pairs.into_inner() {
        // main rule into inner
        match pair.as_rule() {
            Rule::expr => {
                ast.push(parse_expr(pair));
            }
            _ => (),
        }
    }
    Ok(ast)
}

fn parse_expr(pair: Pair<Rule>) -> AstNode {
    let expr = pair.into_inner().next().unwrap();

    // println!("Parsing `{}` as {:?}", expr.as_str(), expr.as_rule());
    match expr.as_rule() {
        Rule::mathExpr => parse_math_expr(expr),
        Rule::printExpr => parse_print_expr(expr),
        Rule::assignExpr => parse_assign_expr(expr),
        _ => unimplemented!(),
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
