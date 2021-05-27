use pest::iterators::Pair;
use pest::iterators::*;
use pest::Parser;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    BinaryExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>},
    PrintExpr(Box<AstNode>),

    // terminals
    Identifyer(String),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Sum,
    Subtract,
    Assign,
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TestParser;

pub fn parse(source: &str) -> Result<Vec<AstNode>, &'static str> {
    let mut parsed = TestParser::parse(Rule::main, source).expect("unsuccessful parse"); // unwrap the parse result

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

    match expr.as_rule() {
        Rule::binaryExpr => parse_binary_expr(expr),
        Rule::printExpr  => parse_print_expr(expr),
        _ => unimplemented!(),
    }
}

fn parse_binary_expr(pair: Pair<Rule>) -> AstNode {
    let mut pairs = pair.into_inner();
    let lval = parse_value(pairs.next().unwrap());
    let op = parse_math_op(pairs.next().unwrap());
    let rval = parse_value(pairs.next().unwrap());

    AstNode::BinaryExpr {
        left: Box::new(lval), 
        op, 
        right: Box::new(rval),
    }
}

fn parse_print_expr(pair: Pair<Rule>) -> AstNode {
    let inner_val = parse_value(pair.into_inner().next().unwrap());
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
    match pair.into_inner().next().unwrap().as_rule() {
        Rule::plus => BinaryOp::Sum,
        Rule::minus => BinaryOp::Subtract,
        Rule::assig => BinaryOp::Assign,
        _ => unreachable!(),
    }
}
