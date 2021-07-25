use pest::iterators::Pair;
use pest::{
    Parser,
    prec_climber::*,
    error::LineColLocation,
};
use pest::error::Error as PestErr;

use super::{VarType, LogMesg, MessageType};

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
                | Operator::new(eq, Left)
                | Operator::new(ne, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            // Operator::new(power, Right)
        ])
    };
}


#[derive(Debug, PartialEq)]
pub enum AstNode {
    FuncDecl { name: String, ret_type: Option<VarType>, 
        params: Vec<(String, VarType)>, stmts: Box<AstNode> },

    Stmts(Vec<AstNode>),

    // expressions
    VarDeclExpr { id: String, var_type: VarType, value: Box<AstNode> },
    BinaryExpr { left: Box<AstNode>, op: BinaryOp, right: Box<AstNode>},
    UnaryExpr  { op: UnaryOp, value: Box<AstNode> },
    AssignExpr { left: Box<AstNode>, right: Box<AstNode>},
    FunCall { name: String, params: Vec<AstNode> },
    IfElseExpr { cond: Box<AstNode>, true_b: Box<AstNode>, false_b: Box<AstNode> },
    ReturnExpr(Box<AstNode>),
    LoopExpr(Box<AstNode>), // contains an AstNode::Stmts variant inside
    BreakExpr,

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
    Ne,
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

pub fn parse(source: &str) -> Result<Vec<AstNode>, PestErr<Rule>> {
    let mut parsed = TestParser::parse(Rule::main, source)?;

    let mut main = parsed.next().unwrap() // get `main` rule
        .into_inner();

    let mut parsed = vec![];
    while let Some(pair) = main.next() {
        parsed.push(match pair.as_rule() {
            Rule::funcDecl => parse_func_decl(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        });
    }

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

    let stmts = Box::new(parse_stmts(next));

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
    // println!("expr: {:?}", expr.as_rule());
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
    let mut pairs = pair.into_inner();
    
    let var_type = parse_var_type(pairs.next().unwrap());

    let lhs = pairs.next().unwrap();
    let id = lhs.as_str().to_string();

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
                Rule::ne => BinaryOp::Ne,
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

/*fn parse_for_expr(pair: Pair<Rule>) -> AstNode {
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
*/

fn parse_ifelse_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();

    let cond_rule = inner.next().unwrap();
    let cond = parse_valued_expr(cond_rule);

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

fn parse_return_expr(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();
    let ret_value = parse_valued_expr(inner);
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
    let mut inner = pair.into_inner(); 

    let cond = parse_valued_expr(inner.next().unwrap());
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
