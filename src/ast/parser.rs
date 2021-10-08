use pest::error::Error as PestErr;
use pest::error::LineColLocation;
use pest::Parser;
use pest::iterators::{Pairs, Pair};
use petgraph::Graph;
use petgraph::graph::NodeIndex;

use std::collections::HashMap;

use super::*;
use crate::{ST, LogMesg};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct PestParser;

pub fn parse(source: &str) -> Result<(Vec<AstNode>, AstNode), PestErr<Rule>> {
    let mut parsed = PestParser::parse(Rule::main, source)?;

    let main = parsed
        .next()
        .unwrap() // get `main` rule
        .into_inner();
    //dbg!(&main);

    // create a table for the modules scope
    ST.lock().unwrap().push_table();
    
    // AST generation passes
    let protos = first_pass(main.clone());
    let ast = main_pass(main);

    // destroy the table for the main scope
    ST.lock().unwrap().pop_table();

    Ok((protos, ast))
}

/// Iterates over all the definitions in the `main` rule, pushing all the prototype definitions to
/// the symbol table of the module. Running this step before the main pass is mandatory, as
/// prototype definitions must be in the symbol table before converting all the parsed tree into an
/// AST, mainly for error checking and symbol declaration order invariance.
fn first_pass(main_pairs: Pairs<Rule>) -> Vec<AstNode> {

    let mut func_pairs = vec![];
    let mut strct_pairs = vec![];

    // parse all prototypes
    for pair in main_pairs {
        match pair.as_rule() {
            Rule::funcDecl | Rule::externFunc => func_pairs.push(pair),
            Rule::structDef => strct_pairs.push(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        }
    }

    let mut protos = parse_struct_protos(strct_pairs);
    let mut func_protos = parse_fun_protos(func_pairs);

    protos.append(&mut func_protos);

    // dbg!(&ST.lock().unwrap()); // show the symbol table

    protos
}

fn parse_fun_protos(pairs: Vec<Pair<Rule>>) -> Vec<AstNode> {
    let mut protos = vec![];
    for pair in pairs {
        protos.push(match pair.as_rule() {
            Rule::funcDecl => func::parse_func_proto(pair),
            Rule::externFunc => func::parse_extern_func_proto(pair),
            _ => unreachable!(),
        });
    }
    protos
}

fn parse_struct_protos(pairs: Vec<Pair<Rule>>) -> Vec<AstNode> {
    let mut graph = Graph::<String, String>::new(); // TODO: Try to replace with <&str, &str>

    let mut strct_protos = HashMap::new();
    let mut named_edges = vec![];
    let mut node_idx = HashMap::new();
    let mut strct_pair_info = HashMap::new();

    // parse all prototypes
    for pair in pairs {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;

        let proto = strct::parse_struct_proto(pair);

        let name = match &proto {
            AstNode::StructProto {name, ..} => name.to_string(),
            _ => unreachable!(),
        };

        node_idx.insert(name.clone(), graph.add_node(name.clone()));
        strct_pair_info.insert(name.clone(), (pair_str, pair_loc));

        strct::struct_deps(&proto)
            .iter()
            .for_each(|d| named_edges.push((d.clone(), name.clone())));

        strct_protos.insert(name, proto);
    }

    let mut edges = vec![];
    for (a, b) in named_edges.iter() {
        // this never fails as `b` is always in the `node_idx` hashmap
        let idx_b = node_idx.get(b).unwrap().clone();

        match node_idx.get(a) {
            Some(idx_a) => edges.push((idx_a.clone(), idx_b)),
            None => {
                let (pair_str, pair_loc) = strct_pair_info.get(b).unwrap();
                LogMesg::err()
                    .name("Undefined type")
                    .cause(format!("Type {} is not declared in the current scope", a).as_str())
                    .lines(pair_str)
                    .location(*pair_loc)
                    .send().unwrap();
            }
        };
    }

    graph.extend_with_edges(&edges);

    let ordered = petgraph::algo::toposort(&graph, None).unwrap(); 

    let protos = ordered.iter()
        .map(|idx| strct_protos.remove(&graph[*idx]).unwrap())
        .collect::<Vec<AstNode>>();

    // register the structs in the correct order in the symbol table
    for p in &protos {
        if let AstNode::StructProto { name, members, visibility } = p {
            let res = ST
                .lock()
                .unwrap()
                .record_struct(&name, members.clone(), visibility.clone());

            if let Err(e) = res {
                e
                // TODO
                // .lines(pair.as_str())
                // .location(pair.as_span().start_pos().line_col().0)
                .send().unwrap();
            }
        } else { unreachable!(); }
    }

    protos
}

fn main_pass(main_pairs: Pairs<Rule>) -> AstNode {
    // panic!("{:#?}", main_pairs);
    let mut subtrees = vec![];
    for pair in main_pairs {
        if let Rule::funcDecl = pair.as_rule() {
            subtrees.push(func::parse_func_decl(pair));
        }
    }

    // all modules start with a stmts block, in other words, 
    // the root node of all AST's is the `AstNode::Stmts` node
    AstNode::Stmts(subtrees)
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
