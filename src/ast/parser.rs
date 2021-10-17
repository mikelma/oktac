use pest::error::Error as PestErr;
use pest::error::LineColLocation;
use pest::Parser;
use pest::iterators::{Pairs, Pair};

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
    let mut ty_pairs = vec![];

    // parse all prototypes
    for pair in main_pairs {
        match pair.as_rule() {
            Rule::funcDecl | Rule::externFunc => func_pairs.push(pair),
            Rule::structDef | Rule::enumDef => ty_pairs.push(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        }
    }

    let mut protos = parse_ty_protos(ty_pairs);
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

fn parse_ty_protos(pairs: Vec<Pair<Rule>>) -> Vec<AstNode> {
    // get the names of all type definitions in the module and register the 
    // types as opaque in the symbol table
    for pair in pairs.clone() {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;
        let pair_rule = pair.as_rule();

        let mut inner = pair.into_inner();
        let next = inner.next().unwrap();
        let name = match next.as_rule() {
            Rule::id => next,
            Rule::visibility => inner.next().unwrap(),
            _ => unreachable!(),
        }.as_str();
        
        let res = match pair_rule {
            Rule::structDef => ST.lock().unwrap().record_opaque_struct(name),
            Rule::enumDef => ST.lock().unwrap().record_opaque_enum(name),
            _ => unreachable!(),
        };

        if let Err(e) = res {
            e.location(pair_loc)
                .lines(pair_str)
                .send().unwrap();
            return vec![];
        };
    }

    // parse all prototypes 
    let mut protos = vec![];
    let mut dependencies: HashMap<String, Vec<String>> = HashMap::new();

    for pair in pairs {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;

        let (proto, deps) = match pair.as_rule() {
            Rule::structDef => strct::parse_struct_proto(pair),
            Rule::enumDef => ty_enum::parse_enum_proto(pair),
            _ => unreachable!(),
        };

        let name = match &proto {
            AstNode::StructProto { name, .. } | AstNode::EnumProto { name, .. } => name,
            _ => unreachable!(),
        };

        // check circular dependencies
        dependencies.iter()
            .filter(|(other, _)| deps.contains(other))
            .for_each(|(other, other_deps)| {
                if other_deps.iter().find(|d| *d == name).is_some() {
                    LogMesg::err()
                        .name("Circular dependency")
                        .cause(format!("Types {} and {} contain a circular dependency", 
                                        other, name).as_str())
                        .help("Consider encapsulating at least one of this types\
                                \n* NOTE: This feature is not implemented yet!")
                        .location(pair_loc)
                        .lines(pair_str)
                        .send().unwrap();
                }
        });

        dependencies.insert(name.to_string(), deps);

        let res = match &proto {
            AstNode::EnumProto {name, visibility, variants, is_simple } => {
                ST.lock()
                  .unwrap()
                  .record_enum(&name, 
                               variants.clone(), 
                               visibility.clone())
            },
            AstNode::StructProto {name, visibility, members } => {
                ST.lock()
                  .unwrap()
                  .record_struct(&name, 
                                 members.clone(), 
                                 visibility.clone())
            },
            _ => unreachable!(),
        };


        if let Err(e) = res {
            e.location(pair_loc).lines(pair_str).send().unwrap();
        }

        protos.push(proto);
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
