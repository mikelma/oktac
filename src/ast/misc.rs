use console::style;
use pest::iterators::Pair;
use petgraph::Graph;

use super::{parser::*, *};
use crate::{current_unit_st, current_unit_status, CompUnitStatus, LogMesg, VarType};

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

pub fn parse_visibility(pair: Pair<Rule>) -> Visibility {
    match pair.as_str() {
        "pub" => Visibility::Pub,
        _ => unreachable!(),
    }
}

pub fn parse_const_var_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();
    let (vis, id) = match next.as_rule() {
        Rule::visibility => {
            let vis = parse_visibility(next);
            let id = pairs.next().unwrap().as_str().to_string();
            (vis, id)
        }
        // Rule::id
        _ => (Visibility::Priv, next.as_str().to_string()),
    };

    let mut ty = ty::parse_ty_or_default(pairs.next().unwrap(), Some((pair_str, pair_loc)));

    if let Err(e) = current_unit_st!().record_const_var(&id, ty.clone(), vis.clone()) {
        e.lines(pair_str).location(pair_loc).send().unwrap();
        ty = VarType::Unknown;
    }

    AstNode::ConstVarProto {
        name: id.clone(),
        visibility: vis.clone(),
        ty: ty.clone(),
    }
}

fn const_var_deps(node: &AstNode, deps: &mut Vec<String>) -> Result<(), LogMesg> {
    match node {
        // ----- `expr` rule ----- //
        AstNode::BinaryExpr { left, right, .. } => {
            const_var_deps(left, deps)?;
            const_var_deps(right, deps)
        }
        AstNode::UnaryExpr { value, .. } => const_var_deps(value, deps),
        AstNode::FunCall { .. } => Err(LogMesg::err().name("Invalid constant initilizer").cause(
            "Constant values cannot be initialized \
                           with a function call"
                .to_string(),
        )),
        AstNode::MemberAccessExpr {
            parent, members, ..
        } => {
            const_var_deps(parent, deps)?;
            for member in members {
                match member {
                    MemberAccess::Index(value) => const_var_deps(value, deps)?,
                    MemberAccess::Range { start, end } => {
                        const_var_deps(start, deps)?;
                        if let Some(val) = end {
                            const_var_deps(val, deps)?;
                        }
                    }
                    MemberAccess::MemberId(_) => (),
                }
            }
            Ok(())
        }
        // ----- `value` rule ----- //
        AstNode::Identifyer(id) => {
            if !deps.contains(id) {
                deps.push(id.to_string());
            }
            Ok(())
        }
        AstNode::Array { values, .. } => {
            for val in values {
                const_var_deps(val, deps)?;
            }
            Ok(())
        }
        AstNode::Strct { .. } => Err(LogMesg::err()
            .name("Invalid constant initializer")
            .cause("Structs as constant initializers are not supported".into())),
        AstNode::EnumVariant { .. } => Err(LogMesg::err()
            .name("Invalid constant initializer")
            .cause("Enums as constant initializers are not supported".into())),
        // ----- rules that don't affect the dependecies list, such as `float`  ----- //
        _ => Ok(()),
    }
}

// TODO: Check if the constant value depends on itself, in other words handle
// reculrsive definitions with a proper error message
pub fn parse_const_var(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();
    let (vis, id) = match next.as_rule() {
        Rule::visibility => {
            let vis = parse_visibility(next);
            let id = pairs.next().unwrap().as_str().to_string();
            (vis, id)
        }
        // Rule::id
        _ => (Visibility::Priv, next.as_str().to_string()),
    };

    let mut ty = ty::parse_ty_or_default(pairs.next().unwrap(), Some((pair_str, pair_loc)));

    let value_pair = pairs.next().unwrap();
    let value = match value_pair.as_rule() {
        Rule::id => AstNode::Identifyer(value_pair.as_str().to_string()),
        _ => expr::parse_expr(value_pair),
    };

    // get the type of the right hand value
    let (value, val_ty) = match check::node_type(value, Some(ty.clone())) {
        (node, Ok(ty)) => (node, ty),
        (node, Err(err)) => {
            err.lines(pair_str).location(pair_loc).send().unwrap();
            (node, VarType::Unknown)
        }
    };

    // check if the defined type and the type of the right value match
    if let Err(err) = check::expect_type(ty.clone(), &val_ty) {
        err.lines(pair_str).location(pair_loc).send().unwrap();
        ty = VarType::Unknown;
    }

    let mut dependencies = vec![];
    if let Err(err) = const_var_deps(&value, &mut dependencies) {
        err.lines(pair_str).location(pair_loc).send().unwrap();
        ty = VarType::Unknown;
    }

    if dependencies.iter().find(|&v| *v == id).is_some() {
        LogMesg::err()
            .name("Invalid constant initializer")
            .cause(format!("Constant variable {} is recursive", id))
            .lines(pair_str)
            .location(pair_loc)
            .send()
            .unwrap();
    }

    // let res = current_unit_st!().record_const_var(&id, ty.clone(), vis.clone());

    // if let Err(err) = res {
    //     err.lines(pair_str)
    //        .location(pair_loc)
    //        .send()
    //        .unwrap();

    //     dbg!(&current_unit_st!());
    // }

    AstNode::ConstVarDecl {
        name: id,
        visibility: vis,
        value: Box::new(value),
        ty,
        dependencies,
    }
}

/// This function imports `AstNode::ConstVarDecl`s from imported units.
/// Then, the function topologically sorts all `ConstVarDecl` nodes.
/// This is a mandatory step efore codegen.
pub fn import_and_sort_consts(unit_consts: &mut Vec<Arc<AstNode>>) {
    // get references to the imported units
    // let imports = current_unit_status!().lock().unwrap().imports.clone();
    let imports = current_unit_status!()
        .lock()
        .unwrap()
        .imports
        .iter()
        .map(|(_, unit_arc)| Arc::clone(unit_arc))
        .collect::<Vec<Arc<Mutex<CompUnitStatus>>>>();

    for unit in imports {
        // for each imported unit
        // get all the public constant variables of the imported unit
        let mut pub_consts = unit
            .lock()
            .unwrap()
            .protos
            .iter()
            .filter(|&p| match &**p {
                AstNode::ConstVarDecl { visibility, .. } => *visibility == Visibility::Pub,
                _ => false,
            })
            .map(|p| Arc::clone(p))
            .collect::<Vec<Arc<AstNode>>>();

        unit_consts.append(&mut pub_consts);
    }

    let mut graph = Graph::<String, String>::new();
    let mut map = HashMap::new();

    // add all nodes
    for node in &*unit_consts {
        if let AstNode::ConstVarDecl { name, .. } = &**node {
            map.insert(name.to_string(), graph.add_node(name.to_string()));
        } else {
            unreachable!()
        }
    }

    // add all edges
    for node in &*unit_consts {
        if let AstNode::ConstVarDecl {
            name, dependencies, ..
        } = &**node
        {
            for dep in dependencies {
                let from_node = map.get(name).unwrap();
                let to_node = map.get(dep).unwrap();
                graph.add_edge(from_node.clone(), to_node.clone(), "".into());
            }
        } else {
            unreachable!()
        }
    }

    let mut ordered = vec![];

    match petgraph::algo::toposort(&graph, None) {
        Ok(order) => {
            for node_idx in order.iter().rev() {
                ordered.push(Arc::clone(&unit_consts[node_idx.index()]));
            }
        }
        Err(cycle) => {
            // there is a cycle in the graph
            let err_node = &graph[cycle.node_id()];
            LogMesg::err()
                .name("Cyclic dependency")
                .cause(format!(
                    "Cyclic dependency detected in constant variable {}",
                    style(err_node).italic()
                ))
                .send()
                .unwrap();
        }
    }

    *unit_consts = ordered;
}
