use console::style;
use pest::iterators::Pair;
use petgraph::Graph;

use super::{parser::*, *};
use crate::*;

use std::{collections::HashMap, sync::Arc};

pub fn parse_const_var(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut pairs = pair.into_inner();

    let next = pairs.next().unwrap();
    let (vis, id) = match next.as_rule() {
        Rule::visibility => {
            let vis = misc::parse_visibility(next);
            let id = pairs.next().unwrap().as_str().to_string();
            (vis, id)
        }
        // Rule::id
        _ => (Visibility::Priv, next.as_str().to_string()),
    };

    let mut ty = ty::parse_ty_or_default(pairs.next().unwrap(), Some((pair_str, pair_loc)));

    let value_pair = pairs.next().unwrap();
    let value = match value_pair.as_rule() {
        Rule::id => AstNode::Identifier(value_pair.as_str().to_string()),
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

    // get all the ids of other constants values that this constant value depends on
    let mut dependencies = vec![];
    if let Err(err) = const_var_deps(&value, &mut dependencies) {
        err.lines(pair_str).location(pair_loc).send().unwrap();
        ty = VarType::Unknown;
    }

    // check if the value depends on itself
    if dependencies.iter().find(|&v| *v == id).is_some() {
        LogMesg::err()
            .name("Invalid constant initializer")
            .cause(format!("Constant variable {} is recursive", id))
            .lines(pair_str)
            .location(pair_loc)
            .send()
            .unwrap();
    }

    AstNode::ConstVarDecl {
        name: id,
        visibility: vis,
        value: Box::new(value),
        ty,
        dependencies,
    }
}

/// The function topologically sorts all `ConstVarDecl` nodes from the
/// current compilation unit's prototypes list (referred to as `protos`).
///
/// This function is needed to ensure that constant variables are
/// processed by `Codegen` in the correct order with respect their
/// dependencies to other constant variables. Thus, it is a mandatory
/// before the codegen process.
pub fn toposort_const_vars() {
    // from the list of all prototypes, get the ones that refer to constant values
    let mut consts = vec![]; // list of: constant variable AstNodes
    let mut consts_info = vec![]; // list of: (const's name, const's dependencies)

    let num_protos = current_unit_protos!().lock().unwrap().len();

    // remove all nodes of constant values from prototypes list
    for i in 0..num_protos {
        let index = i - consts.len();
        let node = Arc::clone(&current_unit_protos!().lock().unwrap()[index]);
        if let AstNode::ConstVarDecl {
            name, dependencies, ..
        } = &*node
        {
            consts_info.push((name.to_string(), dependencies.clone()));
            // consts.push(Arc::clone(
            //     &current_unit_protos!().lock().unwrap().remove(index),
            // ));
            consts.push(current_unit_protos!().lock().unwrap().remove(index));
        }
    }

    let mut graph = Graph::<String, String>::new();
    let mut map = HashMap::new(); // map of: name -> graph index

    // add all nodes
    for (name, _) in &consts_info {
        let graph_idx = graph.add_node(name.to_string());
        map.insert(name.to_string(), graph_idx);
    }

    // add all edges
    for (name, dependencies) in &consts_info {
        for dep in dependencies {
            let from_node = map.get(name).unwrap();
            let to_node = match map.get(dep) {
                Some(d) => d,
                None => {
                    // constant variable dependency `dep` does not exist in the unit's scope.

                    // If this line gets executed, no error has to be thrown, as the error is
                    // already registered when the right hand side expression of the constant
                    // variable declaration `name` is parsed.

                    // If an error was thrown here, modules that import a module containing
                    // a public constant with an invalid dependency, will also throw a repeated
                    // error
                    continue; // just skip
                }
            };
            graph.add_edge(from_node.clone(), to_node.clone(), "".into());
        }
    }

    match petgraph::algo::toposort(&graph, None) {
        Ok(order) => {
            for index in order.iter().rev() {
                current_unit_protos!()
                    .lock()
                    .unwrap()
                    .push(Arc::clone(consts.get(index.index()).unwrap()));
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
        AstNode::Identifier(id) => {
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
