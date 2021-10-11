use pest::iterators::Pair;
use console::style;

use super::{parser::*, *};
use crate::{VarType, ST, LogMesg};

pub fn parse_struct_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    let next = inner.next().unwrap();
    let (visibility, name) = match next.as_rule() {
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        Rule::visibility => (misc::parse_visibility(next), 
                             inner.next().unwrap().as_str().to_string()),
        _ => unreachable!(),
    };

    let members = inner.map(|p| {
        let mut param = p.into_inner();

        /*let ty = match ty::parse_var_type(param.next().unwrap()) {
            Ok(t) => t,
            Err(_) => VarType::Unknown,
        };*/

        let ty_pair = param.next().unwrap()
            .into_inner().next().unwrap();
        let res = match ty_pair.as_rule() {
            Rule::simpleType => Ok(match ty::parse_simple_ty(ty_pair.clone()) {
                Ok(t) => t,
                Err(_) => VarType::Struct(ty_pair.as_str().into()),
            }),
            Rule::arrayType => ty::parse_array_ty(ty_pair),
            Rule::refType => ty::parse_ref_ty(ty_pair),
            _ => unreachable!(),
        };
        let ty = match res {
            Ok(t) => t,
            Err(e) => {
                e.lines(pair_str).location(pair_loc).send().unwrap();
                VarType::Unknown
            },
        };

        let id = param.next().unwrap().as_str();    

        (id.into(), ty)

    }).collect::<Vec<(String, VarType)>>();

    AstNode::StructProto { name, visibility, members }
}

pub fn parse_struct_value(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let struct_name = inner.next().unwrap().as_str().to_string();

    // check if the struct type exists
    let true_members = match ST.lock().unwrap().search_struct(&struct_name) {
        Ok(Some(m)) => Some(m),
        // the struct definition had an error, so return a default struct value
        Ok(None) => return AstNode::Strct {
            name: struct_name,
            members: vec![],
            is_const: false,
        },
        Err(e) => {
            e.lines(pair.as_str())
             .location(pair.as_span().start_pos().line_col().0)
             .lines(pair.as_str())
             .send()
             .unwrap();
            None
        },
    };

    // pase member of the struct
    let unordered_members = inner.map(|m| {
        let mut member = m.into_inner();

        let memb_name = member.next().unwrap().as_str().to_string();

        // extract the "true" type of this member (extracted from the struct definition)
        let true_ty = match &true_members {
            Some(membs) => membs.iter().find(|(name, _)| *name == memb_name).map(|v| v.1.clone()),
            None => None,
        };

        // check if the member really exits in the struct definition
        if true_ty.is_none() {
            LogMesg::err()
                .name("Wrong member")
                .cause(format!("Member {} does not exist in {}", 
                               style(&memb_name).italic(), 
                               style(&struct_name).bold())
                       .as_str())
                .help(format!("Remove member {} from {} intiliazation",
                               style(&memb_name).italic(), 
                               style(&struct_name).bold())
                      .as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .lines(pair.as_str())
                .send()
                .unwrap();
        }

        let value = expr::parse_expr(member.next().unwrap()); 
        let (value, ty) = check::node_type(value, true_ty.clone());

        let ty = match ty {
            Ok(t) => t,
            Err(e) => {
                e.location(pair.as_span().start_pos().line_col().0)
                 .lines(pair.as_str())
                 .send()
                 .unwrap();
                VarType::Unknown
            },
        };

        // check if the member type is correct 
        if let Some(tty) = true_ty {
            if let Err(e) = check::expect_type(tty, &ty) {
                e.location(pair.as_span().start_pos().line_col().0)
                 .lines(pair.as_str())
                 .send()
                 .unwrap();
            }
        }

        (memb_name, value)
    }).collect::<Vec<(String, AstNode)>>();

    // reoreder the members list following the true order of the members
    let mut members = vec![];
    if let Some(true_membs) = true_members {
        let mut missing_members = vec![];
        for (tm_name, _)in &true_membs  {
             match unordered_members.iter().position(|(name, _)| name == tm_name) {
                Some(index) => members.push(unordered_members[index].clone()),
                None => missing_members.push(tm_name.as_str()),
             }
        }

        if !missing_members.is_empty() {
            let missing_str = missing_members.iter()
                .map(|m| format!("{}", style(m).italic()))
                .collect::<Vec<String>>()
                .join(", ");

            LogMesg::err()            
                .name("Missing members")
                .cause(format!("Members of {} struct are missing", 
                               style(&struct_name).bold()).as_str())
                .help(format!("Consider adding the following members to struct initialization: {}", 
                              missing_str).as_str())
                .lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
        }
    }

    let is_const = members.iter().all(|(_, m)| m.is_const());

    AstNode::Strct { name: struct_name, members, is_const } 
}

pub fn parse_strct_member(parent_ty: VarType, member_name: &str, pair_str: &str, pair_loc: usize) -> (usize, VarType) {
    let def_ret = (0, VarType::Unknown); // default result returned if any error occurs

    let parent_name = match parent_ty {
        VarType::Unknown => return def_ret,
        VarType::Struct(name) => name,
        other => { // the parent node must be a struct
            LogMesg::err()
                .name("Invalid operation")
                .cause(format!("Cannot access member of a non struct type {:?}", other).as_str())
                .location(pair_loc)
                .lines(pair_str)
                .send()
                .unwrap();
            return def_ret;
        },
    };

    match ST.lock().unwrap().struct_member(&parent_name, member_name) {
        Ok(Some(v)) => v,
        Ok(None) => def_ret,
        Err(e) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            def_ret
        },
    }
}

/// Returns a list with the names of the structs the given struct node depends on. 
pub fn struct_deps(node: &AstNode) -> Vec<String> {
    if let AstNode::StructProto { members, .. }  = node {
        let mut deps = vec![];
        for (_, ty) in members {
            if let VarType::Struct(name) = ty {
                deps.push(name.clone());
            }
        }
        deps
        
    } else {
        unreachable!();
    }
}

// Returns true if the type `ty` is an struct named as `name`.
// pub fn member_is_struct(memb_name: &str, node: &AstNode) -> bool {
//     match node {
//         AstNode::Array { ty, ..} => ty,
//         AstNode::Strct { name } => memb_name == name,
//         _ => false,
//     }
// }
