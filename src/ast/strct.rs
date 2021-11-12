use console::style;
use pest::iterators::{Pair, Pairs};

use super::{parser::*, *};
use crate::{LogMesg, VarType, current_unit_st};

pub fn parse_struct_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    let next = inner.next().unwrap();
    let (visibility, name) = match next.as_rule() {
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        Rule::visibility => (
            misc::parse_visibility(next),
            inner.next().unwrap().as_str().to_string(),
        ),
        _ => unreachable!(),
    };

    // let mut deps = vec![];
    let mut members = vec![];

    for p in inner {
        let mut param = p.into_inner();

        let ty_pair = param.next().unwrap();
        let id = param.next().unwrap().as_str();

        let ty = if members.iter().find(|(other, _)| other == id).is_some() {
            // send an error if another member with the same name exists
            LogMesg::err()
                .name("Invalid name")
                .cause(
                    format!(
                        "Struct {} contains multiple members with the name {}",
                        style(&name).bold(),
                        style(id).italic()
                    )
                )
                .help("Consider changing the name of the repeated members".into())
                .location(pair_loc)
                .lines(pair_str)
                .send()
                .unwrap();

            VarType::Unknown
        } else {
            ty::parse_var_type(ty_pair).unwrap_or_else(|e| {
                e.lines(pair_str).location(pair_loc).send().unwrap();
                VarType::Unknown
            })
        };

        /*
        if let Some(dep) = extract_dependency_from_ty(&ty) {
            // check if the type of the member is te struct we are parsing (check if is recursive)
            if dep == name {
                LogMesg::err()
                    .name("Recursive type")
                    .cause(format!("Member {} of struct {} is recursive", 
                                style(id).italic().bold(),
                                style(&dep).italic().bold()))
                    .help(format!("Consider encapsulating member {} of {} \n* NOTE: This feature is not implemented yet!", 
                                style(id).italic().bold(),
                                style(dep).italic().bold()
                        ))
                    .location(pair_loc)
                    .lines(pair_str)
                    .send().unwrap();
            } else {
                deps.push(dep.to_string());
            }
        }
        */

        members.push((id.into(), ty));
    }

    if let Err(e) = current_unit_st!()
        .record_struct(
            &name, members.clone(), 
            visibility.clone()
    ) {
        e.lines(pair_str)
         .location(pair_loc)
         .send()
         .unwrap();
    }

    AstNode::StructProto {
        name,
        visibility,
        members,
    }
}

pub fn parse_struct_value(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut inner = pair.clone().into_inner();

    let struct_name = inner.next().unwrap().as_str().to_string();

    // check if the struct type exists
    let res = current_unit_st!().search_struct(&struct_name);
    let true_members = match res {
        Ok(Some(m)) => Some(m),
        // the struct definition had an error, so return a default struct value
        Ok(None) => {
            return AstNode::Strct {
                name: struct_name,
                members: vec![],
                is_const: false,
            }
        }
        Err(e) => {
            e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .lines(pair.as_str())
                .send()
                .unwrap();
            None
        }
    };

    let members = parse_strct_members(inner, &struct_name, true_members, pair_str, pair_loc);

    let members: Vec<(String, AstNode)> = members.iter().map(|(_, v)| v.clone()).collect();

    let is_const = members.iter().all(|(_, m)| m.is_const());

    AstNode::Strct {
        name: struct_name,
        members,
        is_const,
    }
}

// TODO: Remove `Option` for `true_members`.
pub fn parse_strct_members(
    pairs: Pairs<Rule>,
    struct_name: &str,
    true_members: Option<Vec<(String, VarType)>>,
    pair_str: &str,
    pair_loc: usize,
) -> Vec<(usize, (String, AstNode))> {
    // pase member of the struct
    let unordered_members = pairs
        .map(|m| {
            let mut member = m.into_inner();

            let memb_name = member.next().unwrap().as_str().to_string();

            // extract the "true" type of this member (extracted from the struct definition)
            let true_ty = match &true_members {
                Some(membs) => membs
                    .iter()
                    .find(|(name, _)| *name == memb_name)
                    .map(|v| v.1.clone()),
                None => None,
            };

            // check if the member really exits in the type definition
            if true_ty.is_none() {
                LogMesg::err()
                    .name("Wrong member")
                    .cause(
                        format!(
                            "Member {} does not exist in struct type {}",
                            style(&memb_name).italic(),
                            style(&struct_name).bold()
                        )
                    )
                    .help(
                        format!(
                            "Remove member {} from struct type {}",
                            style(&memb_name).italic(),
                            style(&struct_name).bold()
                        )
                    )
                    .location(pair_loc)
                    .lines(pair_str)
                    .send()
                    .unwrap();
            }

            let value = expr::parse_expr(member.next().unwrap());
            let (value, ty) = check::node_type(value, true_ty.clone());

            let ty = match ty {
                Ok(t) => t,
                Err(e) => {
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                    VarType::Unknown
                }
            };

            // check if the member type is correct
            if let Some(tty) = true_ty {
                if let Err(e) = check::expect_type(tty, &ty) {
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                }
            }

            (memb_name, value)
        })
        .collect::<Vec<(String, AstNode)>>();

    // check if members are repeated
    let mut repeated = vec![];
    unordered_members.iter().for_each(|(a, _)| {
        if unordered_members.iter().filter(|(b, _)| a == b).count() > 1 && !repeated.contains(a) {
            repeated.push(a.to_string());
        }
    });

    if !repeated.is_empty() {
        LogMesg::err()
            .name("Invalid name")
            .cause(
                format!(
                    "Struct {} contains multiple members with names: {}",
                    style(&struct_name).bold(),
                    repeated.join(", ")
                )
            )
            .help("Consider removing the name of the repeated members".into())
            .location(pair_loc)
            .lines(pair_str)
            .send()
            .unwrap();
    }

    // reoreder the members list following the true order of the members
    let mut members = vec![];
    if let Some(true_membs) = true_members {
        let mut missing_members = vec![];
        for (tm_name, _) in &true_membs {
            match unordered_members
                .iter()
                .position(|(name, _)| name == tm_name)
            {
                Some(index) => members.push((index, unordered_members[index].clone())),
                None => missing_members.push(tm_name.as_str()),
            }
        }

        if !missing_members.is_empty() {
            let missing_str = missing_members
                .iter()
                .map(|m| format!("{}", style(m).italic()))
                .collect::<Vec<String>>()
                .join(", ");

            LogMesg::err()
                .name("Missing members")
                .cause(
                    format!(
                        "Members for struct type {} are missing",
                        style(&struct_name).bold()
                    )
                )
                .help(format!("Consider adding the following members to {}", missing_str))
                .lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
        }
    }

    members
}

pub fn parse_strct_member_access(
    parent_ty: VarType,
    member_name: &str,
    pair_str: &str,
    pair_loc: usize,
) -> (usize, VarType) {
    let def_ret = (0, VarType::Unknown); // default result returned if any error occurs

    let parent_name = match parent_ty {
        VarType::Unknown => return def_ret,
        VarType::Struct(name) => name,
        other => {
            // the parent node must be a struct
            LogMesg::err()
                .name("Invalid operation")
                .cause(format!("Cannot access member of a non struct type {:?}", other))
                .location(pair_loc)
                .lines(pair_str)
                .send()
                .unwrap();
            return def_ret;
        }
    };

    match current_unit_st!().struct_member(&parent_name, member_name) {
        Ok(Some(v)) => v,
        Ok(None) => def_ret,
        Err(e) => {
            e.lines(pair_str).location(pair_loc).send().unwrap();
            def_ret
        }
    }
}

/*
pub fn extract_dependency_from_ty(ty: &VarType) -> Option<&str> {
    match ty {
        VarType::Struct(name) | VarType::Enum(name) => Some(name),
        VarType::Ref(inner) | VarType::Array { inner, .. } => extract_dependency_from_ty(inner),
        _ => None,
    }
}
*/
