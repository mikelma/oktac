use console::style;
use pest::iterators::{Pair, Pairs};

use super::{parser::*, *};
use crate::{current_unit_st, LogMesg, VarType};

pub fn parse_enum_proto(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.clone().into_inner();

    // parse enum's name and visibility
    let next = inner.next().unwrap();
    let (visibility, name) = match next.as_rule() {
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        Rule::visibility => (
            misc::parse_visibility(next),
            inner.next().unwrap().as_str().to_string(),
        ),
        _ => unreachable!(),
    };

    let mut variants = vec![];
    let mut is_simple = true;

    for rule in inner {
        let mut variant_rule = rule.into_inner();

        let variant_id = variant_rule.next().unwrap().as_str();

        let mut fields = vec![];

        // check if another variant with the same name exists
        if variants
            .iter()
            .find(|(other, _)| *other == variant_id)
            .is_some()
        {
            LogMesg::err()
                .name("Invalid name")
                .cause(format!(
                    "Enum {} contains multiple variants with the name {}",
                    style(&name).bold(),
                    style(variant_id).italic()
                ))
                .help("Consider changing the name of the repeated variant".into())
                .location(pair_loc)
                .lines(pair_str)
                .send()
                .unwrap();
        } else {
            // parse the fields of the variant (if some)

            if let Some(fields_rule) = variant_rule.next() {
                is_simple = false;

                let mut inner = fields_rule.into_inner();

                while let Some(field_rule) = inner.next() {
                    let mut field = field_rule.into_inner();

                    // get the type of the field
                    let field_ty = ty::parse_var_type(field.next().unwrap()).unwrap_or_else(|e| {
                        e.lines(pair_str).location(pair_loc).send().unwrap();
                        VarType::Unknown
                    });

                    let field_name = field.next().unwrap().as_str();

                    /*
                    // extract possible any dependency from the field type
                    if let Some(ref dep) = strct::extract_dependency_from_ty(&field_ty) {
                        // check if the type of the field is the enum we are parsing (check if it is recursive)
                        if dep == &name {
                            LogMesg::err()
                                .name("Recursive type")
                                .cause(
                                    format!(
                                        "Field {} of variant {} of enum {} is recursive",
                                        style(field_name).italic().bold(),
                                        style(variant_id).italic().bold(),
                                        style(&name).italic().bold()
                                    )
                                )
                                .help(
                                    format!(
                                        "Consider encapsulating variant {}\
                                            \n* NOTE: This feature is not implemented yet!",
                                        style(variant_id).italic().bold(),
                                    )
                                )
                                .location(pair_loc)
                                .lines(pair_str)
                                .send()
                                .unwrap();
                        } else {
                            deps.push(dep.to_string());
                        }
                    }
                    */

                    // check if a field with the same name exists in the variant
                    if fields
                        .iter()
                        .find(|(other, _)| *other == field_name)
                        .is_some()
                    {
                        LogMesg::err()
                            .name("Invalid name")
                            .cause(format!(
                                "Field {} of enum {} contains multiple variants with the name {}",
                                style(variant_id).italic(),
                                style(&name).bold(),
                                style(field_name).bold()
                            ))
                            .help("Consider changing the name of the repeated field".into())
                            .location(pair_loc)
                            .lines(pair_str)
                            .send()
                            .unwrap();
                    } else {
                        fields.push((field_name.to_string(), field_ty));
                    }
                }
            }
        }
        variants.push((variant_id.to_string(), fields));
    }

    if let Err(e) = current_unit_st!().record_enum(&name, variants.clone(), visibility.clone()) {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::EnumProto {
        name,
        visibility,
        variants,
        is_simple,
    }
}

pub fn parse_enum_value(pair: Pair<Rule>, unpacking: bool) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut inner = pair.into_inner();

    let enum_name = inner.next().unwrap().as_str().to_string();
    let variant_name = inner.next().unwrap().as_str().to_string();

    let enum_info = current_unit_st!().search_enum_variant(&enum_name, &variant_name);
    // check if the enum type exists
    let (tag, true_members) = match enum_info {
        Ok(Some((tag, membs))) => (tag, membs),
        // the struct definition had an error, so return a default struct value
        other => {
            if let Err(e) = other {
                e.lines(pair_str).location(pair_loc).send().unwrap();
            }
            return AstNode::EnumVariant {
                enum_name,
                variant_name,
                tag: 0,
                fields: vec![],
                is_const: false,
            };
        }
    };

    let fields = parse_enum_var_fields(
        inner,
        &variant_name,
        true_members,
        unpacking,
        pair_str,
        pair_loc,
    );

    AstNode::EnumVariant {
        enum_name,
        variant_name,
        tag,
        fields,
        is_const: false,
    }
}

pub fn parse_enum_var_fields(
    pairs: Pairs<Rule>,
    variant_name: &str,
    true_fields: Vec<(String, VarType)>,
    unpackig: bool,
    pair_str: &str,
    pair_loc: usize,
) -> Vec<(usize, VarType, AstNode)> {
    let fields = pairs
        .map(|pair| {
            let mut field_rule = pair.into_inner();

            let field_name = field_rule.next().unwrap().as_str().to_string();

            // extract the "true" type of this field (extracted from the enum definition)
            let (index, true_ty) = match true_fields
                .iter()
                .enumerate()
                .find(|(_, (name, _))| *name == field_name)
                .map(|(i, (_, v))| (i, v.clone()))
            {
                Some(v) => v,
                None => {
                    LogMesg::err()
                        .name("Wrong field")
                        .cause(format!(
                            "Field {} does not exist in enum variant {}",
                            style(&field_name).italic(),
                            style(&variant_name).bold()
                        ))
                        .help(format!(
                            "Remove field {} from enum variant {}",
                            style(&field_name).italic(),
                            style(&variant_name).bold()
                        ))
                        .location(pair_loc)
                        .lines(pair_str)
                        .send()
                        .unwrap();
                    (0, VarType::Unknown)
                }
            };

            let value = if !unpackig {
                let value = expr::parse_expr(field_rule.next().unwrap());
                let (value, ty) = check::node_type(value, Some(true_ty.clone()));

                let ty = match ty {
                    Ok(t) => t,
                    Err(e) => {
                        e.location(pair_loc).lines(pair_str).send().unwrap();
                        VarType::Unknown
                    }
                };

                // check if the member type is correct
                if let Err(e) = check::expect_type(true_ty.clone(), &ty) {
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                }

                value
            } else {
                let val_pair = field_rule.next().unwrap().into_inner().next().unwrap();

                if val_pair.as_rule() == Rule::id {
                    AstNode::Identifyer(val_pair.as_str().into())
                } else {
                    LogMesg::err()
                        .name("Invalid value")
                        .cause("Field must be assigned to an identifier".into())
                        .help(format!(
                            "Try assigning field {} to an identifier. {}={} for example.",
                            field_name, field_name, field_name
                        ))
                        .location(pair_loc)
                        .lines(pair_str)
                        .send()
                        .unwrap();

                    AstNode::Identifyer("".into())
                }
            };

            (index, field_name, true_ty, value)
        })
        .collect::<Vec<(usize, String, VarType, AstNode)>>();

    if !unpackig {
        let missing = true_fields
            .iter()
            .filter(|(name, _)| {
                fields
                    .iter()
                    .find(|(_, fname, _, _)| fname == name)
                    .is_none()
            })
            .map(|(n, _)| n.as_str())
            .collect::<Vec<&str>>();

        if !missing.is_empty() {
            LogMesg::err()
                .name("Missing members")
                .cause(format!(
                    "Members for enum variant {} are missing",
                    style(&variant_name).bold()
                ))
                .help(format!(
                    "Consider adding the following members to {}: {}",
                    variant_name,
                    missing.join(", ")
                ))
                .lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
        }
    }

    // check if fields are repeated
    let mut repeated = vec![];
    fields.iter().for_each(|(_, a, _, _)| {
        if fields.iter().filter(|(_, b, _, _)| a == b).count() > 1 && !repeated.contains(a) {
            repeated.push(a.to_string());
        }
    });

    if !repeated.is_empty() {
        LogMesg::err()
            .name("Invalid name")
            .cause(format!(
                "Enum {} contains multiple fields with names: {}",
                style(&variant_name).bold(),
                repeated.join(", ")
            ))
            .help("Consider removing the name of the repeated fields".into())
            .location(pair_loc)
            .lines(pair_str)
            .send()
            .unwrap();
    }

    fields
        .iter()
        .map(|(index, _, ty, val)| (*index, ty.clone(), val.clone()))
        .collect()
}
