use pest::iterators::Pair;
use console::style;

use super::{parser::*, *};
use crate::{VarType, ST, LogMesg};

pub fn parse_enum_proto(pair: Pair<Rule>) -> (AstNode, Vec<String>) {
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

    let mut variants = vec![];
    let mut deps = vec![];
    let mut is_simple = true;

    for rule in inner {
        let mut variant_rule = rule.into_inner();

        let variant_id = variant_rule.next().unwrap().as_str();

        let mut fields = vec![];

        // check if another variant with the same name exists
        if variants.iter().find(|(other, _)| *other == variant_id).is_some() {
            LogMesg::err()
                .name("Invalid name")
                .cause(format!("Enum {} contains multiple variants with the name {}", 
                                style(&name).bold(), style(variant_id).italic()).as_str())
                .help("Consider changing the name of the repeated variant")
                .location(pair_loc)
                .lines(pair_str)
                .send().unwrap();

        } else { // parse the fields of the variant (if some)

            if let Some(fields_rule) = variant_rule.next() {
                is_simple = false;

                let mut inner = fields_rule.into_inner();

                while let Some(field_rule) = inner.next() {
                    let mut field = field_rule.into_inner();

                    // get the type of the field
                    let field_ty = ty::parse_var_type(field.next().unwrap()).unwrap_or_else(|e| {
                        e.lines(pair_str)
                        .location(pair_loc)
                        .send().unwrap();
                        VarType::Unknown
                    });

                    let field_name = field.next().unwrap().as_str();

                    // extract possible any dependency from the field type
                    if let Some(ref dep) = strct::extract_dependency_from_ty(&field_ty) {
                        // check if the type of the field is the enum we are parsing (check if it is recursive)
                        if dep == &name {
                            LogMesg::err()
                                .name("Recursive type")
                                .cause(format!("Field {} of variant {} of enum {} is recursive", 
                                            style(field_name).italic().bold(), 
                                            style(variant_id).italic().bold(), 
                                            style(&name).italic().bold()).as_str())
                                .help(format!("Consider encapsulating variant {}\
                                            \n* NOTE: This feature is not implemented yet!", 
                                            style(variant_id).italic().bold(),
                                    ).as_str())
                                .location(pair_loc)
                                .lines(pair_str)
                                .send().unwrap();
                        } else {
                            deps.push(dep.to_string());
                        }
                    }

                    // check if a field with the same name exists in the variant
                    if fields.iter().find(|(other, _)| *other == field_name).is_some() {
                        LogMesg::err()
                            .name("Invalid name")
                            .cause(format!("Field {} of enum {} contains multiple variants with the name {}", 
                                            style(variant_id).italic(), style(&name).bold(), style(field_name).bold()).as_str())
                            .help("Consider changing the name of the repeated field")
                            .location(pair_loc)
                            .lines(pair_str)
                            .send().unwrap();

                    } else {
                        fields.push((field_name.to_string(), field_ty));
                    }
                }
            }
        }

        variants.push((variant_id.to_string(), fields));
    }

    (AstNode::EnumProto { name, visibility, variants, is_simple }, deps)
}

pub fn parse_enum_value(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;
    let mut inner = pair.into_inner();

    let enum_name = inner.next().unwrap().as_str().to_string();
    let variant_id = inner.next().unwrap().as_str().to_string();

    // check if the enum type exists
    let true_members = match ST.lock().unwrap().search_enum_variant(&enum_name, &variant_id) {
        Ok(Some(m)) => Some(m),
        // the struct definition had an error, so return a default struct value
        Ok(None) => return AstNode::EnumVariant {
            enum_name,
            variant: variant_id,
            fields: vec![],
            is_const: false,
        },
        Err(e) => {
            e.lines(pair_str)
             .location(pair_loc)
             .send()
             .unwrap();
            None
        },
    };

    let fields = strct::parse_strct_members(inner, 
                                             &enum_name, 
                                             true_members, 
                                             pair_str, 
                                             pair_loc);
    AstNode::EnumVariant {
        enum_name,
        variant: variant_id,
        fields,
        is_const: false,
    }
}
