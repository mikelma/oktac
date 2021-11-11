use pest::iterators::Pairs;

use std::path::PathBuf;
use std::sync::Arc;

use super::{parser::*, *};
use crate::{
    LogMesg, VarType, 
    ast::misc::parse_visibility, 
    current_unit_st,
    current_unit_status
};

/// Records all local symbols of the unit in the unit's symbol table (as opaque types) 
/// and parses the import statements of the unit (if some).
///
/// # Returns
///
/// The function returns a vector of parsed import statements contained in the
/// input syntax tree. If there are no import statements, an empty vector is returned.
pub fn rec_types_and_parse_imports(syntax_tree: Pairs<Rule>) -> Vec<PathBuf> {
    let mut imports = vec![];

    for pair in syntax_tree {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;
        let pair_rule = pair.as_rule();
        
        if !matches!(pair_rule, Rule::structDef | Rule::enumDef | Rule::useModules) {
            continue;
        }

        match pair_rule {
            Rule::useModules => imports.append(&mut imports::parse_use_module(pair)),
            Rule::structDef | Rule::enumDef => {
                let mut inner = pair.into_inner();
                let next = inner.next().unwrap();
                
                let mut visibility = Visibility::Priv;

                let name = match next.as_rule() {
                    Rule::id => next,
                    Rule::visibility => {
                        visibility = parse_visibility(next);
                        inner.next().unwrap()
                    },
                    _ => unreachable!(),
                }
                .as_str();

                let res = match pair_rule {
                    Rule::structDef => current_unit_st!().record_opaque_struct(name, visibility),
                    Rule::enumDef => current_unit_st!().record_opaque_enum(name, visibility),
                    _ => unreachable!(),
                };

                if let Err(e) = res { // if there are more than one symbols with the same name
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                };
            },
            _ => continue,
        }
    }

    imports 
}

/// Iterates over all the definitions in the `main` rule, pushing all the prototype definitions to
/// the symbol table of the module. Running this step before the main pass is mandatory, as
/// prototype definitions must be in the symbol table before converting all the parsed tree into an
/// AST, mainly for error checking and symbol declaration order invariance.
pub fn generate_protos(syntax_tree: Pairs<Rule>) -> Vec<AstNode> {
    let mut protos = vec![];

    // parse all prototypes
    for pair in syntax_tree {
        protos.push(match pair.as_rule() {
            Rule::funcDecl => func::parse_func_proto(pair),
            Rule::externFunc => func::parse_extern_func_proto(pair),
            Rule::structDef => strct::parse_struct_proto(pair),
            Rule::enumDef => ty_enum::parse_enum_proto(pair),
            Rule::EOI => break,
            _ => continue,
        });
    }

    // let mut protos = parse_ty_protos(ty_pairs);
    // let mut func_protos = parse_fun_protos(func_pairs);
    // protos.append(&mut func_protos);

    protos
}

/*
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
*/

pub fn validate_protos() {
    let protos = Arc::clone(&current_unit_status!().lock().unwrap().protos);

    for proto in &*protos {
        let name = match &**proto {
            AstNode::StructProto {name, ..} 
            | AstNode::EnumProto {name, ..} => name,
            _ => continue,
        };

        let mut deps = vec![];
        if let Err(e) = type_dependencies(&name, &mut deps) {
            e.send().unwrap();
        }

        for dep in deps {
            if dep == *name {
                LogMesg::err()
                    .name("Infinite size type")
                    .cause(format!("Type {} is recursive without \
                                    indirection, it has infinite size", name))
                    .help(format!("Try inserting some indirection (e.g. `&`)"))
                    .send()
                    .unwrap();
                break;
            }
        }
    }
}

fn type_dependencies(symbol: &str, dependecies: &mut Vec<String>) -> Result<(), LogMesg> {
    let ty = current_unit_st!().symbol_type(symbol)?.unwrap();
    
    let all_types = match ty {
        VarType::Struct(name) => {
            let (_, members): (Vec<_>, Vec<_>) = current_unit_st!()
                .search_struct(&name)
                .unwrap().unwrap()
                .iter()
                .cloned() // TODO: To optimize
                .unzip();
            members
        },
        VarType::Enum(name) => {
            let (_, fields): (Vec<_>, Vec<_>) = current_unit_st!()
                .search_enum(&name)
                .unwrap().unwrap()
                .iter()
                .cloned() // TODO: To optimize
                .unzip();

            let (_, f): (Vec<_>, Vec<_>) = fields
                         .concat()
                         .iter()
                         .cloned() // TODO: To optimize
                         .unzip();
            f
        },
        _ => panic!("Cannot get dependecies of type {:?}", ty),
    };

    for ty in all_types {
        match ty {
            VarType::Struct(name) | VarType::Enum(name) => {
                if name == symbol {
                    return Err(
                        LogMesg::err()
                            .name("Infinite size type")
                            .cause(format!("Type {} is recursive without \
                                           indirection, it has infinite size", symbol))
                            .help(format!("Try inserting some indirection (e.g. `&`)"))
                    );
                } else {
                    if dependecies.contains(&name) {
                        // endless recursive loop detected, just return here, as all dependencies are
                        // already contained in the `dependecies` list
                        return Ok(());

                    } else { 
                        dependecies.push(name.clone());
                        type_dependencies(&name, dependecies)?;
                    }
                }
            },
            _ => continue, 
        }
    }

    Ok(())
}

pub fn import_protos() {
    let imports = current_unit_status!().lock().unwrap().imports.clone();
    let mut imported_protos = vec![];

    for (path, unit_arc) in imports {

        // don't allow to import protos from the current unit into the current unit
        if current_unit_status!().lock().unwrap().path == *path {
            continue;
        }

        let mut pub_protos = unit_arc.lock().unwrap()
            .protos
            .iter()
            .filter(|&p| match &**p {
                AstNode::StructProto { visibility, .. }
                | AstNode::EnumProto { visibility, .. } 
                | AstNode::FuncProto { visibility, .. } => *visibility == Visibility::Pub,
                _ => false,
            })
            .map(|p| Arc::clone(&*p))
            .collect::<Vec<Arc<AstNode>>>();

        for proto in &pub_protos {
            match &**proto {
                AstNode::StructProto { name, members, visibility } 
                    => current_unit_st!()
                        .record_struct(name, members.clone(), visibility.clone()),
                AstNode::EnumProto { name, variants, visibility, .. } 
                    => current_unit_st!().record_enum(name, variants.clone(), visibility.clone()),
                AstNode::FuncProto { name, ret_type, params, visibility } 
                    => {
                        let (_, params): (Vec<String>, Vec<VarType>) = params.iter().cloned().unzip();
                        current_unit_st!().record_func(name, ret_type.clone(), params, visibility.clone())
                    },
                _ => Ok(()), 

            }.unwrap(); // this cannot fail
        }

        imported_protos.append(&mut pub_protos);
    }

    current_unit_status!().lock().unwrap().imported_protos = Arc::new(imported_protos);

}

/*
fn parse_ty_protos(pairs: Vec<Pair<Rule>>) -> Vec<AstNode> {
    // get the names of all type definitions in the module and register the
    // types as opaque in the symbol table
    for pair in pairs.clone() {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;
        let pair_rule = pair.as_rule();

        let mut inner = pair.into_inner();
        let next = inner.next().unwrap();

        let mut visibility = Visibility::Priv;

        let name = match next.as_rule() {
            Rule::id => next,
            Rule::visibility => {
                visibility = parse_visibility(next);
                inner.next().unwrap()
            },
            _ => unreachable!(),
        }
        .as_str();

        let res = match pair_rule {
            Rule::structDef => current_unit_st!().record_opaque_struct(name, visibility),
            Rule::enumDef => current_unit_st!().record_opaque_enum(name, visibility),
            _ => unreachable!(),
        };

        if let Err(e) = res {
            e.location(pair_loc).lines(pair_str).send().unwrap();
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
        dependencies
            .iter()
            .filter(|(other, _)| deps.contains(other))
            .for_each(|(other, other_deps)| {
                if other_deps.iter().find(|d| *d == name).is_some() {
                    LogMesg::err()
                        .name("Circular dependency")
                        .cause(
                            format!("Types {} and {} contain a circular dependency", other, name)
                        )
                        .help(
                            "Consider encapsulating at least one of this types\
                                \n* NOTE: This feature is not implemented yet!".into(),
                        )
                        .location(pair_loc)
                        .lines(pair_str)
                        .send()
                        .unwrap();
                }
            });

        dependencies.insert(name.to_string(), deps);

        let res = match &proto {
            AstNode::EnumProto {
                name,
                visibility,
                variants,
                ..
            } => current_unit_st!().record_enum(&name, variants.clone(), visibility.clone()),
            AstNode::StructProto {
                name,
                visibility,
                members,
            } => current_unit_st!().record_struct(&name, members.clone(), visibility.clone()),
            _ => unreachable!(),
        };

        if let Err(e) = res {
            e.location(pair_loc).lines(pair_str).send().unwrap();
        }

        protos.push(proto);
    }

    protos
}
*/
