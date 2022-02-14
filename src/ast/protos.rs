use pest::iterators::Pair;
use pest::iterators::Pairs;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use super::{parser::*, *};
use crate::current_unit_protos;
use crate::{
    ast::misc::parse_visibility, current_unit_st, current_unit_status, macros, types,
    CompUnitStatus, LogMesg, VarType,
};

/// Records all module-local symbols of the unit in the unit's symbol table
/// (as opaque symbols). This function, in the same processing step, also parses
/// all import statements and macros of the unit (if some).
///
/// After this function is called, all module-local symbols of the unit will be
/// evailable as opaque symbols in the unit's symbol table's main scope.
///
/// # Returns
///
/// The function returns a tuple of vectors, where the first element is the list
/// of all import statements and the second element is the list of all macros.
pub fn rec_types_and_parse_imports_and_macros(
    syntax_tree: Pairs<Rule>,
) -> (Vec<PathBuf>, Vec<Arc<AstNode>>) {
    let mut imports = vec![];
    let mut macros = vec![];

    for pair in syntax_tree {
        let pair_str = pair.as_str();
        let pair_loc = pair.as_span().start_pos().line_col().0;
        let pair_rule = pair.as_rule();

        match pair_rule {
            // fully parse imports (`use` statements) and macros
            Rule::useModules => imports.append(&mut imports::parse_use_module(pair)),
            Rule::macroDecl => macros.push(Arc::new(macros::parser::parse_macro(pair))),

            Rule::structDef | Rule::enumDef | Rule::aliasDecl | Rule::constVarDecl => {
                let mut inner = pair.into_inner();

                // skip `compOp` rule
                let mut next = inner.next().unwrap();
                if next.as_rule() == Rule::compOpts {
                    next = inner.next().unwrap();
                }

                let mut visibility = Visibility::Priv;

                let name = match next.as_rule() {
                    Rule::id => next,
                    Rule::visibility => {
                        visibility = parse_visibility(next);
                        inner.next().unwrap()
                    }
                    _ => unreachable!(),
                }
                .as_str();

                let res = match pair_rule {
                    Rule::structDef => current_unit_st!().record_opaque_struct(name, visibility),
                    Rule::enumDef => current_unit_st!().record_opaque_enum(name, visibility),
                    Rule::aliasDecl => current_unit_st!().record_opaque_alias(name, visibility),
                    Rule::constVarDecl => {
                        let ty = ty::parse_ty_or_default(
                            inner.next().unwrap(),
                            Some((pair_str, pair_loc)),
                        );
                        current_unit_st!().record_opaque_const_var(name, visibility, ty)
                    }
                    _ => unreachable!(),
                };

                if let Err(e) = res {
                    // if there are more than one symbols with the same name
                    e.location(pair_loc).lines(pair_str).send().unwrap();
                };
            }
            _ => continue,
        }
    }

    (imports, macros)
}

/// Iterates over all the definitions in the `main` rule, pushing all the prototype definitions to
/// the symbol table of the module. Running this step before the main pass is mandatory, as
/// prototype definitions must be in the symbol table before converting all the parsed tree into an
/// AST, mainly for error checking and symbol declaration order invariance.
pub fn generate_protos(syntax_tree: Pairs<Rule>) -> Vec<AstNode> {
    let mut protos = vec![];

    // parse all prototypes
    for pair in syntax_tree {
        match pair.as_rule() {
            Rule::funcDecl => protos.push(func::parse_func_proto(pair)),
            Rule::externFunc => protos.push(func::parse_extern_func_proto(pair)),
            Rule::structDef => protos.push(strct::parse_struct_proto(pair)),
            Rule::enumDef => protos.push(ty_enum::parse_enum_proto(pair)),
            Rule::aliasDecl => protos.push(parse_alias(pair)),
            Rule::constVarDecl => protos.push(consts::parse_const_var(pair)),
            Rule::EOI => break,
            _ => continue,
        }
    }

    protos
}

fn parse_alias(pair: Pair<Rule>) -> AstNode {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut inner = pair.into_inner();

    let next = inner.next().unwrap();
    let (visibility, name) = match next.as_rule() {
        Rule::id => (Visibility::Priv, next.as_str().to_string()),
        Rule::visibility => (
            misc::parse_visibility(next),
            inner.next().unwrap().as_str().to_string(),
        ),
        _ => unreachable!(),
    };

    let ty = ty::parse_ty_or_default(inner.next().unwrap(), Some((pair_str, pair_loc)));

    let res = current_unit_st!().record_alias(&name, ty.clone(), visibility.clone());
    if let Err(e) = res {
        e.lines(pair_str).location(pair_loc).send().unwrap();
    }

    AstNode::AliasProto {
        name,
        visibility,
        ty,
    }
}

/// This function check for infinite size types (recursive type definitions with no indirection) in
/// the current compilation unit's prototype list.
pub fn validate_protos() {
    let protos = Arc::clone(&current_unit_protos!());

    for proto in protos.lock().unwrap().iter() {
        let name = match &**proto {
            AstNode::StructProto { name, .. } | AstNode::EnumProto { name, .. } => name,
            _ => continue, // non-type definitions (such as functions) are skipped
        };

        let mut deps = vec![];
        if let Err(e) = types::type_dependencies(&name, &mut deps) {
            e.send().unwrap();
        }

        for dep in deps {
            if dep == *name {
                LogMesg::err()
                    .name("Infinite size type")
                    .cause(format!(
                        "Type {} is recursive without \
                                    indirection, it has infinite size",
                        name
                    ))
                    .help(format!("Try inserting some indirection (e.g. `&`)"))
                    .send()
                    .unwrap();
                break;
            }
        }
    }
}

/// Push all the public prototypes of the imported modules into the current unit's symbol table.
/// This function also appends all these prototypes in the `import_protos` field of the current
/// unit.
pub fn import_protos() {
    let imports = current_unit_status!()
        .lock()
        .unwrap()
        .imports
        .iter()
        .map(|(path, unit_arc)| (path.clone(), Arc::clone(&unit_arc)))
        .collect::<Vec<(PathBuf, Arc<Mutex<CompUnitStatus>>)>>();

    // public prototyes from all the imported modules
    let mut imported_protos = vec![];

    for (path, unit_arc) in imports {
        // for each imported unit

        // don't allow to import protos from the current unit into the current unit
        if current_unit_status!().lock().unwrap().path == *path {
            continue;
        }

        // get imported unit's public prototypes
        let protos = Arc::clone(&unit_arc.lock().unwrap().protos);
        let mut pub_protos = protos
            .lock()
            .unwrap()
            .iter()
            .filter(|&p| match &**p {
                AstNode::StructProto { visibility, .. }
                | AstNode::EnumProto { visibility, .. }
                | AstNode::FuncProto { visibility, .. }
                | AstNode::AliasProto { visibility, .. }
                | AstNode::ConstVarDecl { visibility, .. }
                | AstNode::ExternFuncProto { visibility, .. } => *visibility == Visibility::Pub,
                _ => false,
            })
            .map(|p| Arc::clone(&*p))
            .collect::<Vec<Arc<AstNode>>>();

        // register all the imported public protos into the current unit's symbol table
        for proto in &pub_protos {
            match &**proto {
                AstNode::StructProto {
                    name,
                    members,
                    visibility,
                    ..
                } => current_unit_st!().record_struct(name, members.clone(), visibility.clone()),
                AstNode::EnumProto {
                    name,
                    variants,
                    visibility,
                    ..
                } => current_unit_st!().record_enum(name, variants.clone(), visibility.clone()),
                AstNode::AliasProto {
                    name,
                    ty,
                    visibility,
                } => current_unit_st!().record_alias(name, ty.clone(), visibility.clone()),
                AstNode::FuncProto {
                    name,
                    ret_type,
                    params,
                    visibility,
                } => {
                    let (_, params): (Vec<String>, Vec<VarType>) = params.iter().cloned().unzip();
                    current_unit_st!().record_func(
                        name,
                        ret_type.clone(),
                        params,
                        visibility.clone(),
                        false, // okta functions cannot be variadic (only extern functions)
                    )
                }
                AstNode::ExternFuncProto {
                    name,
                    ret_type,
                    param_types,
                    variadic,
                    visibility,
                } => current_unit_st!().record_func(
                    name,
                    ret_type.clone(),
                    param_types.clone(),
                    visibility.clone(),
                    *variadic,
                ),
                AstNode::ConstVarDecl {
                    name,
                    ty,
                    visibility,
                    value,
                    ..
                } => current_unit_st!().record_const_var(
                    name,
                    ty.clone(),
                    visibility.clone(),
                    *value.clone(),
                ),
                _ => Ok(()),
            }
            .unwrap(); // this cannot fail
        }

        imported_protos.append(&mut pub_protos);
    }

    current_unit_status!().lock().unwrap().imported_protos = Arc::new(imported_protos);
}
