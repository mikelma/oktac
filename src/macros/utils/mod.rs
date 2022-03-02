use pest::Parser;

use std::sync::Arc;

use crate::{
    ast::parser::*, ast::*, current_unit_ast, current_unit_protos, current_unit_st,
    current_unit_status, AstNode, LogMesg, VarType,
};

pub mod ast;

pub fn quote(source: &str) -> Result<Vec<AstNode>, String> {
    let mut parsed = match PestParser::parse(Rule::macroQuote, &source) {
        Ok(v) => v,
        Err(e) => return Err(e.to_string()),
    };

    let syntax_tree = parsed.next().unwrap();

    let mut result = vec![];

    let pairs = syntax_tree.into_inner();

    for pair in pairs {
        match pair.as_rule() {
            Rule::funcDecl => {
                result.push(func::parse_func_proto(pair.clone()));
                result.push(func::parse_func_decl(pair));
            }
            Rule::externFunc => result.push(func::parse_extern_func_proto(pair)),
            Rule::structDef => result.push(strct::parse_struct_proto(pair)),
            Rule::enumDef => result.push(ty_enum::parse_enum_proto(pair)),
            Rule::aliasDecl => result.push(misc::parse_alias(pair)),
            Rule::constVarDecl => result.push(consts::parse_const_var(pair)),

            Rule::stmts => {
                for stmt_pair in pair.into_inner() {
                    result.push(stmts::parse_stmt(stmt_pair));
                }
            }
            Rule::stmt => result.push(stmts::parse_stmt(pair)),
            _ => result.push(expr::parse_expr(pair)),
        }
    }

    Ok(result)
}

// TODO: Missing nodes
pub fn register(node: &AstNode) -> Result<(), LogMesg> {
    let protos_contains = |node: &AstNode| {
        current_unit_protos!()
            .lock()
            .unwrap()
            .iter()
            .map(|v| &**v)
            .find(|v| *v == node)
            .is_some()
    };

    let ast_contains = |node: &AstNode| current_unit_ast!().lock().unwrap().contains(node);

    match node {
        AstNode::FuncProto {
            name,
            visibility,
            ret_type,
            params,
            ..
        } => {
            // NOTE: Always push before recording, as recording can return an error

            let contains = protos_contains(node);
            if !contains {
                current_unit_protos!()
                    .lock()
                    .unwrap()
                    .push(Arc::new(node.clone()));
            }

            let exists = current_unit_st!().exits(name);
            if !exists {
                // register function in the symbol table
                let p = params
                    .iter()
                    .map(|(_, p)| p.clone())
                    .collect::<Vec<VarType>>();

                current_unit_st!().record_func(
                    &name,
                    ret_type.clone(),
                    p,
                    visibility.clone(),
                    false,
                )?;
            }
        }
        AstNode::FuncDecl { .. } => {
            // add function to the AST of the current unit
            let contains = ast_contains(node);
            if !contains {
                current_unit_status!()
                    .lock()
                    .unwrap()
                    .ast
                    .lock()
                    .unwrap()
                    .push(node.clone());
            }
        }
        AstNode::StructProto {
            name,
            visibility,
            members,
            ..
        } => {
            // NOTE: Always push before recording, as recording can return an error

            let contains = protos_contains(node);
            if !contains {
                current_unit_protos!()
                    .lock()
                    .unwrap()
                    .push(Arc::new(node.clone()));
            }

            let exists = current_unit_st!().exits(name);
            if !exists {
                current_unit_st!().record_struct(name, members.to_vec(), visibility.clone())?;
            }
        }
        _ => (), // ignore nodes that don't need to be registered
    }
    Ok(())
}

pub fn get_node_type(node: AstNode) -> String {
    check::node_type(node, None)
        .1
        .unwrap_or(VarType::Unknown)
        .to_string()
}

pub fn compiler_error(
    macro_id: String,
    location: usize,
    lines: String,
    cause: Option<String>,
    help: Option<String>,
) {
    let mut error = LogMesg::err().name(format!("Macro error: {}", macro_id).as_str());

    if let Some(v) = cause {
        error = error.cause(v);
    } else {
        error = error.cause("Unknown cause".into());
    }

    if let Some(v) = help {
        error = error.help(v);
    }

    error
        .lines(lines.as_str())
        .location(location)
        .send()
        .unwrap();
}
