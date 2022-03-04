use mlua::{Lua, LuaSerdeExt, String as LuaString, Table, Value};

use std::sync::Arc;

use crate::*;

pub fn add_st_utils(lua: &Lua, okta_table: &Table) -> mlua::Result<()> {
    let st_utils = lua.create_table().unwrap();

    let registern_fn = lua.create_function(|lua, val: Table| {
        let node: AstNode = lua.from_value(Value::Table(val))?;
        let _ = register(&node);
        Ok(())
    })?;

    st_utils.set("register", registern_fn).unwrap();

    let exists_fn = lua.create_function(|_, symbol: LuaString| Ok(exists(symbol.to_str()?)))?;

    st_utils.set("exists", exists_fn).unwrap();

    okta_table.set("st", st_utils)?;

    Ok(())
}

fn exists(symbol: &str) -> bool {
    let res: bool = current_unit_st!().exists(symbol);
    res
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

            let exists = current_unit_st!().exists(name);
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

            let exists = current_unit_st!().exists(name);
            if !exists {
                current_unit_st!().record_struct(name, members.to_vec(), visibility.clone())?;
            }
        }
        _ => (), // ignore nodes that don't need to be registered
    }
    Ok(())
}
