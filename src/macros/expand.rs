use mlua::{Lua, LuaSerdeExt, String as LuaString, Table, Value};

use crate::{current_unit_st, AstNode, LogMesg, VarType};

use super::lua_utils::*;

/// Runs the macro code (in lua), with the given AST node list as input and returns the AST generated
/// by the macro.
pub fn macro_expand(
    macro_name: &str,
    input_ast: &[AstNode],
    location: usize,
    lines: &str,
) -> Result<AstNode, LogMesg> {
    let res = current_unit_st!().search_macro(&macro_name);
    let code = match res {
        Ok(c) => c,
        Err(err) => return Err(err.lines(lines).location(location)),
    };

    run_lua_macro(
        macro_name.to_string(),
        location,
        lines.to_string(),
        &code,
        input_ast,
    )
    .map_err(|lua_err| {
        LogMesg::err()
            .name("Macro failed")
            .cause(format!("Failed to run {} macro", macro_name))
            .lines(lua_err.to_string().as_str())
    })
}

fn run_lua_macro(
    macro_id: String,
    location: usize,
    lines: String,
    macro_code: &str,
    ast: &[AstNode],
) -> mlua::Result<AstNode> {
    let lua = Lua::new();

    let globals = lua.globals();

    let okta_table = lua.create_table().unwrap();

    // create macro table
    let macro_table = lua.create_table().unwrap();

    let quote_fn = lua.create_function(|lua, input: LuaString| {
        let res = quote(input.to_str().unwrap());
        Ok(lua.to_value(&res))
    })?;

    let get_type_fn = lua.create_function(|lua, val: Table| {
        let node: AstNode = lua.from_value(Value::Table(val))?;
        let res = get_node_type(node);
        Ok(lua.to_value(&res))
    })?;

    let macro_name = macro_id.clone();
    let pair_lines = lines.clone();

    let compiler_error_fn = lua.create_function(move |lua, err_table: Table| {
        let cause = err_table
            .get::<_, Option<LuaString>>("cause")?
            .map(|val| lua.from_value::<String>(Value::String(val)).unwrap());

        let help = err_table
            .get::<_, Option<LuaString>>("help")?
            .map(|val| lua.from_value::<String>(Value::String(val)).unwrap());

        compiler_error(macro_id.clone(), location, pair_lines.clone(), cause, help);
        Ok(())
    })?;

    okta_table.set("quote", quote_fn).unwrap();
    okta_table.set("node_type", get_type_fn).unwrap();
    okta_table.set("compiler_error", compiler_error_fn).unwrap();

    // pass the input AST list
    macro_table
        .set("input", lua.to_value(ast).unwrap())
        .unwrap();
    okta_table.set("macro", macro_table).unwrap();

    globals.set("okta", okta_table)?;

    // evaluate the lua code and return the generated AST
    let val = lua.load(macro_code).eval()?; // get return value
    let out: Vec<AstNode> = lua.from_value(val)?;

    for node in &out {
        if let Err(e) = record_ast_in_st(node) {
            e.location(location).lines(&lines).send().unwrap();
        }
    }

    Ok(AstNode::MacroResult {
        id: macro_name,
        stmts: out,
    })
}

fn record_ast_in_st(node: &AstNode) -> Result<(), LogMesg> {
    match node {
        AstNode::VarDeclStmt { id, var_type, .. } => {
            current_unit_st!().record_var(id, var_type.clone())
        }
        AstNode::Stmts(nodes) => {
            for v in nodes {
                record_ast_in_st(v)?;
            }
            Ok(())
        }
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
            ..
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
}
