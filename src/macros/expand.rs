use mlua::{Lua, LuaSerdeExt, String as LuaString, Table, Value};

use super::lua_utils::{self, *};
use crate::{current_unit_st, AstNode, LogMesg};

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
        let res = match quote(input.to_str().unwrap()) {
            Ok(ast) => lua.to_value(&ast),
            Err(err) => {
                let err_table = lua.create_table()?;
                err_table.set("error", err)?;
                Ok(Value::Table(err_table))
            }
        };
        Ok(res)
    })?;

    let get_type_fn = lua.create_function(|lua, val: Table| {
        let node: AstNode = lua.from_value(Value::Table(val))?;
        let res = get_node_type(node);
        Ok(lua.to_value(&res))
    })?;

    let registern_fn = lua.create_function(|lua, val: Table| {
        let node: AstNode = lua.from_value(Value::Table(val))?;
        let _ = register(&node);
        Ok(())
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
    okta_table.set("register", registern_fn).unwrap();

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
        // TODO: Find a better alternative than ignoring errors here
        lua_utils::register(node).unwrap();
    }

    Ok(AstNode::MacroResult {
        id: macro_name,
        stmts: out,
    })
}