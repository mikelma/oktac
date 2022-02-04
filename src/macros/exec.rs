use mlua::{Lua, LuaSerdeExt, String as LuaString, Table, Value};
use pest::Parser;

use crate::{
    ast::{self, check, parser::*},
    AstNode, LogMesg, VarType,
};

/// Runs the macro code (in lua), with the given AST node list as input and returns the AST generated
/// by the macro.
pub fn run_macro(macro_id: String, macro_code: &str, ast: &[AstNode]) -> mlua::Result<AstNode> {
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

    let compiler_error_fn =
        lua.create_function(move |_, (cause, lines): (LuaString, LuaString)| {
            compiler_error(
                macro_id.clone(),
                cause.to_str().unwrap(),
                lines.to_str().unwrap(),
            );
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

    Ok(AstNode::MacroResult {
        id: macro_name,
        stmts: out,
    })
}

fn quote(input: &str) -> Vec<AstNode> {
    let source = format!("{{ {} }}", input); // to match stmts rule (TODO: Improve)

    let mut parsed = match PestParser::parse(Rule::stmts, &source) {
        Ok(v) => v,
        Err(e) => {
            dbg!(e);
            todo!()
        }
    };

    let syntax_tree = parsed.next().unwrap();

    let mut stmts = vec![];
    for pair in syntax_tree.into_inner() {
        stmts.push(ast::stmts::parse_stmt(pair));
    }

    stmts
}

fn get_node_type(node: AstNode) -> VarType {
    match check::node_type(node, None).1 {
        Ok(t) => t,
        Err(_) => VarType::Unknown,
    }
}

fn compiler_error(macro_id: String, cause: &str, lines: &str) {
    LogMesg::err()
        .name(format!("Macro error: {}", macro_id).as_str())
        .cause(cause.to_string())
        .lines(&lines)
        .send()
        .unwrap()
}
