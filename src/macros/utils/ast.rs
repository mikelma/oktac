use crate::AstNode;
use mlua::{Lua, LuaSerdeExt, String as LuaString, Table, Value};

pub fn add_ast_utils(lua: &Lua, okta_table: &Table) -> mlua::Result<()> {
    let ast_utils = lua.create_table().unwrap();

    let if_stmt = lua.create_function(|lua, vals: Table| {
        let cond: Box<AstNode> = lua.from_value(vals.get("cond")?)?;
        let then_b: Box<AstNode> = lua.from_value(vals.get("then_block")?)?;
        let elif_b: Vec<(AstNode, AstNode)> = lua.from_value(vals.get("elif_blocks")?)?;
        let else_b: Box<AstNode> = lua.from_value(vals.get("elif_blocks")?)?;

        let result = AstNode::IfStmt {
            cond,
            then_b,
            elif_b,
            else_b: Some(else_b),
        };

        Ok(lua.to_value(&result)?)
    })?;

    ast_utils.set("if_stmt", if_stmt)?;

    okta_table.set("ast", ast_utils)?;

    Ok(())
}
