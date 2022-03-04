use crate::{ast::check, AstNode, BinaryOp, VarType};
use mlua::{Lua, LuaSerdeExt, Table, Value};

pub fn add_ast_utils(lua: &Lua, okta_table: &Table) -> mlua::Result<()> {
    let ast_utils = lua.create_table().unwrap();

    let if_stmt = lua.create_function(|lua, vals: Table| {
        let cond: Box<AstNode> = lua.from_value(vals.get("condition")?)?;
        let then_b: Box<AstNode> = lua.from_value(vals.get("then_block")?)?;
        let elif_b: Vec<(AstNode, AstNode)> = lua.from_value(vals.get("elif_blocks")?)?;
        let else_b: Box<AstNode> = lua.from_value(vals.get("else_block")?)?;

        let result = AstNode::IfStmt {
            cond,
            then_b,
            elif_b,
            else_b: Some(else_b),
        };

        Ok(lua.to_value(&result)?)
    })?;

    ast_utils.set("if_stmt", if_stmt)?;

    let binary_expr = lua.create_function(|lua, vals: Table| {
        let left: AstNode = lua.from_value(vals.get("left")?)?;
        let right: AstNode = lua.from_value(vals.get("right")?)?;
        let op: BinaryOp = lua.from_value(vals.get("op")?)?;

        let (left, left_ty) = match check::node_type(left, None) {
            (node, Ok(t)) => (node, t),
            (_, Err(err)) => {
                err.send().unwrap();
                return Ok(Value::Table(lua.create_table()?));
            }
        };

        let (right, right_ty) = match check::node_type(right, Some(left_ty.clone())) {
            (node, Ok(t)) => (node, t),
            (_, Err(err)) => {
                err.send().unwrap();
                return Ok(Value::Table(lua.create_table()?));
            }
        };

        let expr_ty = match check::binop_resolve_types(&left_ty, &right_ty, &op) {
            Ok(t) => t,
            Err(err) => {
                err.send().unwrap();
                VarType::Unknown
            }
        };

        let result = AstNode::BinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
            expr_ty,
            vars_ty: left_ty,
        };

        Ok(lua.to_value(&result)?)
    })?;

    ast_utils.set("binary_expr", binary_expr)?;

    okta_table.set("ast", ast_utils)?;

    Ok(())
}
