use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetTriple;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use either::Either;
use target_lexicon::Triple;

use std::fmt;

use crate::{ast::*, VarType};

mod builtin;
mod expr;
mod protos;
mod st;
mod stmts;
mod utils;

use st::*;

const FN_RET_BB: &str = "ret.bb";

type CompRet<'ctx> = Result<Option<BasicValueEnum<'ctx>>, String>;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    st: CodegenST<'ctx>,
    curr_func: Option<FunctionValue<'ctx>>,
    curr_fn_ret_val: Option<PointerValue<'ctx>>,
    curr_fn_ret_bb: Option<BasicBlock<'ctx>>,
    loop_exit_bb: Option<BasicBlock<'ctx>>,
    global_var_init: bool,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, unit_name: String, target: Triple) -> CodeGen {
        // create main module
        let module = context.create_module(&unit_name);
        let builder = context.create_builder();

        // set target triple
        let triple = TargetTriple::create(target.to_string().as_str());
        module.set_triple(&triple);

        // let global_print_str = builder.build_global_string_ptr("%d\n", "my_str");

        CodeGen {
            context,
            module,
            builder,
            st: CodegenST::default(),
            // functions: HashMap::new(),
            // global_print_str,
            curr_func: None,
            curr_fn_ret_val: None,
            curr_fn_ret_bb: None,
            loop_exit_bb: None,
            global_var_init: false,
        }
    }

    pub fn compile_nodes(&mut self, nodes: &[AstNode]) -> Result<(), String> {
        for node in nodes {
            self.compile_node(node)?;
        }
        Ok(())
    }

    pub fn compile_node(&mut self, node: &AstNode) -> CompRet<'ctx> {
        match node {
            AstNode::FuncDecl {
                name,
                ret_type,
                params,
                stmts,
                ..
            } => self.compile_func_decl(name, ret_type, params, stmts),
            AstNode::Stmts(exprs) => {
                // in every statement block, a new symbol table is pushed to the symbol table
                // stack
                self.st.push_table();

                for expr in exprs {
                    let _ = self.compile_node(expr)?;
                }

                // pop the symbol table as the scope of the statments block has ended
                self.st.pop_table();

                Ok(None)
            }
            AstNode::MacroResult { stmts, .. } => {
                for stmt in stmts {
                    let _ = self.compile_node(stmt)?;
                }
                Ok(None)
            }
            AstNode::VarDeclStmt {
                id,
                var_type,
                value,
            } => self.compile_var_decl_stmt(id, var_type, value),
            AstNode::BinaryExpr {
                left,
                op,
                right,
                expr_ty: _,
                vars_ty,
            } => self.compile_binary_expr(left, op, right, vars_ty),
            AstNode::UnaryExpr {
                op: operator,
                value,
                expr_ty: _,
                var_ty,
            } => self.compile_unary_expr(operator, value, var_ty),
            AstNode::AssignStmt {
                left: lhs,
                right: rhs,
            } => self.compile_assign_stmt(lhs, rhs),
            AstNode::FunCall {
                name,
                params,
                builtin,
                ret_ty,
                ..
            } => {
                if !builtin {
                    self.compile_func_call(name, params)
                } else {
                    self.compile_builtin_func(name, &params, ret_ty)
                }
            }
            AstNode::IfStmt {
                cond,
                then_b,
                elif_b,
                else_b,
            } => self.compile_ifelse_stmt(cond, then_b, elif_b, else_b),
            AstNode::IfLetStmt {
                l_enum,
                r_expr,
                then_b,
                else_b,
            } => self.compile_if_let_stmt(l_enum, r_expr, then_b, else_b),
            AstNode::ReturnStmt(expr) => self.compile_return_stmt(expr),
            AstNode::LoopStmt(stmts) => self.compile_loop_stmt(stmts),
            AstNode::BreakStmt => self.compile_break_stmt(),
            AstNode::MemberAccessExpr {
                parent,
                members,
                parent_ty,
                access_types,
                ..
            } => self.compile_memb_acess_expr(parent, members, access_types, parent_ty),
            AstNode::Int32(_)
            | AstNode::UInt8(_)
            | AstNode::Int8(_)
            | AstNode::UInt16(_)
            | AstNode::Int16(_)
            | AstNode::UInt32(_)
            | AstNode::Int64(_)
            | AstNode::UInt64(_)
            | AstNode::Float32(_)
            | AstNode::Float64(_)
            | AstNode::Identifyer(_)
            | AstNode::Array { .. }
            | AstNode::Strct { .. }
            | AstNode::EnumVariant { .. }
            | AstNode::String(_)
            | AstNode::Boolean(_) => self.compile_value(node),
            _ => unreachable!("{:#?}", node),
        }
    }

    /*
    /// Writes module bitcode to a file in the given path
    pub fn write_bc(&self, path: &Path) -> Result<(), String> {
        if self.module.write_bitcode_to_path(path) {
           Ok(())
        } else {
            Err(format!("Cannot write bitcode to {}", path.to_str().unwrap()))
        }
    }
    */
}

fn get_value_from_result<'a>(
    value: &Option<BasicValueEnum<'a>>,
) -> Result<BasicValueEnum<'a>, String> {
    match value {
        Some(v) => Ok(*v),
        None => Err("Expression does not return any value.".to_string()),
    }
}

fn basic_to_int_value<'ctx>(value: &dyn BasicValue<'ctx>) -> Result<IntValue<'ctx>, String> {
    match value.as_basic_value_enum() {
        BasicValueEnum::IntValue(val) => Ok(val),
        _ => Err("Cannot convert basic value to int value".to_string()),
    }
}

impl<'ctx> fmt::Display for CodeGen<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.module.print_to_string().to_string())
    }
}
