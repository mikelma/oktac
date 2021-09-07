use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::TargetTriple;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntMathValue, IntValue, PhiValue,
    PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use either::Either;

use std::collections::HashMap;

use crate::{ast::*, VarType};

const FN_RET_BB: &'static str = "ret.bb";

type CompRet<'ctx> = Result<Option<BasicValueEnum<'ctx>>, String>;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (VarType, PointerValue<'ctx>)>,
    curr_func: Option<FunctionValue<'ctx>>,
    curr_fn_ret_val: Option<PointerValue<'ctx>>,
    curr_fn_ret_bb: Option<BasicBlock<'ctx>>,
    loop_exit_bb: Option<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> CodeGen {
        // create main module
        let module = context.create_module("main");
        let builder = context.create_builder();

        // set target triple
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        module.set_triple(&triple);

        // let global_print_str = builder.build_global_string_ptr("%d\n", "my_str");

        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            // functions: HashMap::new(),
            // global_print_str,
            curr_func: None,
            curr_fn_ret_val: None,
            curr_fn_ret_bb: None,
            loop_exit_bb: None,
        }
    }

    fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        name: &str,
        var_type: T,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.curr_func.unwrap().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(var_type, name)
    }

    fn create_basic_block(&self, name: &'static str) -> BasicBlock<'ctx> {
        match self.curr_fn_ret_bb {
            Some(bb) => self.context.prepend_basic_block(bb, name),
            None => self
                .context
                .append_basic_block(self.curr_func.unwrap(), name),
        }
    }

    pub fn compile(&mut self, node: &AstNode) -> CompRet<'ctx> {
        match node {
            AstNode::FuncDecl {
                name,
                ret_type,
                params,
                stmts,
            } => self.compile_func_decl(name, ret_type, params, stmts),
            AstNode::ExternFunc {
                name,
                ret_type,
                param_types,
            } => self.compile_extern_func(name, ret_type, param_types),
            AstNode::Stmts(exprs) => {
                for expr in exprs {
                    let _ = self.compile(expr)?;
                }
                Ok(None)
            }
            AstNode::VarDeclExpr {
                id,
                var_type,
                value,
            } => self.compile_var_decl_expr(id, var_type, value),
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
            AstNode::AssignExpr {
                left: lhs,
                right: rhs,
            } => self.compile_assign(lhs, rhs),
            AstNode::FunCall { name, params } => self.compile_func_call(name, params),
            AstNode::IfExpr {
                cond,
                then_b,
                elif_b,
                else_b,
            } => self.compile_ifelse_expr(cond, then_b, elif_b, else_b),
            AstNode::ReturnExpr(expr) => self.compile_return_expr(expr),
            AstNode::LoopExpr(stmts) => self.compile_loop_expr(stmts),
            AstNode::BreakExpr => self.compile_break_expr(),
            AstNode::IndexationExpr { value, indexes, ty } => {
                self.compile_indexation_expr(value, indexes, ty)
            }
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
            | AstNode::Boolean(_) => self.compile_value(node),
            _ => unimplemented!(),
        }
    }

    fn compile_func_decl(
        &mut self,
        name: &String,
        ret_type: &Option<VarType>,
        params: &Vec<(String, VarType)>,
        stmts: &AstNode,
    ) -> CompRet<'ctx> {
        // create function header
        let args: Vec<BasicTypeEnum<'ctx>> = params
            .iter()
            .map(|(_, ty)| *self.okta_type_to_llvm(ty))
            .collect();
        let fn_type = match ret_type {
            Some(ty) => self.okta_type_to_llvm(ty).fn_type(&args, false),
            None => self.context.void_type().fn_type(&args, false),
        };

        let fn_val = self.module.add_function(name, fn_type, None);

        // create entry Basic Block
        let entry = self.context.append_basic_block(fn_val, "entry");

        // create return basic block
        self.curr_fn_ret_bb = Some(self.context.append_basic_block(fn_val, FN_RET_BB));

        self.builder.position_at_end(entry);

        // set argument names
        for (fn_arg, arg_name) in fn_val.get_param_iter().zip(params.iter().map(|v| &v.0)) {
            fn_arg.set_name(arg_name);
        }

        // clear symbol table
        self.variables.clear();

        // set this function as current function
        self.curr_func = Some(fn_val);

        // record function arguments in the symbol table
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let (arg_name, var_type) = &params[i];
            let arg_llvm_ty = *self.okta_type_to_llvm(&params[i].1);
            let alloca = self.create_entry_block_alloca(&arg_name, arg_llvm_ty);

            self.builder.build_store(alloca, arg);

            self.variables
                .insert(arg_name.to_string(), (var_type.clone(), alloca));
        }

        // allocate the variable to hold the return value
        self.curr_fn_ret_val = match ret_type {
            Some(ty) => {
                Some(self.create_entry_block_alloca("ret.val", *self.okta_type_to_llvm(ty)))
            }
            None => None,
        };

        // compile function's body
        let _ = self.compile(stmts)?;

        // join the current basick block with the return basic block
        self.builder
            .build_unconditional_branch(self.curr_fn_ret_bb.unwrap());

        // create return statement
        self.builder.position_at_end(self.curr_fn_ret_bb.unwrap());
        if let Some(ret_ptr) = self.curr_fn_ret_val {
            let val = self.builder.build_load(ret_ptr, "ret.val");
            self.builder.build_return(Some(&val));
        } else {
            self.builder.build_return(None); // return void
        }

        // DEBUG: produce .dot file
        // fn_val.view_function_cfg();

        Ok(None)
    }

    fn compile_extern_func(
        &mut self,
        name: &str,
        ret_type: &Option<VarType>,
        param_types: &Vec<VarType>,
    ) -> CompRet<'ctx> {
        // create function header
        let arg_types: Vec<BasicTypeEnum<'ctx>> = param_types
            .iter()
            .map(|ty| *self.okta_type_to_llvm(ty))
            .collect();
        let fn_type = match ret_type {
            Some(ty) => self.okta_type_to_llvm(ty).fn_type(&arg_types, false),
            None => self.context.void_type().fn_type(&arg_types, false),
        };

        let _fn_val = self
            .module
            .add_function(name, fn_type, Some(Linkage::External));

        Ok(None)
    }

    fn compile_var_decl_expr(
        &mut self,
        id: &str,
        var_type: &VarType,
        value: &AstNode,
    ) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile(value)?)?;

        // allocate variable
        let ptr = self.create_entry_block_alloca(id, *self.okta_type_to_llvm(var_type));
        self.variables
            .insert(id.to_string(), (var_type.clone(), ptr));
        self.variables.get(id).unwrap();

        // store the value into the variable
        let _instr = self.builder.build_store(ptr, value);

        Ok(None)
    }

    fn compile_binary_expr(
        &mut self,
        lhs: &AstNode,
        op: &BinaryOp,
        rhs: &AstNode,
        ty: &VarType,
    ) -> CompRet<'ctx> {
        let lhs = get_value_from_result(&self.compile(&lhs)?)?;
        let rhs = get_value_from_result(&self.compile(rhs)?)?;

        Ok(Some(match op {
            BinaryOp::Add => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8 => BasicValueEnum::IntValue(self.builder.build_int_add(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.add",
                )),
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::FloatValue(self.builder.build_float_add(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.add",
                    ))
                }
                _ => unimplemented!(),
            },
            BinaryOp::Subtract => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8 => BasicValueEnum::IntValue(self.builder.build_int_sub(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.sub",
                )),
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::FloatValue(self.builder.build_float_sub(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.sub",
                    ))
                }
                _ => unimplemented!(),
            },
            BinaryOp::Multiply => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8 => BasicValueEnum::IntValue(self.builder.build_int_mul(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.mul",
                )),
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::FloatValue(self.builder.build_float_mul(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.mul",
                    ))
                }
                _ => unimplemented!(),
            },
            BinaryOp::Divide => match ty {
                VarType::Int32 | VarType::Int64 | VarType::Int8 | VarType::Int16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_signed_div(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.div",
                    ))
                }
                VarType::UInt32 | VarType::UInt64 | VarType::UInt8 | VarType::UInt16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_unsigned_div(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.div",
                    ))
                }
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::FloatValue(self.builder.build_float_div(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.div",
                    ))
                }
                _ => unimplemented!(),
            },
            BinaryOp::Eq => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8 => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::OEQ,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            BinaryOp::Ne => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8 => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            BinaryOp::Lt => match ty {
                VarType::Int32 | VarType::Int64 | VarType::Int8 | VarType::Int16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::SLT,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::UInt32 | VarType::UInt64 | VarType::UInt8 | VarType::UInt16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::ULT,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::OLT,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            BinaryOp::Gt => match ty {
                VarType::Int32 | VarType::Int64 | VarType::Int8 | VarType::Int16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::SGT,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::UInt32 | VarType::UInt64 | VarType::UInt8 | VarType::UInt16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::UGT,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::OGT,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            BinaryOp::Leq => match ty {
                VarType::Int32 | VarType::Int64 | VarType::Int8 | VarType::Int16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::SLE,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::UInt32 | VarType::UInt64 | VarType::UInt8 | VarType::UInt16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::ULE,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::OLE,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            BinaryOp::Geq => match ty {
                VarType::Int32 | VarType::Int64 | VarType::Int8 | VarType::Int16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::SGE,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::UInt32 | VarType::UInt64 | VarType::UInt8 | VarType::UInt16 => {
                    BasicValueEnum::IntValue(self.builder.build_int_compare(
                        IntPredicate::UGE,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmp.cmp",
                    ))
                }
                VarType::Float32 | VarType::Float64 => {
                    BasicValueEnum::IntValue(self.builder.build_float_compare(
                        FloatPredicate::OGE,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmp.cmp",
                    ))
                }
                _ => unreachable!(),
            },
            // only for boolean type
            BinaryOp::Or if *ty == VarType::Boolean => BasicValueEnum::IntValue(
                self.builder
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "tmp.or"),
            ),
            BinaryOp::And if *ty == VarType::Boolean => BasicValueEnum::IntValue(
                self.builder
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "tmp.or"),
            ),
            _ => unreachable!(),
        }))
    }

    fn compile_unary_expr(&mut self, op: &UnaryOp, value: &AstNode, ty: &VarType) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile(value)?)?;

        Ok(Some(match op {
            UnaryOp::Not => match ty {
                VarType::Boolean => BasicValueEnum::IntValue(
                    self.builder.build_not(value.into_int_value(), "tmp.not"),
                ),
                _ => unimplemented!(),
            },
        }))
    }

    fn compile_assign(&mut self, lhs: &AstNode, rhs: &AstNode) -> CompRet<'ctx> {
        // get the pointer of the left hand side value
        let lptr = match lhs {
            AstNode::Identifyer(id) => {
                match self.variables.get(id) {
                    Some(val) => val.1,
                    None => {
                        return Err(format!("Variable `{}` was not declared in this scope", id));
                    }
                }
            }
            AstNode::IndexationExpr { value, indexes, ..} => {
                self.compile_indexation_to_ptr(value, indexes)?
            },
            _ => unreachable!(),
        };

        let rhs = get_value_from_result(&self.compile(rhs)?)?;

        // store the value of rhs to lhs
        let _instr = self.builder.build_store(lptr, rhs);
        Ok(None)
    }

    fn compile_func_call(&mut self, name: &str, params: &Vec<AstNode>) -> CompRet<'ctx> {
        let func = self
            .module
            .get_function(name)
            .expect("Cannot find function in module");

        let args: Vec<BasicValueEnum> = params
            .iter()
            .map(|node| {
                self.compile(node)
                    .unwrap()
                    .expect("Non valued expression as function argument")
            })
            .collect();

        let call = self.builder.build_call(func, &args, "fcall");

        Ok(match call.try_as_basic_value() {
            Either::Left(l) => Some(l),
            Either::Right(_r) => None,
        })
    }

    fn compile_return_expr(&mut self, expr: &AstNode) -> CompRet<'ctx> {
        if let Some(ret_ptr) = self.curr_fn_ret_val {
            let ret_val = get_value_from_result(&self.compile(expr)?)?;
            self.builder.build_store(ret_ptr, ret_val);
            self.builder
                .build_unconditional_branch(self.curr_fn_ret_bb.unwrap());
        }
        Ok(None)
    }

    fn compile_ifelse_expr(
        &mut self,
        cond: &AstNode,
        then_b: &AstNode,
        elif_b: &Vec<(AstNode, AstNode)>,
        else_b: &Option<Box<AstNode>>,
    ) -> CompRet<'ctx> {
        // compile if condition
        let cond = basic_to_int_value(&get_value_from_result(&self.compile(cond)?)?)?;
        let then_bb = self.create_basic_block("if.then");

        match else_b {
            // basic `if-else` statement
            Some(else_block) if elif_b.is_empty() => {
                let else_bb = self.create_basic_block("if.else");
                let cont_bb = self.create_basic_block("if.cont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                // build else block
                self.builder.position_at_end(else_bb);
                let _else_stmts = self.compile(else_block)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            },
            // complex `if-elif-...-else` statements
            Some(else_block) => {
                let mut next_cond = self.create_basic_block("elif.cond");
                let else_bb = self.create_basic_block("if.else");
                let cont_bb = self.create_basic_block("if.cont");

                self.builder
                    .build_conditional_branch(cond, then_bb, next_cond);

                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                let mut elif_iter = elif_b.iter().peekable();
                while let Some((cond, elif)) = elif_iter.next() {
                    // compile elif condition basic block
                    self.builder.position_at_end(next_cond);
                    let compiled_cond = basic_to_int_value(&get_value_from_result(&self.compile(cond)?)?)?;

                    let elif_bb = self.context.insert_basic_block_after(next_cond, "if.elif");

                    // check if the next block is the `else` block or if there are any 
                    // `elif.cond` blocks left
                    next_cond = if elif_iter.peek().is_some() { // check if the next element is `Some`
                        //self.create_basic_block("elif.cond")
                        self.context.insert_basic_block_after(elif_bb, "elif.cond")
                    } else {
                        else_bb
                    };

                    self.builder
                        .build_conditional_branch(compiled_cond, elif_bb, next_cond);

                    // create and compile elif block
                    self.builder.position_at_end(elif_bb);
                    let _elif_stmts = self.compile(elif)?;

                    self.builder.build_unconditional_branch(cont_bb);
                } 

                // build else block
                self.builder.position_at_end(else_bb);
                let _else_stmts = self.compile(else_block)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            },
            // single `if` block
            None => {
                let cont_bb = self.create_basic_block("if.cont");
                self.builder
                    .build_conditional_branch(cond, then_bb, cont_bb);
                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            },
        }

        Ok(None)
    }

    fn compile_value(&mut self, node: &AstNode) -> CompRet<'ctx> {
        match node {
            AstNode::Identifyer(id) => {
                if let Some((_, ptr)) = self.variables.get(id) {
                    Ok(Some(self.builder.build_load(*ptr, "tmpload")))
                } else {
                    Err(format!("Variable `{}` was not declared in this scope", id))
                }
            }
            AstNode::Int8(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i8_type().const_int(*val as u64, true),
            ))),
            AstNode::UInt8(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i8_type().const_int(*val as u64, false),
            ))),
            AstNode::Int16(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i16_type().const_int(*val as u64, true),
            ))),
            AstNode::UInt16(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i16_type().const_int(*val as u64, false),
            ))),
            AstNode::Int32(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i32_type().const_int(*val as u64, true),
            ))),
            AstNode::UInt32(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i32_type().const_int(*val as u64, false),
            ))),
            AstNode::Int64(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*val as u64, true),
            ))),
            AstNode::UInt64(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*val, false),
            ))),
            AstNode::Boolean(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.bool_type().const_int(*val as u64, false),
            ))),
            AstNode::Float32(val) => Ok(Some(BasicValueEnum::FloatValue(
                self.context.f32_type().const_float(*val as f64),
            ))),
            AstNode::Float64(val) => Ok(Some(BasicValueEnum::FloatValue(
                self.context.f64_type().const_float(*val as f64),
            ))),
            AstNode::Array { values, ty } => {
                let mut elems = vec![];
                for vals in values {
                    elems.push(self.compile(vals)?.unwrap());
                }

                let arr = match *self.okta_type_to_llvm(&ty) {
                    BasicTypeEnum::IntType(t) => {
                        t.const_array(&elems.iter().map(|v| v.into_int_value()).collect::<Vec<_>>())
                    }
                    BasicTypeEnum::FloatType(t) => t.const_array(
                        &elems
                            .iter()
                            .map(|v| v.into_float_value())
                            .collect::<Vec<_>>(),
                    ),
                    BasicTypeEnum::ArrayType(t) => t.const_array(
                        &elems
                            .iter()
                            .map(|v| v.into_array_value())
                            .collect::<Vec<_>>(),
                    ),
                    _ => unreachable!(),
                };

                Ok(Some(BasicValueEnum::ArrayValue(arr)))
            }
            _ => unreachable!(),
        }
    }

    fn compile_loop_expr(&mut self, node: &AstNode) -> CompRet<'ctx> {
        // create loop body basic block
        let loop_bb = self.create_basic_block("loop.body");

        let old_loop_exit = self.loop_exit_bb; 
        self.loop_exit_bb = Some(self.create_basic_block("loop.end"));

        // jump from the current bb to the loop's body bb
        self.builder.build_unconditional_branch(loop_bb);

        // build loop's body
        self.builder.position_at_end(loop_bb);
        let _stmts = self.compile(node)?;
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(self.loop_exit_bb.unwrap());

        self.loop_exit_bb = old_loop_exit;

        Ok(None)
    }

    fn compile_break_expr(&self) -> CompRet<'ctx> {
        match self.loop_exit_bb {
            Some(bb) => {
                self.builder.build_unconditional_branch(bb);
                Ok(None)
            }
            None => Err("Cannot call `break` outside a loop".to_string()),
        }
    }

    fn compile_indexation_to_ptr(
        &mut self,
        value: &AstNode,
        indexes: &Vec<AstNode>,
        ) -> Result<PointerValue<'ctx>, String> {

        // get the id of the array variable
        let var_id = match value {
            AstNode::Identifyer(id) => id,
            _ => unreachable!(),
        };

        // get the base pointer
        let var_ptr = self.variables.get(var_id).unwrap().1;

        // indexations are composed by at least one indexation, so compute the first one
        let zero_index = self.context.i64_type().const_int(0, false);
        let first =
            get_value_from_result(&self.compile(indexes.iter().next().unwrap())?)?.into_int_value();
        let mut gep_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(var_ptr, &[zero_index, first], "tmp.gep")
        };

        // compute the indexations left, using the last `gep_ptr` as the base pointer in every GEP.
        for idx in indexes.iter().skip(1) {
            let idx_complied = get_value_from_result(&self.compile(idx)?)?.into_int_value();
            gep_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(gep_ptr, &[zero_index, idx_complied], "tmp.gep")
            };
        }
        Ok(gep_ptr)
    }

    fn compile_indexation_expr(
        &mut self,
        value: &AstNode,
        indexes: &Vec<AstNode>,
        _ty: &VarType,
    ) -> CompRet<'ctx> {
        // get the pointer to the indexed value
        let gep_ptr = self.compile_indexation_to_ptr(value, indexes)?;
        // dereference the pointer
        let load_val = self.builder.build_load(gep_ptr, "gep.deref");
        Ok(Some(load_val))
    }

    fn okta_type_to_llvm(&self, var_type: &VarType) -> Box<BasicTypeEnum<'ctx>> {
        Box::new(match var_type {
            VarType::Int8 | VarType::UInt8 => self.context.i8_type().as_basic_type_enum(),
            VarType::Int16 | VarType::UInt16 => self.context.i16_type().as_basic_type_enum(),
            VarType::Int32 | VarType::UInt32 => self.context.i32_type().as_basic_type_enum(),
            VarType::Int64 | VarType::UInt64 => self.context.i64_type().as_basic_type_enum(),
            VarType::Float32 => self.context.f32_type().as_basic_type_enum(),
            VarType::Float64 => self.context.f64_type().as_basic_type_enum(),
            VarType::Boolean => self.context.bool_type().as_basic_type_enum(),
            VarType::Array { inner, len } => self
                .okta_type_to_llvm(inner)
                .array_type(*len as u32)
                .as_basic_type_enum(),
            VarType::Unknown => unimplemented!(),
        })
    }

    pub fn to_string(&self) -> String {
        self.module.print_to_string().to_string()
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
