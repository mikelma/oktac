use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::targets::TargetTriple;
use inkwell::values::{
    BasicValue, BasicValueEnum, IntValue, PhiValue,
    PointerValue, FunctionValue, GlobalValue, IntMathValue
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::{AddressSpace, IntPredicate};
use inkwell::basic_block::BasicBlock;

use either::Either;

use std::collections::HashMap;

use crate::{ast::*, VarType};

const FN_RET_BB: &'static str = "ret.bb";

type CompRet<'ctx> = Result<Option<BasicValueEnum<'ctx>>, String>;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    // execution_engine: ExecutionEngine<'ctx>,
    //
    variables: HashMap<String, (VarType, PointerValue<'ctx>)>,
    curr_func: Option<FunctionValue<'ctx>>,
    curr_fn_ret_val: Option<PointerValue<'ctx>>,
    curr_fn_ret_bb:  Option<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> CodeGen {
        // create main module
        let module = context.create_module("main");
        let builder = context.create_builder();

        // set target triple
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        module.set_triple(&triple);

        // declare external functions
        CodeGen::declare_externals(&context, &module);

        // let global_print_str = builder.build_global_string_ptr("%d\n", "my_str");

        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            // functions: HashMap::new(),
            // global_print_str,
            curr_func: None,
            // curr_fn_phi_vals: None,
            // curr_ret_phi: None,
            curr_fn_ret_val: None,
            curr_fn_ret_bb: None,
        }
    }

    fn declare_externals(context: &'ctx Context, module: &Module<'ctx>) {
        let fn_type = context.i32_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()], true);
        let _fn_val = module.add_function("printf", fn_type, Some(Linkage::External));
        // self.functions.insert("print".to_string(), fn_val);
    }

    fn create_entry_block_alloca<T: BasicType<'ctx>>(&self, name: &str, var_type: T) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.curr_func.unwrap().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry)
        }

        builder.build_alloca(var_type, name)
    }

    pub fn compile(&mut self, node: &AstNode) -> CompRet<'ctx> {
        match node {
            AstNode::FuncDecl { name, ret_type, params, stmts } => self.compile_func_decl(name, ret_type, params, stmts),
            AstNode::Stmts(exprs) => {
                for expr in exprs {
                    let _ = self.compile(expr)?;
                }
                // TODO: Returns?
                Ok(None)
            },
            AstNode::VarDeclExpr {id, var_type, value} => self.compile_var_decl_expr(id, var_type, value),
            AstNode::BinaryExpr {left, op, right} => self.compile_binary_expr(left, op, right),
            AstNode::UnaryExpr {op: operator, value} => self.compile_unary_expr(operator, value),
            AstNode::AssignExpr {left: lhs, right: rhs} => {
                if let AstNode::Identifyer(id) = &**lhs {
                    self.compile_assign(&id, rhs)
                } else { unreachable!(); }
            },
            AstNode::FunCall { name, params } => self.compile_func_call(name, params),
            AstNode::IfElseExpr {cond, true_b, false_b} => self.compile_ifelse_expr(cond, true_b, false_b),
            AstNode::ReturnExpr(expr) => self.compile_return_expr(expr),
            AstNode::Integer(_) 
                | AstNode::Identifyer(_) 
                | AstNode::Boolean(_) => self.compile_value(node),
            // AstNode::ForExpr { pattern, iter, block } => self.compile_for_expr(pattern, iter, block),
            _ => unimplemented!(),
        }
    }

    fn compile_func_decl(&mut self, name: &String, ret_type: &VarType, 
                         params: &Vec<(String, VarType)>, stmts: &AstNode) -> CompRet<'ctx> {
        // create function header
        let args: Vec<BasicTypeEnum<'ctx>> = params.iter().map(|(_, ty)| *self.okta_type_to_llvm(ty)).collect();
        let fn_type = self.okta_type_to_llvm(ret_type).fn_type(&args, false);
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

            self.variables.insert(arg_name.to_string(), (var_type.clone(), alloca));
        }

        // allocate the variable to hold the return value
        self.curr_fn_ret_val = Some(self.create_entry_block_alloca("ret.val", 
                                                     *self.okta_type_to_llvm(ret_type)));

        // compile function's body
        let _ = self.compile(stmts)?;

        // create return statement 
        if let Some(ret_ptr) = self.curr_fn_ret_val {
            self.builder.position_at_end(self.curr_fn_ret_bb.unwrap());
            let val = self.builder.build_load(ret_ptr, "ret.val");
            self.builder.build_return(Some(&val));
        }

        // DEBUG: produce .dot file
        // fn_val.view_function_cfg();

        Ok(None)
    }

    fn compile_var_decl_expr(&mut self, id: &str, 
                               var_type: &VarType, value: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile(value)?)?;

        // allocate variable
        let ptr = self.create_entry_block_alloca(id, *self.okta_type_to_llvm(var_type));
        self.variables.insert(id.to_string(), (var_type.clone(), ptr));
        self.variables.get(id).unwrap();

        // store the value into the variable
        let _instr = self.builder.build_store(ptr, value.into_int_value());

        Ok(None)
    }

    fn compile_binary_expr(&mut self, lhs: &AstNode, 
                             op: &BinaryOp, rhs: &AstNode) -> CompRet<'ctx> {
        let lhs = get_value_from_result(&self.compile(lhs)?)?;
        let rhs = get_value_from_result(&self.compile(rhs)?)?;

        let lty = lhs.get_type(); 
        let rty = lhs.get_type(); 

        if lty != rty {
            return Err(format!("Expression has incompatible types: left is {:?} and right is {:?}",
                               rty, lty));
        }
        // check different int types
        if lty.is_int_type() { 
        }

        Ok(Some(match lty {
            BasicTypeEnum::IntType(_) => {
                let l_width = lty.into_int_type().get_bit_width();
                let r_width = rty.into_int_type().get_bit_width();

                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();

                if l_width != r_width {
                    return Err(
                        format!("Expression has incompatible int: left is int{} and right is int{}",
                                    l_width, r_width));
                }

                BasicValueEnum::IntValue(match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs, rhs, "tmpadd"),
                    BinaryOp::Subtract => self.builder.build_int_sub(lhs, rhs, "tmpsub"),
                    BinaryOp::Multiply => self.builder.build_int_mul(lhs, rhs, "tmpmul"),
                    BinaryOp::Divide => self.builder.build_int_signed_div(lhs, rhs, "tmpdiv"),
                    BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcomp"),
                    BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcomp"),
                    BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcomp"),
                    BinaryOp::Leq => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcomp"),
                    BinaryOp::Geq => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcomp"),
                    // only for boolean type
                    BinaryOp::Or if l_width == 1 => self.builder.build_or(lhs, rhs, "tmpor"),
                    BinaryOp::And if l_width == 1 => self.builder.build_and(lhs, rhs, "tmpand"),
                    _ => return Err(format!("{:?} is not implemented for int{} type", op, l_width)),
                })
            },
            _ => unimplemented!(),
        }))
    }

    fn compile_unary_expr(&mut self, op: &UnaryOp, value: &AstNode) -> CompRet<'ctx> {
        let value = basic_to_int_value(&get_value_from_result(&self.compile(value)?)?)?;
            
        Ok(Some(BasicValueEnum::IntValue(match op {
            UnaryOp::Not => self.builder.build_not(value, "tmpnot"),
        })))
    }

    fn compile_assign(&mut self, id: &str, 
                               rhs: &AstNode) -> CompRet<'ctx> {
        let rhs = get_value_from_result(&self.compile(rhs)?)?;

        // get the pointer of the left hand side variable
        let (_var_type, lptr) = match self.variables.get(id) {
            Some(val) => val,
            None => {
                return Err(format!("Variable `{}` was not declared in this scope", id));
            },
        };
        // store the value of rhs to lhs
        let _instr = self.builder.build_store(*lptr, rhs);
        Ok(None)
    }

    fn compile_func_call(&mut self, name: &str, params: &Vec<AstNode>) -> CompRet<'ctx> {
        let func = self.module.get_function(name).expect("Cannot find function in module");

        let args: Vec<BasicValueEnum> = params.iter()
            .map(|node| self.compile(node)
                    .unwrap()
                    .expect("Non valued expression as function argument")).collect();

        let call = self.builder.build_call(func, &args, "fcall");

        Ok(match call.try_as_basic_value() {
            Either::Left(bv) => Some(bv),
            _ => unreachable!(),
            // Either::Right(instr) => AnyValueEnum::InstructionValue(instr),
        })
    }

    fn compile_return_expr(&mut self, expr: &AstNode) -> CompRet<'ctx> {
        if let Some(ret_ptr) = self.curr_fn_ret_val {
            let ret_val = get_value_from_result(&self.compile(expr)?)?;
            self.builder.build_store(ret_ptr, ret_val);
            self.builder.build_unconditional_branch(self.curr_fn_ret_bb.unwrap());
        }
        // let ret_val: BasicValueEnum<'ctx> = get_value_from_result(&self.compile(expr)?)?;
        // let _ret_instr = self.builder.build_return(Some(&ret_val));
        
        // let tpl = (ret_val, self.builder.get_insert_block().unwrap());
        // match &mut self.curr_ret_phi {
        //     Some(v) => v.add_incoming(&[tpl]),
        //     None => unimplemented!(),
        // }

        Ok(None)
    }

    fn compile_ifelse_expr(&mut self, cond: &AstNode, 
                               true_b: &AstNode, false_b: &AstNode) -> CompRet<'ctx> {
        // compile condition
        let cond = basic_to_int_value(&get_value_from_result(&self.compile(cond)?)?)?;


        let func_val = self.curr_func.unwrap();

        let (then_bb, else_bb) = match self.curr_fn_ret_bb {
            Some(bb) => (self.context.prepend_basic_block(bb, "if.then"),
                         self.context.prepend_basic_block(bb, "if.else")),
            None => (self.context.append_basic_block(func_val, "if.then"),
                     self.context.append_basic_block(func_val, "if.else")),
        };

        self.builder.build_conditional_branch(cond, then_bb, else_bb);

        // build true block
        self.builder.position_at_end(then_bb);
        let _then_val = self.compile(true_b)?;

        // build false block
        self.builder.position_at_end(else_bb);
        let _else_val = self.compile(false_b)?;

        if !(stmts_contains_return(false_b) 
             && stmts_contains_return(false_b)) {
            let cont_bb = match self.curr_fn_ret_bb {
                Some(bb) => self.context.prepend_basic_block(bb, "if.cont"),
                None => self.context.append_basic_block(func_val, "if.cont"),
            };

            self.builder.position_at_end(then_bb);
            if !stmts_contains_return(true_b) {
                self.builder.build_unconditional_branch(cont_bb);
            }

            self.builder.position_at_end(else_bb);
            if !stmts_contains_return(false_b) {
                self.builder.build_unconditional_branch(cont_bb);
            }

            self.builder.position_at_end(cont_bb);
        }

        // let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");
        // phi.add_incoming(&[
        //                  (&then_val, then_bb),
        //                  (&else_val, else_bb)
        // ]);

        // Ok(Some(phi.as_basic_value()))
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
            AstNode::Integer(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.i32_type().const_int(*val as u64, true),
            ))),
            AstNode::Boolean(val) => Ok(Some(BasicValueEnum::IntValue(
                self.context.bool_type().const_int(*val as u64, false),
            ))),
            _ => unreachable!(),
        }
    }

    /*
    fn compile_for_expr(&mut self, patt: &AstNode, iter: &Iter, block: &AstNode) -> CompRet<'ctx> {
        // allocate memory for iterator value
        let iter_var = if let AstNode::Identifyer(id) = patt {
            self.create_entry_block_alloca(id, self.context.i32_type())
        } else { unimplemented!(); };

        let (range_min, range_max) = if let Iter::IntRange(min, max) = iter {
           (self.context.i32_type().const_int(*min as u64, true), 
            self.context.i32_type().const_int(*max as u64, true))
        } else { unimplemented!(); };

        // initialize variable
        let _ = self.builder.build_store(iter_var, range_min);
        
        let cond_bb = self.context.append_basic_block(self.curr_func, "loopcond");
        let loop_bb = self.context.append_basic_block(self.curr_func, "loop");
        let end_bb  = self.context.append_basic_block(self.curr_func, "endlopp");

        self.builder.position_at_end(cond_bb);
        
        let iter_value = if let BasicValueEnum::IntValue(val) =  self.builder.build_load(
            iter_var, "tmpload") {
            val
        } else { unimplemented!(); };
        let cond = self.builder.build_int_compare(IntPredicate::SLT, iter_value, range_max, "ifcond");
        self.builder.build_conditional_branch(cond, loop_bb, end_bb);            

        // lobop body
        self.builder.position_at_end(loop_bb);
        let _ = self.compile(block)?;

        self.builder.position_at_end(end_bb);

        Ok(None) 
    }
    */

    /*
    fn compile_iterator(&mut self, iter: &AstNode) -> CompRet<'ctx> {
        match iter {
            Iter::IntRange(start, stop) =>  
        }
    }
    */

    fn okta_type_to_llvm(&self, var_type: &VarType) -> Box<BasicTypeEnum<'ctx>> {
        Box::new(match var_type {
            VarType::Int32 => self.context.i32_type(),
            VarType::Boolean => self.context.bool_type(),
        }.as_basic_type_enum())
    }

    pub fn print(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

fn get_value_from_result<'a>(value: &Option<BasicValueEnum<'a>>) -> Result<BasicValueEnum<'a>, String> {
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

fn stmts_contains_return(stmts: &AstNode) -> bool {
    if let AstNode::Stmts(list) = stmts {
        if let Some(AstNode::ReturnExpr(_)) = list.iter().last() {
            true
        } else {
            false
        }
    } else {
        unreachable!();
    }
}
