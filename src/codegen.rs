use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::targets::TargetTriple;
use inkwell::values::{
    BasicValue, BasicValueEnum, IntValue, 
    PointerValue, FunctionValue, GlobalValue,
};
use inkwell::types::BasicType;
use inkwell::{AddressSpace, IntPredicate};

use either::Either;

use std::collections::HashMap;

use crate::{ast::*, VarType};

type CompRet<'ctx> = Result<Option<BasicValueEnum<'ctx>>, String>;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    // execution_engine: ExecutionEngine<'ctx>,
    //
    variables: HashMap<String, PointerValue<'ctx>>,
    curr_func: Option<FunctionValue<'ctx>>,
    // functions: HashMap<String, FunctionValue<'ctx>>,
    // global_print_str: GlobalValue<'ctx>,
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

        // create main function
        // let i32_type = context.i32_type();
        // let fn_type = i32_type.fn_type(&[], false);
        // let fn_val = module.add_function("main", fn_type, None);

        // create entry Basic Block
        // let entry = context.append_basic_block(fn_val, "entry");
        // builder.position_at_end(entry);

        // let global_print_str = builder.build_global_string_ptr("%d\n", "my_str");

        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            // functions: HashMap::new(),
            // global_print_str,
            curr_func: None,
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
        let i32_type = self.context.i32_type(); // TODO
        let fn_type = i32_type.fn_type(&[], false);
        let fn_val = self.module.add_function(name, fn_type, None);

        // create entry Basic Block
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        
        self.curr_func = Some(fn_val);
        let _ = self.compile(stmts)?;
        Ok(None)
    }

    fn compile_var_decl_expr(&mut self, id: &str, 
                               var_type: &VarType, value: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile(value)?)?;

        // allocate variable
        let ptr = self.create_entry_block_alloca(id, self.context.i32_type());
        self.variables.insert(id.to_string(), ptr);
        self.variables.get(id).unwrap();

        // store the value into the variable
        let _instr = self.builder.build_store(ptr, value.into_int_value());

        Ok(None)
    }

    // fn compile_math_expr(&mut self, lhs: &AstNode, 
    //                          op: &MathOp, rhs: &AstNode) -> CompRet<'ctx> {
    //     let lhs = basic_to_int_value(&get_value_from_result(&self.compile(lhs)?)?)?;
    //     let rhs = basic_to_int_value(&get_value_from_result(&self.compile(rhs)?)?)?;
    //         
    //     Ok(Some(BasicValueEnum::IntValue(match op {
    //         MathOp::Add => self.builder.build_int_add(lhs, rhs, "tmpadd"),
    //         MathOp::Subtract => self.builder.build_int_sub(lhs, rhs, "tmpsub"),
    //         MathOp::Multiply => self.builder.build_int_mul(lhs, rhs, "tmpmul"),
    //         MathOp::Divide => self.builder.build_int_signed_div(lhs, rhs, "tmpdiv"),
    //     })))
    // }
        
    fn compile_binary_expr(&mut self, lhs: &AstNode, 
                             op: &BinaryOp, rhs: &AstNode) -> CompRet<'ctx> {
        let lhs = basic_to_int_value(&get_value_from_result(&self.compile(lhs)?)?)?;
        let rhs = basic_to_int_value(&get_value_from_result(&self.compile(rhs)?)?)?;
            
        Ok(Some(BasicValueEnum::IntValue(match op {
            BinaryOp::Add => self.builder.build_int_add(lhs, rhs, "tmpadd"),
            BinaryOp::Subtract => self.builder.build_int_sub(lhs, rhs, "tmpsub"),
            BinaryOp::Multiply => self.builder.build_int_mul(lhs, rhs, "tmpmul"),
            BinaryOp::Divide => self.builder.build_int_signed_div(lhs, rhs, "tmpdiv"),
            BinaryOp::Or => self.builder.build_or(lhs, rhs, "tmpor"),
            BinaryOp::And => self.builder.build_and(lhs, rhs, "tmpand"),
            BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcomp"),
            BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcomp"),
            BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcomp"),
            BinaryOp::Leq => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcomp"),
            BinaryOp::Geq => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcomp"),
        })))
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
        let lptr = match self.variables.get(id) {
            Some(ptr) => ptr,
            None => {
                return Err(format!("Variable `{}` was not declared in this scope", id));
            },
        };
        // store the value of rhs to lhs
        let _instr = self.builder.build_store(*lptr, rhs.into_int_value());
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
        let ret_val: BasicValueEnum<'ctx> = get_value_from_result(&self.compile(expr)?)?;
        let _ret_instr = self.builder.build_return(Some(&ret_val));
        Ok(None)
    }

    fn compile_ifelse_expr(&mut self, cond: &AstNode, 
                               true_b: &AstNode, false_b: &AstNode) -> CompRet<'ctx> {
        let zero_const = self.context.i32_type().const_int(0, false);

        let cond = basic_to_int_value(&get_value_from_result(&self.compile(cond)?)?)?;
        let cond = self.builder.build_int_compare(IntPredicate::NE, cond, zero_const, "ifcond");

        let current_fn = self.curr_func.unwrap();
        let then_bb = self.context.append_basic_block(current_fn, "then");
        let else_bb = self.context.append_basic_block(current_fn, "else");
        let cont_bb = self.context.append_basic_block(current_fn, "ifcont");

        self.builder.build_conditional_branch(cond, then_bb, else_bb);

        // build true block
        self.builder.position_at_end(then_bb);
        // TODO
        // let then_val = any_to_basic_value(self.compile(true_b)?)?;
        let _then_val = self.compile(true_b)?;
        let then_val = self.context.i32_type().const_int(0, true);
        self.builder.build_unconditional_branch(cont_bb);
        let then_bb = self.builder.get_insert_block().unwrap();

        // build false block
        self.builder.position_at_end(else_bb);
        // TODO
        // let else_val = any_to_basic_value(self.compile(false_b)?)?;
        let _else_val = self.compile(false_b)?;
        let else_val = self.context.i32_type().const_int(1, true);
        self.builder.build_unconditional_branch(cont_bb);
        let else_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cont_bb);

        let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");
        phi.add_incoming(&[
                         (&then_val, then_bb),
                         (&else_val, else_bb)
        ]);

        Ok(Some(phi.as_basic_value()))
    }

    /*
    fn compile_print_expr(&mut self, inner: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile(inner)?)?;    
        let printf_val = self.module.get_function("printf").unwrap();
        let printf_args = vec![
            self.global_print_str.as_basic_value_enum(),
            value.as_basic_value_enum(),
        ];
        let call = self.builder.build_call(printf_val, &printf_args, "printf_ret");

        Ok(match call.try_as_basic_value() {
            Either::Left(bv) => Some(bv),
            _ => unreachable!(),
            // Either::Right(instr) => AnyValueEnum::InstructionValue(instr),
        })
    }
    */

    fn compile_value(&mut self, node: &AstNode) -> CompRet<'ctx> {
        Ok(Some(match node {
            AstNode::Identifyer(id) => {
                BasicValueEnum::IntValue(
                    self.builder.build_load(*self.variables.get(id).expect("Undefined variable"), "tmpload").into_int_value(),
                )
            }
            AstNode::Integer(val) => BasicValueEnum::IntValue(
                self.context.i32_type().const_int(*val as u64, true),
            ),
            AstNode::Boolean(val) => BasicValueEnum::IntValue(
                self.context.bool_type().const_int(*val as u64, false),
            ),
            _ => unreachable!(),
        }))
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

    pub fn print(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

pub fn get_value_from_result<'a>(value: &Option<BasicValueEnum<'a>>) -> Result<BasicValueEnum<'a>, String> {
    match value {
        Some(v) => Ok(*v),
        None => Err("Expression does not return any value.".to_string()),
    }
}

pub fn basic_to_int_value<'ctx>(value: &dyn BasicValue<'ctx>) -> Result<IntValue<'ctx>, String> {
    match value.as_basic_value_enum() {
        BasicValueEnum::IntValue(val) => Ok(val),
        _ => Err("Cannot convert basic value to int value".to_string()),
    }
}
