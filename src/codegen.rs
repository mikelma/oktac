use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::targets::TargetTriple;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::{
    AnyValueEnum, BasicValue, IntMathValue, BasicValueEnum,
    IntValue, PointerValue, FunctionValue, GlobalValue
};
use inkwell::types::BasicType;
use inkwell::AddressSpace;
// use inkwell::execution_engine::{ExecutionEngine, JitFunction};
// use inkwell::OptimizationLevel;
use either::Either;

use crate::ast::*;
use std::collections::HashMap;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    // execution_engine: ExecutionEngine<'ctx>,
    //
    variables: HashMap<String, PointerValue<'ctx>>,
    curr_func: FunctionValue<'ctx>,
    // functions: HashMap<String, FunctionValue<'ctx>>,
    global_print_str: GlobalValue<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &Context) -> CodeGen {
        // create main module
        let module = context.create_module("main");
        let builder = context.create_builder();

        // set target triple
        let triple = TargetTriple::create("x86_64-pc-linux-gnu");
        module.set_triple(&triple);

        // declare external functions
        CodeGen::declare_externals(&context, &module);

        // create main function
        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_val = module.add_function("main", fn_type, None);

        // create entry Basic Block
        let entry = context.append_basic_block(fn_val, "entry");
        builder.position_at_end(entry);

        // return of main
        let return_type = context.i32_type().const_int(0, false);
        let ret_instr = builder.build_return(Some(&return_type));

        builder.position_before(&ret_instr);

        let global_print_str = builder.build_global_string_ptr("%d\n", "my_str");


        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            // functions: HashMap::new(),
            global_print_str,
            curr_func: fn_val,
        }
    }

    fn declare_externals<'a>(context: &'ctx Context, module: &Module<'ctx>) {
        let fn_type = context.i32_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()], true);
        let _fn_val = module.add_function("printf", fn_type, Some(Linkage::External));
        // self.functions.insert("print".to_string(), fn_val);
    }


    fn create_entry_block_alloca<T: BasicType<'ctx>>(&self, name: &str, var_type: T) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.curr_func.get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry)
        }

        builder.build_alloca(var_type, name)
    }

    pub fn compile<'a>(&'a mut self, node: &AstNode) -> Result<AnyValueEnum<'ctx>, &'static str> {
        match node {
            AstNode::MathExpr {left: lhs, op: operator, right: rhs} => self.compile_math_expr(lhs, operator, rhs),
            AstNode::PrintExpr(expr) => self.compile_print_expr(expr),
            AstNode::AssignExpr {left: lhs, right: rhs} => {
                if let AstNode::Identifyer(id) = &**lhs {
                    self.compile_assign(&id, rhs)
                } else { unreachable!(); }
            },
            AstNode::Integer(_) | AstNode::Identifyer(_) => self.compile_value(node),
        }
    }

    fn compile_print_expr<'a>(&'a mut self, inner: &AstNode) -> Result<AnyValueEnum<'ctx>, &'static str> {
        let value = self.compile(inner)?;    
        let printf_val = self.module.get_function("printf").unwrap();
        let printf_args = vec![
            self.global_print_str.as_basic_value_enum(),
            BasicValueEnum::IntValue(value.into_int_value()),
        ];
        let call = self.builder.build_call(printf_val, &printf_args, "printf_ret");
        Ok(match call.try_as_basic_value() {
            Either::Left(bv) => AnyValueEnum::IntValue(bv.into_int_value()),
            _ => unreachable!(),
            // Either::Right(instr) => AnyValueEnum::InstructionValue(instr),
        })
    }

    fn compile_value<'a>(&'a mut self, node: &AstNode) -> Result<AnyValueEnum<'ctx>, &'static str> {
        match node {
            AstNode::Identifyer(id) => {
                Ok(AnyValueEnum::IntValue(
                    self.builder.build_load(*self.variables.get(id).expect("Undefined variable"), "tmpload").into_int_value(),
                ))
            }
            AstNode::Integer(val) => Ok(AnyValueEnum::IntValue(
                self.context.i32_type().const_int(*val as u64, true),
            )),
            _ => unreachable!(),
        }
    }

    fn compile_assign<'a>(&'a mut self, id: &str, 
                               rhs: &AstNode) -> Result<AnyValueEnum<'ctx>, &'static str> {
        let rhs = self.compile(rhs)?;
        // get the pointer of the left hand side variable
        let lptr = match self.variables.get(id) {
            Some(ptr) => ptr,
            None => {
                // let ptr = self.builder.build_alloca(self.context.i32_type(), &id);
                let ptr = self.create_entry_block_alloca(id, self.context.i32_type());
                self.variables.insert(id.to_string(), ptr);
                self.variables.get(id).unwrap()
            },
        };
        // store the value of rhs to lhs
        let instr = self.builder.build_store(*lptr, rhs.into_int_value());
        Ok(AnyValueEnum::InstructionValue(instr))
    }

    fn compile_math_expr<'a>(&'a mut self, lhs: &AstNode, op: &BinaryOp, rhs: &AstNode) -> Result<AnyValueEnum<'ctx>, &'static str> {
        let lhs = self.compile(lhs)?;
        let rhs = self.compile(rhs)?;
            
        Ok(AnyValueEnum::IntValue(match op {
            BinaryOp::Add => self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "tmpadd"),
            BinaryOp::Subtract => self.builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "tmpsub"),
            BinaryOp::Multiply => self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "tmpmul"),
            BinaryOp::Divide => self.builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "tmpdiv"),
        }))
    }

    pub fn print(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
