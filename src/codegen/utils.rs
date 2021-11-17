use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use crate::VarType;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn create_entry_block_alloca<T: BasicType<'ctx>>(
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

    pub(super) fn create_basic_block(&self, name: &'static str) -> BasicBlock<'ctx> {
        match self.curr_fn_ret_bb {
            Some(bb) => self.context.prepend_basic_block(bb, name),
            None => self
                .context
                .append_basic_block(self.curr_func.unwrap(), name),
        }
    }

    pub(super) fn okta_type_to_llvm(&self, var_type: &VarType) -> Box<BasicTypeEnum<'ctx>> {
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
            VarType::Unknown => unreachable!(),
            VarType::Struct(name) => self
                .module
                .get_struct_type(name)
                .unwrap()
                .as_basic_type_enum(),
            VarType::Ref(ty) => self
                .okta_type_to_llvm(ty)
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            VarType::Enum(name) => self
                .module
                .get_struct_type(name)
                .unwrap()
                .as_basic_type_enum(),
            VarType::CVoidRef => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            // _ => todo!(),
        })
    }
}
