use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::BasicValue,
};

use super::{get_value_from_result, CodeGen, CompRet, VarType};
use crate::AstNode;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_builtin_func(
        &mut self,
        fn_name: &str,
        args: &[AstNode],
        ret_ty: &Option<VarType>,
    ) -> CompRet<'ctx> {
        match fn_name {
            "@sizeof" => self.compile_sizeof(&args[0]),
            "@bitcast" => self.compile_bitcast(&args[0], &args[1]),
            "@cstr" => self.compile_cstr(&args[0]),
            "@slice" => self.compile_slice(&args[0], &args[1], ret_ty),
            "@len" => self.compile_len(&args[0], ret_ty),
            "@inttoptr" => self.compile_inttoptr(&args[0], &args[1]),
            "@ptrtoint" => self.compile_ptrtoint(&args[0], &args[1]),
            _ => unreachable!(),
        }
    }

    fn compile_sizeof(&self, ty: &AstNode) -> CompRet<'ctx> {
        let ty = match ty {
            AstNode::Type(t) => t,
            _ => unreachable!(),
        };

        let size = self.okta_type_to_llvm(ty).size_of().unwrap();
        Ok(Some(size.as_basic_value_enum()))
    }

    fn compile_bitcast(&mut self, value: &AstNode, ty: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile_node(value)?)?;
        let ty = match ty {
            AstNode::Type(t) => self.okta_type_to_llvm(t),
            _ => unreachable!(),
        };

        Ok(Some(self.builder.build_bitcast(value, *ty, "bitcast")))
    }

    fn compile_cstr(&mut self, value: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile_node(value)?)?;
        let ptr = self.create_entry_block_alloca("str.tmp", *self.okta_type_to_llvm(&VarType::Str));
        self.builder.build_store(ptr, value);

        let gep_ptr = self.builder.build_struct_gep(ptr, 0, "str.gep").unwrap();

        let ptr = self.builder.build_load(gep_ptr, "str.cstr");

        Ok(Some(ptr.as_basic_value_enum()))
    }

    fn compile_slice(
        &mut self,
        ref_value: &AstNode,
        len_value: &AstNode,
        fn_ret: &Option<VarType>,
    ) -> CompRet<'ctx> {
        let ref_value = get_value_from_result(&self.compile_node(ref_value)?)?;
        let len_value = get_value_from_result(&self.compile_node(len_value)?)?;
        let slice_ty = self.okta_type_to_llvm(fn_ret.as_ref().unwrap());

        let slice = self.create_entry_block_alloca("slice", *slice_ty);

        let slice_ptr = self
            .builder
            .build_struct_gep(slice, 0, "slice.ptr")
            .unwrap();

        self.builder.build_store(slice_ptr, ref_value);

        let slice_len = self
            .builder
            .build_struct_gep(slice, 1, "slice.len")
            .unwrap();

        self.builder.build_store(slice_len, len_value);

        let slice_val = self.builder.build_load(slice, "slice.deref");

        Ok(Some(slice_val.as_basic_value_enum()))
    }

    fn compile_len(&mut self, value: &AstNode, _ret_ty: &Option<VarType>) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile_node(value)?)?;
        match value.get_type() {
            BasicTypeEnum::StructType(_) => Ok(Some(
                self.builder
                    .build_extract_value(value.into_struct_value(), 1, "slice.len")
                    .unwrap()
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::ArrayType(arr_ty) => {
                let len = self
                    .context
                    .i64_type()
                    .const_int(arr_ty.len() as u64, false);
                Ok(Some(len.as_basic_value_enum()))
            }
            _ => unreachable!(),
        }
    }

    fn compile_inttoptr(&mut self, value: &AstNode, ty: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile_node(value)?)?;
        let ty = match ty {
            AstNode::Type(t) => self.okta_type_to_llvm(t),
            _ => unreachable!(),
        }
        .into_pointer_type();

        Ok(Some(
            self.builder
                .build_int_to_ptr(value.into_int_value(), ty, "int2ptr")
                .as_basic_value_enum(),
        ))
    }

    fn compile_ptrtoint(&mut self, ref_val: &AstNode, ty: &AstNode) -> CompRet<'ctx> {
        let ref_val = get_value_from_result(&self.compile_node(ref_val)?)?;
        let ty = match ty {
            AstNode::Type(t) => self.okta_type_to_llvm(t),
            _ => unreachable!(),
        }
        .into_int_type();

        Ok(Some(
            self.builder
                .build_ptr_to_int(ref_val.into_pointer_value(), ty, "ptr2int")
                .as_basic_value_enum(),
        ))
    }
}
