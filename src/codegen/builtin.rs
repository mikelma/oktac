use inkwell::values::BasicValue;

use super::{CodeGen, CompRet};
use crate::AstNode;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_builtin_func(&self, fn_name: &str, args: &[AstNode]) -> CompRet<'ctx> {
        match fn_name {
            "@sizeof" => self.compile_call_sizeof(&args[0]),
            _ => unreachable!(),
        }
    }

    fn compile_call_sizeof(&self, ty: &AstNode) -> CompRet<'ctx> {
        let ty = match ty {
            AstNode::Type(t) => t,
            _ => unreachable!(),
        };

        let size = self.context.i16_type()
                               .const_int(ty.size() as u64, false);

        Ok(Some(size.as_basic_value_enum()))
    }
}
