use inkwell::values::BasicValue;

use super::{get_value_from_result, CodeGen, CompRet};
use crate::AstNode;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_builtin_func(
        &mut self,
        fn_name: &str,
        args: &[AstNode],
    ) -> CompRet<'ctx> {
        match fn_name {
            "@sizeof" => self.compile_call_sizeof(&args[0]),
            "@bitcast" => self.compile_call_bitcast(&args[0], &args[1]),
            _ => unreachable!(),
        }
    }

    fn compile_call_sizeof(&self, ty: &AstNode) -> CompRet<'ctx> {
        let ty = match ty {
            AstNode::Type(t) => t,
            _ => unreachable!(),
        };

        let size = self.context.i16_type().const_int(ty.size() as u64, false);

        Ok(Some(size.as_basic_value_enum()))
    }

    fn compile_call_bitcast(&mut self, value: &AstNode, ty: &AstNode) -> CompRet<'ctx> {
        let value = get_value_from_result(&self.compile_node(value)?)?;
        let ty = match ty {
            AstNode::Type(t) => self.okta_type_to_llvm(t),
            _ => unreachable!(),
        };

        Ok(Some(self.builder.build_bitcast(value, *ty, "bitcast")))
    }
}
