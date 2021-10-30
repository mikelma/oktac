use inkwell::module::Linkage;
use inkwell::types::{BasicType, BasicTypeEnum, BasicMetadataTypeEnum};

use super::CodeGen;

use crate::{AstNode, VarType};

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_protos(&mut self, protos: &[AstNode]) -> Result<(), String> {
        protos.iter().for_each(|p| match p {
            AstNode::EnumProto { name, .. } | AstNode::StructProto { name, .. } => {
                let _ = self.context.opaque_struct_type(name);
            }
            _ => (),
        });

        for proto in protos {
            match proto {
                AstNode::FuncProto {
                    name,
                    ret_type,
                    params,
                    ..
                } => self.compile_func_proto(name, params, ret_type),
                AstNode::StructProto { name, members, .. } => {
                    self.compile_struct_proto(name, members)?
                }
                AstNode::EnumProto { name, variants, .. } => {
                    self.compile_enum_proto(name, variants)?
                }
                AstNode::ExternFuncProto {
                    name,
                    param_types,
                    ret_type,
                } => self.compile_extern_func_proto(name, ret_type, param_types)?,
                _ => unreachable!("Node {:?} is not a prototype", proto),
            }
        }
        Ok(())
    }

    pub(super) fn compile_func_proto(
        &mut self,
        name: &str,
        params: &[(String, VarType)],
        ret_type: &Option<VarType>,
    ) {
        // create function header
        let args: Vec<BasicMetadataTypeEnum<'ctx>> = params
            .iter()
            .map(|(_, ty)| BasicMetadataTypeEnum::from(*self.okta_type_to_llvm(ty)))
            .collect();

        let fn_type = match ret_type {
            Some(ty) => self.okta_type_to_llvm(ty).fn_type(&args, false),
            None => self.context.void_type().fn_type(&args, false),
        };

        let _ = self.module.add_function(name, fn_type, None);
    }

    pub(super) fn compile_struct_proto(
        &self,
        name: &str,
        members: &[(String, VarType)],
    ) -> Result<(), String> {
        let opaque = self.module.get_struct_type(name).unwrap();
        let field_types = members
            .iter()
            .map(|(_, ty)| *self.okta_type_to_llvm(ty))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();
        opaque.set_body(&field_types, true); // TODO: packed?? Handle data alignment

        Ok(())
    }

    pub(super) fn compile_extern_func_proto(
        &mut self,
        name: &str,
        ret_type: &Option<VarType>,
        param_types: &[VarType],
    ) -> Result<(), String> {
        // create function header
        let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = param_types
            .iter()
            .map(|ty| BasicMetadataTypeEnum::from(*self.okta_type_to_llvm(ty)))
            .collect();
        let fn_type = match ret_type {
            Some(ty) => self.okta_type_to_llvm(ty).fn_type(&arg_types, false),
            None => self.context.void_type().fn_type(&arg_types, false),
        };

        let _fn_val = self
            .module
            .add_function(name, fn_type, Some(Linkage::External));

        Ok(())
    }

    pub(super) fn compile_enum_proto(
        &mut self,
        name: &str,
        variants: &[(String, Vec<(String, VarType)>)],
    ) -> Result<(), String> {
        let opaque = self.module.get_struct_type(name).unwrap();

        // println!("Hey!");
        // dbg!(ST.lock().unwrap());

        // get the largest variant size
        let max_size = variants
            .iter()
            .map(|(_, variants)| variants.iter().map(|(_, ty)| ty.size()).sum())
            .max()
            .unwrap_or(0);

        let index_ty = self.context.i8_type().as_basic_type_enum();
        let field_types = [
            index_ty,
            self.context
                .i8_type()
                .array_type(max_size as u32)
                .as_basic_type_enum(),
        ];

        opaque.set_body(&field_types, true); // TODO: packed?? Handle data alignment

        for (var_id, var_fields) in variants {
            let var_opaque = self
                .context
                .opaque_struct_type(format!("{}.{}", name, var_id).as_str());

            let mut body = vec![self.context.i8_type().as_basic_type_enum()];
            let mut field_types = var_fields
                .iter()
                .map(|(_, ty)| *self.okta_type_to_llvm(ty))
                .collect::<Vec<BasicTypeEnum<'ctx>>>();
            body.append(&mut field_types);
            var_opaque.set_body(&body, false);
        }

        Ok(())
    }
}
