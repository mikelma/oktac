use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

use std::sync::Arc;

use super::CodeGen;
use crate::{AstNode, VarType};

impl<'ctx> CodeGen<'ctx> {
    pub fn compile_protos(&mut self, protos: &Vec<Arc<AstNode>>) -> Result<(), String> {
        protos.iter().for_each(|p| match p.as_ref() {
            AstNode::EnumProto { name, .. } | AstNode::StructProto { name, .. } => {
                let _ = self.context.opaque_struct_type(name);
            }
            _ => (),
        });

        for proto in protos {
            match proto.as_ref() {
                AstNode::FuncProto {
                    name,
                    ret_type,
                    params,
                    inline,
                    ..
                } => self.compile_func_proto(name, params, ret_type, *inline),
                AstNode::StructProto {
                    name,
                    members,
                    packed,
                    ..
                } => self.compile_struct_proto(name, members, *packed)?,
                AstNode::EnumProto { name, variants, .. } => {
                    self.compile_enum_proto(name, variants)?
                }
                AstNode::ExternFuncProto {
                    name,
                    param_types,
                    ret_type,
                    variadic,
                    ..
                } => self.compile_extern_func_proto(name, ret_type, param_types, *variadic)?,

                AstNode::ConstVarDecl {
                    name, value, ty, ..
                } => {
                    // let global = self.module.get_global(name).unwrap();
                    let glob_val = self.module.add_global(
                        *self.okta_type_to_llvm(ty),
                        Some(AddressSpace::Generic),
                        name,
                    );

                    glob_val.set_constant(true);
                    glob_val.set_linkage(Linkage::Private);

                    self.st.register_global(&name, ty.clone(), glob_val);

                    self.global_var_init = true;
                    let compiled_val = self.compile_node(value).unwrap().unwrap();
                    self.global_var_init = false;

                    // set the global variable as locally initialized
                    glob_val.set_externally_initialized(false);
                    glob_val.set_initializer(&compiled_val);
                }
                // type alias prototypes are not compiled, they are just an abstraction for the
                // user. Aliases get resolved by `okta_type_to_llvm` into their undelying type.
                AstNode::AliasProto { .. } => (),
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
        inline: bool,
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

        let fn_val = self.module.add_function(name, fn_type, None);

        if inline {
            let inline_attr_id = Attribute::get_named_enum_kind_id("alwaysinline");
            let inline_attr = self.context.create_enum_attribute(inline_attr_id, 1);
            fn_val.add_attribute(AttributeLoc::Function, inline_attr);
        }

        // register the function in the symbol table
        let fn_ty = VarType::Fun {
            ret_ty: ret_type.clone().map(|v| Box::new(v)),
            param_ty: params.iter().map(|(_, t)| t.clone()).collect(),
        };

        self.st.register_function(name, fn_ty, fn_val);
    }

    pub(super) fn compile_struct_proto(
        &self,
        name: &str,
        members: &[(String, VarType)],
        packed: bool,
    ) -> Result<(), String> {
        let opaque = self.module.get_struct_type(name).unwrap();
        let field_types = members
            .iter()
            .map(|(_, ty)| *self.okta_type_to_llvm(ty))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();
        opaque.set_body(&field_types, packed);

        Ok(())
    }

    pub(super) fn compile_extern_func_proto(
        &mut self,
        name: &str,
        ret_type: &Option<VarType>,
        param_types: &[VarType],
        variadic: bool,
    ) -> Result<(), String> {
        // create function header
        let arg_types: Vec<BasicMetadataTypeEnum<'ctx>> = param_types
            .iter()
            .map(|ty| BasicMetadataTypeEnum::from(*self.okta_type_to_llvm(ty)))
            .collect();

        let fn_type = match ret_type {
            Some(ty) => self.okta_type_to_llvm(ty).fn_type(&arg_types, variadic),
            None => self.context.void_type().fn_type(&arg_types, variadic),
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
