use super::*;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, CallableValue};

impl<'ctx> CodeGen<'ctx> {
    pub fn compile_func_decl(
        &mut self,
        name: &str,
        ret_type: &Option<VarType>,
        params: &[(String, VarType)],
        stmts: &AstNode,
    ) -> CompRet<'ctx> {
        // this never panics, as the function prototype is already compiled by `compile_func_proto`
        let fn_val = self.module.get_function(name).unwrap();

        // create entry Basic Block
        let entry = self.context.append_basic_block(fn_val, "entry");

        // create return basic block
        self.curr_fn_ret_bb = Some(self.context.append_basic_block(fn_val, FN_RET_BB));

        self.builder.position_at_end(entry);

        // set argument names
        for (fn_arg, arg_name) in fn_val.get_param_iter().zip(params.iter().map(|v| &v.0)) {
            fn_arg.set_name(arg_name);
        }

        // set this function as current function
        self.curr_func = Some(fn_val);

        // record function arguments in the symbol table
        //
        // NOTE: A symbol table is created before processing the statements block of the function
        // in order to register the function arguments into, as at this point the st stack might be
        // empty.
        self.st.push_table();
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let (arg_name, var_type) = &params[i];
            let arg_llvm_ty = *self.okta_type_to_llvm(&params[i].1);
            let alloca = self.create_entry_block_alloca(arg_name, arg_llvm_ty);

            self.builder.build_store(alloca, arg);

            self.st
                .register_variable(arg_name, var_type.clone(), alloca);
        }

        // allocate the variable to hold the return value
        self.curr_fn_ret_val = ret_type
            .as_ref()
            .map(|ty| self.create_entry_block_alloca("ret.val", *self.okta_type_to_llvm(ty)));

        // compile function's body
        let _ = self.compile_node(stmts)?;

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

        self.st.pop_table();

        Ok(None)
    }

    pub fn compile_binary_expr(
        &mut self,
        lhs: &AstNode,
        op: &BinaryOp,
        rhs: &AstNode,
        ty: &VarType,
    ) -> CompRet<'ctx> {
        let lhs = get_value_from_result(&self.compile_node(lhs)?)?;
        let rhs = get_value_from_result(&self.compile_node(rhs)?)?;

        let ty = ty.resolve_alias();

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
            BinaryOp::Modulo => {
                let name = "tmp.rem";
                if ty.is_signed() && ty.is_int() {
                    self.builder
                        .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), name)
                        .as_basic_value_enum()
                } else if ty.is_int() {
                    self.builder
                        .build_int_unsigned_rem(lhs.into_int_value(), rhs.into_int_value(), name)
                        .as_basic_value_enum()
                } else {
                    // is float
                    self.builder
                        .build_float_rem(lhs.into_float_value(), rhs.into_float_value(), name)
                        .as_basic_value_enum()
                }
            }
            BinaryOp::Eq => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
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
                _ => {
                    println!("Unreachable caused by: {}", ty);
                    unreachable!()
                }
            },
            BinaryOp::Ne => match ty {
                VarType::Int32
                | VarType::UInt32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Int16
                | VarType::UInt16
                | VarType::Int8
                | VarType::UInt8
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
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
                VarType::UInt32
                | VarType::UInt64
                | VarType::UInt8
                | VarType::UInt16
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::ULT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
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
                VarType::UInt32
                | VarType::UInt64
                | VarType::UInt8
                | VarType::UInt16
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::UGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
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
                VarType::Int32
                | VarType::Int64
                | VarType::Int8
                | VarType::Int16
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
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
                VarType::UInt32
                | VarType::UInt64
                | VarType::UInt8
                | VarType::UInt16
                | VarType::Boolean => BasicValueEnum::IntValue(self.builder.build_int_compare(
                    IntPredicate::UGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "tmp.cmp",
                )),
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
            // bitwise operations
            BinaryOp::BinaryAnd => self
                .builder
                .build_and(lhs.into_int_value(), rhs.into_int_value(), "tmp.and")
                .as_basic_value_enum(),
            BinaryOp::BinaryOr => self
                .builder
                .build_or(lhs.into_int_value(), rhs.into_int_value(), "tmp.and")
                .as_basic_value_enum(),
            BinaryOp::BinaryXor => self
                .builder
                .build_xor(lhs.into_int_value(), rhs.into_int_value(), "tmp.and")
                .as_basic_value_enum(),
            BinaryOp::ShiftRight => self
                .builder
                .build_right_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    ty.is_signed(),
                    "tmp.shiftR",
                )
                .as_basic_value_enum(),
            BinaryOp::ShiftLeft => self
                .builder
                .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "tmp.shiftL")
                .as_basic_value_enum(),
            // only for boolean type
            BinaryOp::Or if ty == VarType::Boolean => BasicValueEnum::IntValue(
                self.builder
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "tmp.or"),
            ),
            BinaryOp::And if ty == VarType::Boolean => BasicValueEnum::IntValue(
                self.builder
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "tmp.or"),
            ),
            _ => unreachable!(),
        }))
    }

    pub fn compile_unary_expr(
        &mut self,
        op: &UnaryOp,
        value: &AstNode,
        ty: &VarType,
    ) -> CompRet<'ctx> {
        Ok(Some(match op {
            UnaryOp::Not => {
                let value = get_value_from_result(&self.compile_node(value)?)?;
                match ty {
                    VarType::Boolean => BasicValueEnum::IntValue(
                        self.builder.build_not(value.into_int_value(), "tmp.not"),
                    ),
                    _ => unimplemented!(),
                }
            }
            UnaryOp::Deref => {
                let ptr = match get_value_from_result(&self.compile_node(value)?)? {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => unimplemented!(),
                };
                self.builder.build_load(ptr, "deref")
            }
            UnaryOp::Reference => {
                let ptr = match value {
                    // if the rvalue is an identifier, just get the ptr of the variable
                    AstNode::Identifier(name) => self.st.search_variable(name).1,
                    _ => {
                        // if the rvalue is not stored in a variable
                        let r_expr = get_value_from_result(&self.compile_node(value)?)?;
                        let expr_ty = r_expr.get_type();
                        // store the right side expression in a variable and get the pointer to
                        // that value
                        let ptr = self.create_entry_block_alloca("tmp.store", expr_ty);
                        self.builder.build_store(ptr, r_expr);
                        ptr
                    }
                };

                BasicValueEnum::PointerValue(ptr)
            }
            UnaryOp::Minus => {
                let value = get_value_from_result(&self.compile_node(value)?)?;
                if ty.is_int() {
                    self.builder
                        .build_int_nsw_neg(value.into_int_value(), "tmp.neg")
                        .as_basic_value_enum()
                } else {
                    self.builder
                        .build_float_neg(value.into_float_value(), "tmp.neg")
                        .as_basic_value_enum()
                }
            }
            UnaryOp::BinaryNot => {
                let value = get_value_from_result(&self.compile_node(value)?)?.into_int_value();
                let minus_one = self
                    .okta_type_to_llvm(ty)
                    .into_int_type()
                    .const_int((-1i64) as u64, true);
                self.builder
                    .build_xor(value, minus_one, "tmp.binNot")
                    .as_basic_value_enum()
            }
        }))
    }

    pub fn compile_func_call(&mut self, name: &str, params: &[AstNode]) -> CompRet<'ctx> {
        let func: CallableValue = match self.module.get_function(name) {
            Some(f) => CallableValue::from(f),
            None => {
                let fn_ptr = self.st.search_variable(name).1;

                CallableValue::try_from(
                    self.builder
                        .build_load(fn_ptr, "fn.deref")
                        .into_pointer_value(),
                )
                .unwrap()
            }
        };

        let args: Vec<BasicMetadataValueEnum> = params
            .iter()
            .map(|node| {
                BasicMetadataValueEnum::from(
                    self.compile_node(node)
                        .unwrap()
                        .expect("Non valued expression as function argument"),
                )
            })
            .collect();

        let call = self.builder.build_call(func, &args, "fcall");

        Ok(match call.try_as_basic_value() {
            Either::Left(l) => Some(l),
            Either::Right(_r) => None,
        })
    }

    pub fn compile_value(&mut self, node: &AstNode) -> CompRet<'ctx> {
        match node {
            AstNode::Identifier(id) => {
                if let Some(val) = self.st.search_global(id) {
                    if self.global_var_init {
                        Ok(Some(val.get_initializer().unwrap()))
                    } else {
                        Ok(Some(self.builder.build_load(val.as_pointer_value(), "tmp.load")))
                    }

                    // Ok(Some(val.get_initializer().unwrap())) 
                    // Ok(Some(val.get_initializer().unwrap())) 
                } else {
                    let (_, ptr, is_fn) = self.st.search_variable(id);

                    Ok(Some(if is_fn {
                        // if the variable is a function pointer, just return the pointer, don't
                        // dereference it 
                        ptr.as_basic_value_enum()
                    } else {
                        // dereference the variable and return it's value
                        self.builder.build_load(ptr, "tmp.load")
                    }))
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
                self.context.f32_type().const_float(val.into_inner() as f64),
            ))),
            AstNode::Float64(val) => Ok(Some(BasicValueEnum::FloatValue(
                self.context.f64_type().const_float(val.into_inner() as f64),
            ))),
            AstNode::Array { values, ty, is_const } => self.compile_array(values, ty, is_const),
            AstNode::String(bytes) => self.compile_str(bytes),
            AstNode::Strct { name, members, .. /*is_const*/ } => {
                let struct_ty = self.module.get_struct_type(name).unwrap();
                // allocate space for the value
                let strct_alloca = self.create_entry_block_alloca("tmp.strct", struct_ty);

                self.build_struct_in_ptr(strct_alloca, members)?;

                let strct = self.builder.build_load(strct_alloca, "tmp.deref");
                Ok(Some(strct))
            },
            AstNode::EnumVariant { enum_name, variant_name, tag, fields, .. } => {
                let enum_ty = self.module.get_struct_type(enum_name).unwrap();

                // allocate the enum variant as an instance of the enum 
                let enum_ptr = self.create_entry_block_alloca("tmp.enum", enum_ty);

                self.build_enum_var_in_ptr(enum_ptr, enum_name,
                                           variant_name, *tag as u8, fields).unwrap();

                // dereference enum
                let enm = self.builder.build_load(enum_ptr, "tmp.deref");
                Ok(Some(enm))
            },
            _ => unreachable!("Panic caused by {:?}", node),
        }
    }

    pub fn compile_memb_acess_expr(
        &mut self,
        parent: &AstNode,
        members: &[MemberAccess],
        access_types: &[VarType],
        parent_ty: &VarType,
    ) -> CompRet<'ctx> {
        let gep_ptr = self.compile_memb_acess_ptr(parent, members, access_types, parent_ty)?;

        // dereference the pointer returned by the GEP instruction
        let load_val = self.builder.build_load(gep_ptr, "gep.deref");
        Ok(Some(load_val))
    }

    fn compile_array(
        &mut self,
        values: &[AstNode],
        ty: &VarType,
        is_const: &bool,
    ) -> CompRet<'ctx> {
        // compile the values that the array is initialized with
        let mut compiled_vals = vec![];
        for vals in values {
            compiled_vals.push(self.compile_node(vals)?.unwrap());
        }

        if *is_const {
            // if all the values in the array are constant values of a literal type
            let arr = match *self.okta_type_to_llvm(ty) {
                BasicTypeEnum::IntType(t) => t.const_array(
                    &compiled_vals
                        .iter()
                        .map(|v| v.into_int_value())
                        .collect::<Vec<_>>(),
                ),
                BasicTypeEnum::FloatType(t) => t.const_array(
                    &compiled_vals
                        .iter()
                        .map(|v| v.into_float_value())
                        .collect::<Vec<_>>(),
                ),
                BasicTypeEnum::ArrayType(t) => t.const_array(
                    &compiled_vals
                        .iter()
                        .map(|v| v.into_array_value())
                        .collect::<Vec<_>>(),
                ),
                BasicTypeEnum::StructType(t) => t.const_array(
                    &compiled_vals
                        .iter()
                        .map(|v| v.into_struct_value())
                        .collect::<Vec<_>>(),
                ),
                _ => unreachable!(),
            };
            Ok(Some(BasicValueEnum::ArrayValue(arr)))
        } else {
            // construct the type of the array
            let arr_ty = self.okta_type_to_llvm(ty).array_type(values.len() as u32);

            // allocate space for the anonymous array
            let ptr = self.create_entry_block_alloca("anon.array", arr_ty);

            // store all the values of the array in the memory the pointer points to
            self.compile_array_in_ptr(&ptr, values)?;

            // deref the pointer to get the array
            let array = self.builder.build_load(ptr, "tmp.deref");

            Ok(Some(array))
        }
    }

    fn compile_str(&mut self, bytes: &[u8]) -> CompRet<'ctx> {
        let ptr = self.compile_str_in_ptr(bytes);
        let deref = self.builder.build_load(ptr, "tmp.deref");
        Ok(Some(deref))
    }

    fn compile_str_in_ptr(&mut self, bytes: &[u8]) -> PointerValue<'ctx> {
        let slice = self.create_entry_block_alloca("str", *self.okta_type_to_llvm(&VarType::Str));

        // compile string's bytes
        let compiled_bytes: Vec<IntValue<'ctx>> = bytes
            .iter()
            .map(|b| self.context.i8_type().const_int(*b as u64, false))
            .collect();
        let byte_array = self
            .context
            .i8_type()
            .const_array(&compiled_bytes)
            .as_basic_value_enum();

        let byte_arr_ty = self.okta_type_to_llvm(&VarType::Array {
            inner: Box::new(VarType::UInt8),
            len: bytes.len(),
        });

        let byte_array_ptr = self.create_entry_block_alloca("str.arr", *byte_arr_ty);
        self.builder.build_store(byte_array_ptr, byte_array);

        // get the pointer to the first element of the string
        let str_ptr = unsafe {
            let zero_index = self.context.i64_type().const_int(0, false);
            self.builder.build_in_bounds_gep(
                byte_array_ptr,
                &[zero_index, zero_index.clone()],
                "str.ptr",
            )
        };

        let str_len = self.context.i64_type().const_int(bytes.len() as u64, false);

        let slice_ptr = self
            .builder
            .build_struct_gep(slice, 0, "slice.ptr")
            .unwrap();
        let slice_len = self
            .builder
            .build_struct_gep(slice, 1, "slice.len")
            .unwrap();

        self.builder.build_store(slice_ptr, str_ptr);
        self.builder.build_store(slice_len, str_len);

        slice
    }

    pub fn compile_array_in_ptr(
        &mut self,
        ptr: &PointerValue<'ctx>,
        values: &[AstNode],
    ) -> CompRet<'ctx> {
        let zero = self.context.i64_type().const_zero();

        for (i, value) in values.iter().enumerate() {
            let compiled = get_value_from_result(&self.compile_node(value)?)?;

            let index = self.context.i64_type().const_int(i as u64, false);
            let val_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(*ptr, &[zero, index], "tmp.gep")
            };

            self.builder.build_store(val_ptr, compiled);
        }
        Ok(None)
    }

    pub fn build_struct_in_ptr(
        &mut self,
        ptr: PointerValue<'ctx>,
        members: &[(String, AstNode)],
    ) -> CompRet<'ctx> {
        for (i, (_, node)) in members.iter().enumerate() {
            let member_ptr = self
                .builder
                .build_struct_gep(ptr, i as u32, "tmp.memb")
                .unwrap();
            let value = self.compile_node(node)?.unwrap();
            self.builder.build_store(member_ptr, value);
        }
        Ok(None)
    }

    pub fn build_enum_var_in_ptr(
        &mut self,
        enum_ptr: PointerValue<'ctx>,
        enum_name: &str,
        variant_name: &str,
        tag: u8,
        fields: &[(usize, VarType, AstNode)],
    ) -> CompRet<'ctx> {
        let variant_ty = self
            .module
            .get_struct_type(format!("{}.{}", enum_name, variant_name).as_str())
            .unwrap();

        // bitcast enum to the specific variant
        let variant_ptr = self
            .builder
            .build_bitcast(
                enum_ptr,
                variant_ty.ptr_type(AddressSpace::Generic),
                variant_name,
            )
            .into_pointer_value();

        let tag_ptr = self
            .builder
            .build_struct_gep(variant_ptr, 0, "enum.tag")
            .unwrap();

        // set the tag value of the variant
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(tag as u64, false));

        // set the rest of variant values
        for (i, _, field_node) in fields {
            let value = self.compile_node(field_node)?.unwrap();
            // the final index of the field is index+1 as the first index (i = 0) is reserved for
            // the variant tag
            let field_ptr = self
                .builder
                .build_struct_gep(variant_ptr, (*i as u32) + 1, "field.ptr")
                .unwrap();
            self.builder.build_store(field_ptr, value);
        }
        Ok(None)
    }

    pub fn compile_memb_acess_ptr(
        &mut self,
        parent: &AstNode,
        members: &[MemberAccess],
        access_types: &[VarType],
        parent_ty: &VarType,
    ) -> Result<PointerValue<'ctx>, String> {
        let mut base_ptr = match parent {
            AstNode::Identifier(id) => {
                match parent_ty {
                    // handle the special case where the parent's type is a reference to a struct
                    VarType::Ref(v) if v.is_struct() => {
                        // just get the pointer to the referenced struct
                        let ptr_ref = self.st.search_variable(id).1;
                        self.builder
                            .build_load(ptr_ref, "deref")
                            .into_pointer_value()
                    }
                    _ => {
                        // get the base pointer of the GEP instruction
                        self.st.search_variable(id).1
                    }
                }
            }
            // otherwise, compile the parent
            other => match self.compile_node(other)?.unwrap() {
                // if the parent is a pointer return this pointer
                BasicValueEnum::PointerValue(ptr) => ptr,
                // else, allocate space in the stack to save the compiled value in, and return the pointer
                // to this new variable
                value => {
                    let p =
                        self.create_entry_block_alloca("tmp", *self.okta_type_to_llvm(parent_ty));
                    self.builder.build_store(p, value);
                    p
                }
            },
        };

        for (res_ty, memb) in access_types.iter().zip(members.iter()) {
            base_ptr = match memb {
                MemberAccess::Range { .. } => self
                    .compile_slice_expr(base_ptr, memb, res_ty)?
                    .unwrap()
                    .into_pointer_value(),
                MemberAccess::MemberId(id) => self
                    .builder
                    .build_struct_gep(base_ptr, *id, "strct.memb")
                    .unwrap(),
                MemberAccess::Index(index) => self
                    .compile_indexation_expr(base_ptr, index)?
                    .unwrap()
                    .into_pointer_value(),
            };
        }

        Ok(base_ptr)
    }

    fn compile_indexation_expr(
        &mut self,
        parent_ptr: PointerValue<'ctx>,
        index: &AstNode,
    ) -> CompRet<'ctx> {
        let idx = get_value_from_result(&self.compile_node(index)?)?.into_int_value();
        let zero_index = self.context.i64_type().const_int(0, false);

        let index_check_fn = self.module.get_function("__okta_indexation_check").unwrap();

        let ptr = match parent_ptr.get_type().get_element_type() {
            AnyTypeEnum::ArrayType(ty) => {
                // check if the index is valid
                let array_len = self
                    .context
                    .i64_type()
                    .const_int(ty.len() as u64, false)
                    .as_basic_value_enum();

                self.builder.build_call(
                    index_check_fn,
                    &[
                        BasicMetadataValueEnum::from(idx),
                        BasicMetadataValueEnum::from(array_len),
                    ],
                    "",
                );

                unsafe {
                    self.builder.build_in_bounds_gep(
                        parent_ptr,
                        &[zero_index.clone(), idx],
                        "indx.expr",
                    )
                }
            }
            // otherwise, parent_ptr is a pointer to a slice
            _ => {
                // get the length of the slice
                let slice_len_gep = self
                    .builder
                    .build_struct_gep(parent_ptr, 1, "slice.len")
                    .unwrap();
                let slice_len = self
                    .builder
                    .build_load(slice_len_gep, "len.deref")
                    .into_int_value();

                // call an intrinsic function to check if the index is inside the bounds of the
                // slice
                self.builder.build_call(
                    index_check_fn,
                    &[
                        BasicMetadataValueEnum::from(idx),
                        BasicMetadataValueEnum::from(slice_len),
                    ],
                    "",
                );

                // get the pointer inside the slice object
                let slice_base_ptr_gep = self
                    .builder
                    .build_struct_gep(parent_ptr, 0, "slice.ptr")
                    .unwrap();
                let slice_base_ptr = self
                    .builder
                    .build_load(slice_base_ptr_gep, "ptr.deref")
                    .into_pointer_value();

                let ptr = unsafe {
                    self.builder
                        .build_in_bounds_gep(slice_base_ptr, &[idx], "indx.expr")
                };

                ptr
            }
        };

        Ok(Some(ptr.as_basic_value_enum()))
    }

    fn compile_slice_expr(
        &mut self,
        parent_ptr: PointerValue<'ctx>,
        range: &MemberAccess,
        slice_result: &VarType,
    ) -> CompRet<'ctx> {
        let (start, end) = match range {
            MemberAccess::Range { start, end } => (start, end),
            _ => unreachable!(),
        };

        // copile range's start and end
        let start = get_value_from_result(&self.compile_node(start)?)?.into_int_value();
        let end = match end {
            Some(v) => Some(get_value_from_result(&self.compile_node(v)?)?.into_int_value()),
            None => None,
        };

        // intrinsic function for slice length calculation
        let slice_length_fn = self.module.get_function("__okta_slice_length").unwrap();

        // check if the parent pointer is a pointer to an array or to a slice
        let (base_ptr, slice_len) = match parent_ptr.get_type().get_element_type() {
            AnyTypeEnum::ArrayType(arr_ty) => {
                // get the length of the array to slice
                let arr_len = self
                    .context
                    .i64_type()
                    .const_int(arr_ty.len().into(), false);

                // convert the pointer to the array to a pointer to the inner type of the array
                // for instance: &[T; n] --> &T
                let bitcast = self.builder.build_bitcast(
                    parent_ptr,
                    arr_ty.get_element_type().ptr_type(AddressSpace::Generic),
                    "bitcast",
                );

                // compute the length of the new new slice, and check slice errors
                let end = match end {
                    Some(v) => v,
                    None => arr_len,
                };
                let args = [
                    BasicMetadataValueEnum::from(start),
                    BasicMetadataValueEnum::from(end),
                    BasicMetadataValueEnum::from(arr_len),
                ];
                let slice_len = self
                    .builder
                    .build_call(slice_length_fn, &args, "len.new")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                (bitcast.into_pointer_value(), slice_len)
            }
            // the parent pointer is another slice (structs cannot be indexed)
            _ => {
                // get the pointer inside the slice
                let gep_len = self
                    .builder
                    .build_struct_gep(parent_ptr, 1, "slice.len")
                    .unwrap();
                let len = self
                    .builder
                    .build_load(gep_len, "len.deref")
                    .into_int_value();

                // compute the length of the new new slice, and check slice errors
                let end = match end {
                    Some(v) => v,
                    None => len,
                };
                let args = [
                    BasicMetadataValueEnum::from(start),
                    BasicMetadataValueEnum::from(end),
                    BasicMetadataValueEnum::from(len),
                ];
                let slice_len = self
                    .builder
                    .build_call(slice_length_fn, &args, "len.new")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                // get the pointer inside the slice
                let gep_ptr = self
                    .builder
                    .build_struct_gep(parent_ptr, 0, "slice.ptr")
                    .unwrap();
                let ptr = self
                    .builder
                    .build_load(gep_ptr, "ptr.deref")
                    .into_pointer_value();

                (ptr, slice_len)
            }
        };

        let slice = self.create_entry_block_alloca("slice", *self.okta_type_to_llvm(slice_result));

        let new_ptr_val = self
            .builder
            .build_struct_gep(slice, 0, "slice.ptr")
            .unwrap();
        self.builder.build_store(new_ptr_val, base_ptr);

        let new_len_val = self
            .builder
            .build_struct_gep(slice, 1, "slice.len")
            .unwrap();
        self.builder.build_store(new_len_val, slice_len);

        Ok(Some(slice.as_basic_value_enum()))
    }
}
