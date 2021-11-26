use super::*;

impl<'ctx> CodeGen<'ctx> {
    pub fn compile_var_decl_stmt(
        &mut self,
        id: &str,
        var_type: &VarType,
        value: &AstNode,
    ) -> CompRet<'ctx> {
        // allocate space for the value to hold
        let ptr = self.create_entry_block_alloca(id, *self.okta_type_to_llvm(var_type));

        match value {
            // structs have to be treated different from other values, as struct initialization
            // needs a base pointer to build the struct into (in this case, the pointer that have
            // been just obtained from the alloca above)
            AstNode::Strct { members, .. } => {
                self.build_struct_in_ptr(ptr, members)?;
            }
            AstNode::Array {
                is_const, values, ..
            } if !is_const => {
                self.compile_array_in_ptr(&ptr, values)?;
            }
            AstNode::EnumVariant {
                enum_name,
                variant_name,
                tag,
                fields,
                ..
            } => {
                self.build_enum_var_in_ptr(ptr, enum_name, variant_name, *tag as u8, fields)
                    .unwrap();
            }
            _ => {
                let value = get_value_from_result(&self.compile_node(value)?)?;
                // store the value into the variable
                let _instr = self.builder.build_store(ptr, value);
            }
        }

        // log the variable in the symbol table of the current scope
        self.st.register_variable(id, var_type.clone(), ptr);

        Ok(None)
    }

    pub fn compile_assign_stmt(&mut self, lhs: &AstNode, rhs: &AstNode) -> CompRet<'ctx> {
        // get the pointer of the left hand side value
        let lptr = match lhs {
            AstNode::Identifyer(id) => self.st.search_variable(id).1.clone(),
            AstNode::MemberAccessExpr {
                parent,
                members,
                parent_ty,
                ..
            } => self.compile_memb_acess_ptr(parent, members, parent_ty)?,
            AstNode::UnaryExpr { value, .. } => {
                // get_value_from_result(&self.compile_unary_expr(op, &value, var_ty)?)?.into_pointer_value()
                let ptr = match &**value {
                    AstNode::Identifyer(id) => self.st.search_variable(id).1.clone(),
                    _ => unreachable!(), // this cannot be reached (see `derefVar` rule in the grammar)
                };
                self.builder.build_load(ptr, "tmp.deref").into_pointer_value()
            },
            _ => unreachable!(),
        };

        let rhs = get_value_from_result(&self.compile_node(rhs)?)?;

        // store the value of rhs to lhs
        let _instr = self.builder.build_store(lptr, rhs);
        Ok(None)
    }

    pub fn compile_return_stmt(&mut self, expr: &AstNode) -> CompRet<'ctx> {
        if let Some(ret_ptr) = self.curr_fn_ret_val {
            let ret_val = get_value_from_result(&self.compile_node(expr)?)?;
            self.builder.build_store(ret_ptr, ret_val);
            self.builder
                .build_unconditional_branch(self.curr_fn_ret_bb.unwrap());
        }
        Ok(None)
    }

    pub fn compile_ifelse_stmt(
        &mut self,
        cond: &AstNode,
        then_b: &AstNode,
        elif_b: &[(AstNode, AstNode)],
        else_b: &Option<Box<AstNode>>,
    ) -> CompRet<'ctx> {
        // compile if condition
        let cond = basic_to_int_value(&get_value_from_result(&self.compile_node(cond)?)?)?;
        let then_bb = self.create_basic_block("if.then");

        match else_b {
            // basic `if-else` statement
            Some(else_block) if elif_b.is_empty() => {
                let else_bb = self.create_basic_block("if.else");
                let cont_bb = self.create_basic_block("if.cont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile_node(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                // build else block
                self.builder.position_at_end(else_bb);
                let _else_stmts = self.compile_node(else_block)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            }
            // complex `if-elif-...-else` statements
            Some(else_block) => {
                let mut next_cond = self.create_basic_block("elif.cond");
                let else_bb = self.create_basic_block("if.else");
                let cont_bb = self.create_basic_block("if.cont");

                self.builder
                    .build_conditional_branch(cond, then_bb, next_cond);

                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile_node(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                let mut elif_iter = elif_b.iter().peekable();
                while let Some((cond, elif)) = elif_iter.next() {
                    // compile elif condition basic block
                    self.builder.position_at_end(next_cond);
                    let compiled_cond =
                        basic_to_int_value(&get_value_from_result(&self.compile_node(cond)?)?)?;

                    let elif_bb = self.context.insert_basic_block_after(next_cond, "if.elif");

                    // check if the next block is the `else` block or if there are any
                    // `elif.cond` blocks left
                    next_cond = if elif_iter.peek().is_some() {
                        // check if the next element is `Some`
                        //self.create_basic_block("elif.cond")
                        self.context.insert_basic_block_after(elif_bb, "elif.cond")
                    } else {
                        else_bb
                    };

                    self.builder
                        .build_conditional_branch(compiled_cond, elif_bb, next_cond);

                    // create and compile elif block
                    self.builder.position_at_end(elif_bb);
                    let _elif_stmts = self.compile_node(elif)?;

                    self.builder.build_unconditional_branch(cont_bb);
                }

                // build else block
                self.builder.position_at_end(else_bb);
                let _else_stmts = self.compile_node(else_block)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            }
            // single `if` block
            None => {
                let cont_bb = self.create_basic_block("if.cont");
                self.builder
                    .build_conditional_branch(cond, then_bb, cont_bb);
                // build then block
                self.builder.position_at_end(then_bb);
                let _then_stmts = self.compile_node(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
            }
        }

        Ok(None)
    }

    pub fn compile_if_let_stmt(
        &mut self,
        l_enum: &AstNode,
        r_expr: &AstNode,
        then_b: &AstNode,
        else_b: &Option<Box<AstNode>>,
    ) -> CompRet<'ctx> {
        let (l_enum, l_variant, l_tag, l_fields) = match l_enum {
            AstNode::EnumVariant {
                enum_name,
                variant_name,
                tag,
                fields,
                ..
            } => (enum_name, variant_name, tag, fields),
            _ => unreachable!(),
        };

        // compile the right hand side expression and get the pointer to the expression value
        let r_ptr = match r_expr {
            AstNode::Identifyer(id) => self.st.search_variable(id).1.clone(),
            _ => {
                // compile the expression
                let val = self.compile_node(r_expr)?.unwrap();

                // get the type of the enum
                let ty = VarType::Enum(l_enum.into());

                // save the expression's value in a variable and return it's pointer
                let ptr = self.create_entry_block_alloca("r.enum", *self.okta_type_to_llvm(&ty));
                self.builder.build_store(ptr, val);

                ptr
            }
        };

        // get the pointer to the tag of the right side enum
        let tag_ptr = self.builder.build_struct_gep(r_ptr, 0, "tag.ref").unwrap();

        // dereference the tag pointer
        let r_tag = self.builder.build_load(tag_ptr, "tag.val");

        // create the value of the left enum tag
        let l_tag = self.context.i8_type().const_int(*l_tag as u64, false);

        // compare both tags
        let cond = self.builder.build_int_compare(
            IntPredicate::EQ,
            l_tag,
            r_tag.into_int_value(),
            "iflet.cond",
        );

        // get current basick block
        let prev = self.builder.get_insert_block().unwrap();

        let then_bb = self.create_basic_block("iflet.then");
        let cont_bb = self.create_basic_block("iflet.cont");

        // build the `then` block
        self.builder.position_at_end(then_bb);

        // bitcast right side enum to it's variant
        let variant_ty = self
            .module
            .get_struct_type(format!("{}.{}", l_enum, l_variant).as_str())
            .unwrap();

        let variant_ptr = self
            .builder
            .build_bitcast(r_ptr, variant_ty.ptr_type(AddressSpace::Generic), l_variant)
            .into_pointer_value();

        // load the value of each field into a variable
        for (i, field_ty, field_node) in l_fields {
            let field_id = match field_node {
                AstNode::Identifyer(id) => id,
                _ => unreachable!(),
            };

            // the final index of the field is index+1 as the first index (i = 0) is reserved for
            // the variant tag
            let field_ptr = self
                .builder
                .build_struct_gep(variant_ptr, (*i as u32) + 1, "field.ptr")
                .unwrap();

            self.builder.build_load(field_ptr, field_id);

            self.st
                .register_variable(field_id, field_ty.clone(), field_ptr);
        }

        match else_b {
            Some(else_stmts) => {
                let else_bb = self.context.insert_basic_block_after(then_bb, "iflet.else");

                // let prev = then_bb.get_previous_basic_block().unwrap();
                self.builder.position_at_end(prev);

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // compile `then` statements block
                self.builder.position_at_end(then_bb);

                let _then = self.compile_node(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);

                // compile `else` statements block
                self.builder.position_at_end(else_bb);
                let _else = self.compile_node(else_stmts)?;
                self.builder.build_unconditional_branch(cont_bb);
            }

            None => {
                // let prev = then_bb.get_previous_basic_block().unwrap();
                // println!("Last basic block: {:?}", prev);
                self.builder.position_at_end(prev);

                self.builder
                    .build_conditional_branch(cond, then_bb, cont_bb);

                // compile `then` statements block
                self.builder.position_at_end(then_bb);

                let _then = self.compile_node(then_b)?;
                self.builder.build_unconditional_branch(cont_bb);
            }
        }

        self.builder.position_at_end(cont_bb);

        Ok(None)
    }

    pub fn compile_loop_stmt(&mut self, node: &AstNode) -> CompRet<'ctx> {
        // create loop body basic block
        let loop_bb = self.create_basic_block("loop.body");

        let old_loop_exit = self.loop_exit_bb;
        self.loop_exit_bb = Some(self.create_basic_block("loop.end"));

        // jump from the current bb to the loop's body bb
        self.builder.build_unconditional_branch(loop_bb);

        // build loop's body
        self.builder.position_at_end(loop_bb);
        let _stmts = self.compile_node(node)?;
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(self.loop_exit_bb.unwrap());

        self.loop_exit_bb = old_loop_exit;

        Ok(None)
    }

    pub fn compile_break_stmt(&self) -> CompRet<'ctx> {
        match self.loop_exit_bb {
            Some(bb) => {
                self.builder.build_unconditional_branch(bb);
                Ok(None)
            }
            None => Err("Cannot call `break` outside a loop".to_string()),
        }
    }
}
