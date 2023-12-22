use std::{fs::File, sync::Arc};

use cranelift::{
    codegen::{
        ir::{immediates::Offset32, Function, StackSlot, UserFuncName},
        isa::TargetIsa,
        Context,
    },
    prelude::*,
};
use cranelift_module::{DataDescription, FuncId, Init, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::{miette, IntoDiagnostic};
use tracing::info;

use crate::parse::{
    types::{
        BinaryOperator, Declaration, Expression, ExternTaskParams, Literal, Program, Statement,
        Type as TblType, UnaryOperator,
    },
    Location,
};

use super::{
    context::{CodeGenContext, FunctionContext, GlobalContext, StructContext, Symbol},
    debug_info::DebugInfoGenerator,
    Config,
};

pub struct CodeGen {
    mod_name: String,
    obj_module: ObjectModule,
    config: Config,
    ctx: CodeGenContext,
    dig: DebugInfoGenerator,
}

impl CodeGen {
    pub fn new(
        mod_name: String,
        target: Arc<dyn TargetIsa>,
        config: Config,
    ) -> miette::Result<Self> {
        let obj_builder = ObjectBuilder::new(
            target.clone(),
            mod_name.bytes().collect::<Vec<_>>(),
            cranelift_module::default_libcall_names(),
        )
        .into_diagnostic()?;
        Ok(Self {
            mod_name,
            obj_module: ObjectModule::new(obj_builder),
            ctx: CodeGenContext::default(),
            config: config.clone(),
            dig: DebugInfoGenerator::new(target, config),
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn insert_func<S: ToString>(
        &mut self,
        name: S,
        id: FuncId,
        params: Vec<(String, TblType)>,
        returns: Option<TblType>,
        is_variadic: bool,
        is_external: bool,
        location: Location,
    ) -> usize {
        self.ctx.insert_function(
            name.to_string(),
            FunctionContext::new(id, params, returns, is_variadic, is_external, location),
        )
    }

    pub fn compile(mut self, program: Program) -> miette::Result<()> {
        for decl in &program.declarations {
            match decl {
                Declaration::ExternTask {
                    name,
                    params: args,
                    returns,
                } => {
                    let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                    if let ExternTaskParams::WellKnown(args) = args {
                        for (_, ty) in args {
                            sig.params
                                .push(AbiParam::new(self.to_cranelift_type(ty).unwrap()))
                        }
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(self.to_cranelift_type(ret).unwrap()));
                    }

                    let id = self
                        .obj_module
                        .declare_function(name, cranelift_module::Linkage::Import, &sig)
                        .into_diagnostic()?;

                    let _fn_idx = self.insert_func(
                        name,
                        id,
                        args.to_arg_vec(),
                        returns.clone(),
                        args.is_variadic(),
                        true,
                        Location::default(),
                    );
                }
                Declaration::Task {
                    location,
                    name,
                    params,
                    returns,
                    body,
                } => {
                    let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                    for (_, ty) in params {
                        sig.params
                            .push(AbiParam::new(self.to_cranelift_type(ty).unwrap()))
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(self.to_cranelift_type(ret).unwrap()));
                    }

                    info!("{name}({:?})", params);

                    let func_id = self
                        .obj_module
                        .declare_function(name, cranelift_module::Linkage::Export, &sig)
                        .into_diagnostic()?;

                    let fn_idx = self.insert_func(
                        name,
                        func_id,
                        params.to_vec(),
                        returns.clone(),
                        false,
                        false,
                        *location,
                    );
                    let fn_name = UserFuncName::user(0, fn_idx as u32);

                    let mut func = Function::with_name_signature(fn_name, sig);
                    let mut func_ctx = FunctionBuilderContext::new();
                    let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

                    let fn_entry = func_builder.create_block();
                    func_builder.append_block_params_for_function_params(fn_entry);
                    func_builder.switch_to_block(fn_entry);
                    func_builder.seal_block(fn_entry);

                    for (idx, (arg, type_)) in params.iter().enumerate() {
                        let ty = self.to_cranelift_type(type_).unwrap();
                        let fn_ctx = self.ctx.functions.get_mut(name).unwrap();
                        let data = StackSlotData::new(StackSlotKind::ExplicitSlot, ty.bytes());
                        let slot = func_builder.create_sized_stack_slot(data);
                        let params = func_builder.block_params(fn_entry);
                        let param = params[idx];
                        func_builder
                            .ins()
                            .stack_store(param, slot, Offset32::new(0));
                        fn_ctx.declare_var(arg, type_.clone(), slot);
                    }

                    for stmt in body {
                        self.compile_stmt(&mut func_builder, stmt)?;
                    }
                    //if body.last().is_none()
                    //    || !matches!(body.last().unwrap(), Statement::Return(_))
                    //{
                    //    self.compile_stmt(&mut func_builder, &Statement::Return(None))?;
                    //}

                    func_builder.finalize();
                    info!("{}", func.display());

                    let mut ctx = Context::for_function(func);

                    ctx.compute_cfg();
                    ctx.compute_domtree();
                    ctx.verify(self.obj_module.isa()).into_diagnostic()?;
                    ctx.dce(self.obj_module.isa()).into_diagnostic()?;
                    ctx.eliminate_unreachable_code(self.obj_module.isa())
                        .into_diagnostic()?;
                    ctx.replace_redundant_loads().into_diagnostic()?;
                    ctx.egraph_pass(self.obj_module.isa()).into_diagnostic()?;

                    self.obj_module
                        .define_function(func_id, &mut ctx)
                        .into_diagnostic()?;
                }
                Declaration::Struct { name, members } => self.ctx.insert_type(
                    name.clone(),
                    StructContext {
                        members: members.clone(),
                    },
                ),
                Declaration::Global { name, type_, value } => {
                    let data_id = self
                        .obj_module
                        .declare_data(name, cranelift_module::Linkage::Export, true, false)
                        .into_diagnostic()?;
                    let mut data_desc = DataDescription::new();
                    data_desc.init = Init::Zeros {
                        size: self.type_size(type_) as usize,
                    };
                    self.obj_module
                        .define_data(data_id, &data_desc)
                        .into_diagnostic()?;
                    self.ctx.globals.insert(
                        name.into(),
                        GlobalContext {
                            id: data_id,
                            type_: type_.clone(),
                            initializer: value.clone(),
                        },
                    );
                }
            }
        }

        let mut res = self.obj_module.finish();

        if self.config.is_debug {
            self.dig.generate(&self.ctx, &mut res)?;
        }

        let obj_name = format!("{}.o", self.mod_name.trim_end_matches(".tbl"));
        let mut file = File::create(obj_name).into_diagnostic()?;
        res.object.write_stream(&mut file).unwrap();

        Ok(())
    }

    fn compile_stmt(
        &mut self,
        func_builder: &mut FunctionBuilder,
        stmt: &Statement,
    ) -> miette::Result<()> {
        let fn_idx = func_builder.func.name.get_user().unwrap().index;
        let ctx = &self.ctx.func_by_idx(fn_idx as usize);
        match stmt {
            Statement::Conditional { test, then, else_ } => {
                let then_block = func_builder.create_block();
                let else_block = func_builder.create_block();
                let after_block = func_builder.create_block();

                let test_val = self.compile_expr(func_builder, Some(TblType::Bool), test)?;
                func_builder
                    .ins()
                    .brif(test_val, then_block, &[], else_block, &[]);

                func_builder.seal_block(then_block);
                func_builder.seal_block(else_block);
                func_builder.switch_to_block(then_block);
                for stmt in then {
                    self.compile_stmt(func_builder, stmt)?;
                }
                if let Some(last) = then.last() {
                    if !matches!(last, Statement::Return(_)) {
                        func_builder.ins().jump(after_block, &[]);
                    }
                }
                func_builder.switch_to_block(else_block);
                for stmt in else_ {
                    self.compile_stmt(func_builder, stmt)?;
                }
                if let Some(last) = else_.last() {
                    if !matches!(last, Statement::Return(_)) {
                        func_builder.ins().jump(after_block, &[]);
                    }
                }

                func_builder.seal_block(after_block);
                func_builder.switch_to_block(after_block);
            }
            Statement::Exit => {
                func_builder.ins().return_(&[]);
            }
            Statement::Expression(expr) => {
                self.compile_expr(func_builder, None, expr)?;
            }
            Statement::Return(v) => {
                let mut return_values = vec![];
                if let Some(expr) = v {
                    return_values.push(self.compile_expr(
                        func_builder,
                        ctx.returns.clone(),
                        expr,
                    )?);
                }
                func_builder.ins().return_(&return_values);
            }
            Statement::Schedule { task, args } => {
                func_builder.ins().return_(&[]);
            }
            Statement::VarDecl { name, type_, value } => {
                let data = StackSlotData::new(StackSlotKind::ExplicitSlot, self.type_size(type_));
                let slot = func_builder.create_sized_stack_slot(data);
                let fn_ctx = self
                    .ctx
                    .func_by_idx_mut(fn_idx as usize)
                    .ok_or(miette!("Could not find function with ID {fn_idx}"))?;
                fn_ctx.declare_var(name, type_.clone(), slot);
                info!(
                    "Declared variable {name} with type {type_:?} ({} bytes)",
                    self.type_size(type_)
                );
                let addr = func_builder.ins().stack_addr(
                    self.obj_module.isa().pointer_type(),
                    slot,
                    Offset32::new(0),
                );
                self.store_expr(func_builder, type_, addr, 0, value)?;
            }
            Statement::Assign { location, value } => {
                let type_ = self.type_of(ctx, location).unwrap();
                let loc = self.compile_lvalue(func_builder, location)?;
                self.store_expr(func_builder, &type_, loc, 0, value)?;
            }
        }
        Ok(())
    }

    fn store_expr(
        &mut self,
        func_builder: &mut FunctionBuilder,
        type_: &TblType,
        addr: Value,
        offset: i32,
        value: &Expression,
    ) -> miette::Result<()> {
        match type_ {
            TblType::Named(name) => match value {
                Expression::Literal(Literal::Struct(members)) => {
                    let struct_ty = self.ctx.types[name].clone();
                    let mut current_offset = 0;
                    for (idx, (_, value)) in members.iter().enumerate() {
                        let member_ty: TblType = struct_ty.member_ty(idx).clone();
                        self.store_expr(
                            func_builder,
                            &member_ty,
                            addr,
                            offset + current_offset,
                            value,
                        )?;
                        current_offset += self.type_size(&member_ty) as i32;
                    }
                }
                expr => {
                    let src = self.compile_lvalue(func_builder, expr)?;
                    let addr = func_builder.ins().iadd_imm(addr, offset as i64);
                    let size = func_builder
                        .ins()
                        .iconst(types::I64, self.type_size(type_) as i64);
                    func_builder.call_memcpy(self.obj_module.target_config(), addr, src, size);
                }
            },
            TblType::Array { item, length } => match value {
                Expression::Literal(Literal::Array(values)) => {
                    let item_size = self.type_size(item);
                    for (idx, value) in values.iter().enumerate() {
                        self.store_expr(
                            func_builder,
                            item,
                            addr,
                            offset + (item_size as i32 * idx as i32),
                            value,
                        )?;
                    }
                }
                expr => {
                    let src = self.compile_lvalue(func_builder, expr)?;
                    let addr = func_builder.ins().iadd_imm(addr, offset as i64);
                    let size = func_builder.ins().iconst(types::I64, *length as i64);
                    func_builder.call_memcpy(self.obj_module.target_config(), addr, src, size);
                }
            },
            _ => {
                let value = self.compile_expr(func_builder, Some(type_.clone()), value)?;
                func_builder
                    .ins()
                    .store(MemFlags::new(), value, addr, Offset32::new(offset));
            }
        }
        Ok(())
    }

    fn compile_expr(
        &mut self,
        builder: &mut FunctionBuilder,
        type_hint: Option<TblType>,
        expr: &Expression,
    ) -> miette::Result<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        let ctx = &self.ctx.func_by_idx(fn_idx as usize);
        match expr {
            Expression::Literal(literal) => match literal {
                Literal::Int(i) => {
                    if let Some(type_hint) = type_hint {
                        let ty = self.to_cranelift_type(&type_hint).unwrap();
                        Ok(builder.ins().iconst(ty, *i))
                    } else {
                        Err(miette!("No type hint for int literal `{i}`"))
                    }
                }
                Literal::String(s) => {
                    let data_id = self
                        .obj_module
                        .declare_anonymous_data(true, false)
                        .into_diagnostic()?;
                    let mut description = DataDescription::new();
                    description.define(
                        s.clone()
                            .into_bytes()
                            .into_iter()
                            .chain(std::iter::once(0))
                            .collect::<Box<[u8]>>(),
                    );
                    self.obj_module
                        .define_data(data_id, &description)
                        .into_diagnostic()?;
                    let data = self.obj_module.declare_data_in_func(data_id, builder.func);
                    Ok(builder
                        .ins()
                        .global_value(self.obj_module.target_config().pointer_type(), data))
                }
                Literal::Bool(v) => match v {
                    true => Ok(builder.ins().iconst(types::I8, 1)),
                    false => Ok(builder.ins().iconst(types::I8, 0)),
                },
                Literal::Struct(_members) => {
                    // Struct literals are weird. They are handled in the assignment of struct variables.
                    unimplemented!()
                }
                Literal::Array(_) => {
                    unimplemented!()
                }
            },
            Expression::Var(v) => {
                match self
                    .ctx
                    .resolve_name(ctx, v)
                    .ok_or(miette!("Cannot resolve name `{v}`"))?
                {
                    Symbol::Variable(var) => Ok(builder.ins().stack_load(
                        self.to_cranelift_type(&var.type_).unwrap(),
                        var.slot,
                        Offset32::new(0),
                    )),
                    Symbol::Function(fun) => {
                        let func_id = self
                            .obj_module
                            .declare_func_in_func(fun.func_id, builder.func);
                        Ok(builder
                            .ins()
                            .func_addr(self.obj_module.isa().pointer_type(), func_id))
                    }
                    Symbol::Global(global) => {
                        let data = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);

                        let addr = builder
                            .ins()
                            .global_value(self.obj_module.isa().pointer_type(), data);

                        Ok(builder.ins().load(
                            self.to_cranelift_type(&global.type_).unwrap(),
                            MemFlags::new(),
                            addr,
                            Offset32::new(0),
                        ))
                    }
                }
            }
            Expression::Call { task, args } => {
                let task_name = match &**task {
                    Expression::Var(t) => t,
                    _ => unimplemented!(),
                };
                match self.ctx.functions.get(task_name) {
                    Some(func_ctx) => {
                        let func = self
                            .obj_module
                            .declare_func_in_func(func_ctx.func_id, builder.func);

                        if func_ctx.is_variadic {
                            let mut arg_vals = vec![];
                            for arg in args {
                                let v = self.compile_expr(builder, None, arg)?;
                                arg_vals.push(v);
                            }
                            let call = builder.ins().call(func, &arg_vals);

                            let sig_ref = builder.func.dfg.call_signature(call).unwrap();
                            let abi_params = arg_vals
                                .into_iter()
                                .map(|a| {
                                    let ty = builder.func.dfg.value_type(a);
                                    AbiParam::new(ty)
                                })
                                .collect::<Vec<AbiParam>>();

                            builder.func.dfg.signatures[sig_ref].params = abi_params;

                            let res = builder.inst_results(call);
                            if res.is_empty() {
                                Ok(builder.ins().iconst(types::I8, 0))
                            } else {
                                Ok(res[0])
                            }
                        } else {
                            let mut arg_vals = vec![];
                            let param_types = func_ctx.params.clone();
                            for (arg, (_, ty)) in args.iter().zip(param_types) {
                                let v = self.compile_expr(builder, Some(ty), arg)?;
                                arg_vals.push(v);
                            }
                            let call = builder.ins().call(func, &arg_vals);
                            let res = builder.inst_results(call);
                            if res.is_empty() {
                                Ok(builder.ins().iconst(types::I8, 0))
                            } else {
                                Ok(res[0])
                            }
                        }
                    }
                    None => {
                        let func_var = &ctx.vars[task_name];
                        let TblType::TaskPtr { params, returns } = &func_var.type_ else {
                            unreachable!()
                        };
                        let callee = builder.ins().stack_load(
                            self.obj_module.isa().pointer_type(),
                            func_var.slot,
                            Offset32::new(0),
                        );
                        let mut arg_vals = vec![];
                        let mut sig = self.obj_module.make_signature();
                        if let Some(returns) = returns.clone() {
                            sig.returns
                                .push(AbiParam::new(self.to_cranelift_type(&returns).unwrap()));
                        }
                        for (arg, ty) in args.iter().zip(params.clone()) {
                            let v = self.compile_expr(builder, Some(ty.clone()), arg)?;
                            arg_vals.push(v);
                            sig.params
                                .push(AbiParam::new(self.to_cranelift_type(&ty).unwrap()));
                        }

                        let sig_ref = builder.import_signature(sig);
                        let call = builder.ins().call_indirect(sig_ref, callee, &arg_vals);
                        let res = builder.inst_results(call);
                        if res.is_empty() {
                            Ok(builder.ins().iconst(types::I8, 0))
                        } else {
                            Ok(res[0])
                        }
                    }
                }
            }
            Expression::BinaryOperation {
                left,
                right,
                operator,
            } => {
                let type_hint_left = self.type_of(ctx, left);
                let type_hint_right = self.type_of(ctx, right);
                let th = match (type_hint_left, type_hint_right) {
                    (None, None) => type_hint,
                    (Some(l), None) => Some(l),
                    (None, Some(r)) => Some(r),
                    (Some(l), Some(r)) => {
                        if l == r {
                            Some(l)
                        } else {
                            return Err(miette!("Conflicting types `{l:?}` and `{r:?}`"));
                        }
                    }
                };
                let left = self.compile_expr(builder, th.clone(), left)?;
                let right = self.compile_expr(builder, th, right)?;
                match operator {
                    BinaryOperator::Equal => Ok(builder.ins().icmp(IntCC::Equal, left, right)),
                    BinaryOperator::Unequal => Ok(builder.ins().icmp(IntCC::NotEqual, left, right)),
                    BinaryOperator::LessThan => {
                        Ok(builder.ins().icmp(IntCC::SignedLessThan, left, right))
                    }
                    BinaryOperator::LessOrEqual => {
                        Ok(builder
                            .ins()
                            .icmp(IntCC::SignedLessThanOrEqual, left, right))
                    }
                    BinaryOperator::GreaterThan => {
                        Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left, right))
                    }
                    BinaryOperator::GreaterOrEqual => {
                        Ok(builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, left, right))
                    }
                    BinaryOperator::And => Ok(builder.ins().band(left, right)),
                    BinaryOperator::Or => Ok(builder.ins().bor(left, right)),
                    BinaryOperator::Add => Ok(builder.ins().iadd(left, right)),
                    BinaryOperator::Subtract => Ok(builder.ins().isub(left, right)),
                    BinaryOperator::Multiply => Ok(builder.ins().imul(left, right)),
                    BinaryOperator::Divide => Ok(builder.ins().sdiv(left, right)),
                }
            }
            Expression::UnaryOperation { value, operator } => {
                let ty = self.type_of(ctx, value).unwrap();
                let val = self.compile_expr(builder, type_hint.clone(), value)?;
                match operator {
                    UnaryOperator::Dereference => Ok(builder.ins().load(
                        self.to_cranelift_type(&ty).unwrap(),
                        MemFlags::new(),
                        val,
                        Offset32::new(0),
                    )),
                    UnaryOperator::Not => Ok(builder.ins().bnot(val)),
                    UnaryOperator::Minus => Ok(builder.ins().ineg(val)),
                    UnaryOperator::Reference => {
                        let Expression::Var(v) = *value.clone() else {
                            return Err(miette!(
                                "Cannot take reference to something other than a variable"
                            ));
                        };
                        let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                        let var = &ctx.vars[&v];
                        Ok(builder.ins().stack_addr(
                            self.obj_module.target_config().pointer_type(),
                            var.slot,
                            Offset32::new(0),
                        ))
                    }
                }
            }
            Expression::StructAccess { value, member } => {
                let ty_name = self.type_of(ctx, value).unwrap().name();
                let ty = &self.ctx.types[&ty_name].members;

                let mut offset = 0;
                let mut idx = 0;
                while &ty[idx].0 != member {
                    offset += self.type_size(&ty[idx].1);
                    idx += 1;
                }

                let member_ty = self
                    .to_cranelift_type(&ty[idx].1)
                    .ok_or(miette!("Cannot convert TBL type to cranelift type"))?;

                let Expression::Var(ref val) = **value else {
                    unreachable!()
                };

                match self
                    .ctx
                    .resolve_name(ctx, val)
                    .ok_or(miette!("Failed to resolve name `{val}`"))?
                {
                    Symbol::Variable(var) => Ok(builder.ins().stack_load(
                        member_ty,
                        var.slot,
                        Offset32::new(offset as i32),
                    )),
                    Symbol::Function(_) => unimplemented!(),
                    Symbol::Global(global) => {
                        let data = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);
                        let addr = builder
                            .ins()
                            .global_value(self.obj_module.isa().pointer_type(), data);
                        Ok(builder.ins().load(
                            member_ty,
                            MemFlags::new(),
                            addr,
                            Offset32::new(offset as i32),
                        ))
                    }
                }
            }
            Expression::Cast { value, to } => {
                let current_ty = self
                    .type_of(ctx, value)
                    .ok_or(miette!("Type to cast from must be known"))?;
                let val = self.compile_expr(builder, Some(current_ty.clone()), value)?;
                match (current_ty, to.clone()) {
                    (
                        TblType::Integer {
                            width: from_width, ..
                        },
                        TblType::Integer {
                            width: to_width, ..
                        },
                    ) => {
                        if to_width > from_width {
                            Ok(builder
                                .ins()
                                .uextend(self.to_cranelift_type(to).unwrap(), val))
                        } else {
                            Ok(builder
                                .ins()
                                .ireduce(self.to_cranelift_type(to).unwrap(), val))
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Expression::Index { value, at } => {
                let ty = self.type_of(ctx, value).unwrap();
                let TblType::Array { item, .. } = ty else {
                    unreachable!()
                };
                let val = self.compile_lvalue(builder, value)?;
                let idx = self.compile_expr(
                    builder,
                    Some(TblType::Integer {
                        signed: false,
                        width: self.obj_module.isa().pointer_bits(),
                    }),
                    at,
                )?;
                let ty_size = self.type_size(&item) as i64;
                let idx = builder.ins().imul_imm(idx, ty_size);
                let addr = builder.ins().iadd(val, idx);
                Ok(builder.ins().load(
                    self.to_cranelift_type(&item).unwrap(),
                    MemFlags::new(),
                    addr,
                    Offset32::new(0),
                ))
            }
        }
    }

    fn compile_lvalue(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expression,
    ) -> miette::Result<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        let ctx = &self.ctx.func_by_idx(fn_idx as usize);
        match expr {
            Expression::Var(v) => {
                match self
                    .ctx
                    .resolve_name(ctx, v)
                    .ok_or(miette!("Failed to resolve name `{v}`"))?
                {
                    Symbol::Variable(var) => Ok(builder.ins().stack_addr(
                        self.obj_module.isa().pointer_type(),
                        var.slot,
                        Offset32::new(0),
                    )),
                    Symbol::Function(_) => todo!(),
                    Symbol::Global(global) => {
                        let value = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);
                        Ok(builder
                            .ins()
                            .global_value(self.obj_module.isa().pointer_type(), value))
                    }
                }
            }
            Expression::StructAccess { value, member } => {
                let ty_name = self.type_of(ctx, value).unwrap().name();
                let ty = &self.ctx.types[&ty_name].members;

                let mut offset = 0;
                let mut idx = 0;
                while &ty[idx].0 != member {
                    offset += self.type_size(&ty[idx].1);
                    idx += 1;
                }

                let Expression::Var(ref val) = **value else {
                    unreachable!()
                };

                match self.ctx.resolve_name(ctx, val).unwrap() {
                    Symbol::Variable(var) => Ok(builder.ins().stack_addr(
                        self.obj_module.isa().pointer_type(),
                        var.slot,
                        Offset32::new(offset as i32),
                    )),
                    Symbol::Function(_) => unimplemented!(),
                    Symbol::Global(global) => {
                        let data = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);
                        let base_addr = builder
                            .ins()
                            .global_value(self.obj_module.isa().pointer_type(), data);
                        Ok(builder.ins().iadd_imm(base_addr, offset as i64))
                    }
                }
            }
            Expression::Index { value, at } => {
                let addr = self.compile_lvalue(builder, value)?;

                match &**at {
                    Expression::Literal(Literal::Int(i)) => Ok(builder.ins().iadd_imm(addr, *i)),
                    _ => {
                        let offset = self.compile_expr(
                            builder,
                            Some(TblType::Integer {
                                signed: false,
                                width: self.obj_module.isa().pointer_bits(),
                            }),
                            at,
                        )?;

                        Ok(builder.ins().iadd(addr, offset))
                    }
                }
            }
            e => Err(miette!("Illegal expression for lvalue: {e:?}")),
        }
    }

    fn type_of(&self, ctx: &FunctionContext, value: &Expression) -> Option<TblType> {
        match value {
            Expression::Literal(l) => match l {
                Literal::Int(_) => None,
                Literal::String(_) => Some(TblType::Pointer(Box::new(TblType::Integer {
                    signed: false,
                    width: 8,
                }))),
                Literal::Bool(_) => Some(TblType::Bool),
                Literal::Struct(_) => None,
                Literal::Array(inner) => match inner.first() {
                    Some(first) => self.type_of(ctx, first).map(|ty| TblType::Array {
                        item: Box::new(ty),
                        length: inner.len() as u64,
                    }),
                    None => None,
                },
            },
            Expression::Var(v) => self.ctx.resolve_name(ctx, v).map(|r| match r {
                Symbol::Variable(var) => var.type_.clone(),
                Symbol::Function(fun) => TblType::TaskPtr {
                    params: fun.params.iter().map(|(_, ty)| ty.clone()).collect(),
                    returns: fun.returns.clone().map(Box::new),
                },
                Symbol::Global(global) => global.type_.clone(),
            }),
            Expression::Call { task, .. } => match &**task {
                Expression::Var(t) => self.ctx.functions[t].returns.clone(),
                _ => None,
            },
            Expression::BinaryOperation { left, right, .. } => {
                if let Some(ty) = self.type_of(ctx, left) {
                    Some(ty)
                } else {
                    self.type_of(ctx, right)
                }
            }
            Expression::UnaryOperation { value, .. } => self.type_of(ctx, value),
            Expression::StructAccess { value, member } => {
                // TODO: check member type
                let struct_name = match self.type_of(ctx, value).unwrap() {
                    TblType::Named(n) => n,
                    _ => unimplemented!(),
                };
                let struct_ty = &self.ctx.types[&struct_name];
                let member_idx = struct_ty.member_idx(member);
                Some(struct_ty.member_ty(member_idx).clone())
            }
            Expression::Cast { to, .. } => Some(to.clone()),
            Expression::Index { value, .. } => {
                let val_ty = self.type_of(ctx, value);
                match val_ty {
                    Some(TblType::Array { item, .. }) => Some(*item),
                    _ => None,
                }
            }
        }
    }

    fn to_cranelift_type(&self, type_: &TblType) -> Option<Type> {
        match type_ {
            TblType::Any => None,
            TblType::Bool => Some(types::I8),
            TblType::Integer { width: 8, .. } => Some(types::I8),
            TblType::Integer { width: 16, .. } => Some(types::I16),
            TblType::Integer { width: 32, .. } => Some(types::I32),
            TblType::Integer { width: 64, .. } => Some(types::I64),
            TblType::Integer { .. } => None,
            TblType::Array { .. } => Some(self.obj_module.target_config().pointer_type()),
            TblType::Pointer(_) => Some(self.obj_module.target_config().pointer_type()),
            TblType::Named(_) => None,
            TblType::TaskPtr { .. } => Some(self.obj_module.target_config().pointer_type()),
        }
    }

    fn type_size(&self, type_: &TblType) -> u32 {
        match type_ {
            TblType::Array { item, length } => self.type_size(item) * (*length as u32),
            TblType::Named(name) => {
                let ty = &self.ctx.types[name];
                ty.members.iter().map(|(_, t)| self.type_size(t)).sum()
            }
            t => self.to_cranelift_type(t).unwrap().bytes(),
        }
    }
}
