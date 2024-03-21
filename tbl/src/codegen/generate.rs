use std::{fs::File, sync::Arc};

use cranelift::{
    codegen::{
        ir::{immediates::Offset32, Function, UserFuncName},
        isa::TargetIsa,
        Context,
    },
    prelude::*,
};
use cranelift_module::{DataDescription, FuncId, Init, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::{miette, IntoDiagnostic};
use tracing::info;

use tbl_parser::{
    types::{
        BinaryOperator, Declaration, Expression, ExternTaskParams, Literal, Program, Statement,
        Type as TblType, UnaryOperator,
    },
    Location,
};

use crate::{codegen::context::StructMember, module::TblModule};

use super::{
    context::{CodeGenContext, FunctionContext, GlobalContext, Symbol},
    debug_info::DebugInfoGenerator,
    Config,
};

pub struct CodeGen {
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

    pub fn compile(mut self, module: &TblModule) -> miette::Result<String> {
        info!("Building module {}", module.name);
        for dep in &module.dependencies {
            for decl in dep.exports() {
                self.compile_decl(&decl)?;
            }
        }

        for decl in &module.program.declarations {
            self.compile_decl(decl)?;
        }

        if !self.config.compile_only {
            self.build_start()?;
        }

        let mut res = self.obj_module.finish();

        if self.config.is_debug {
            self.dig.generate(&self.ctx, &mut res)?;
        }

        let obj_name = format!("{}.o", module.name);
        let mut file = File::create(&obj_name).into_diagnostic()?;
        res.object.write_stream(&mut file).unwrap();

        Ok(obj_name)
    }

    pub fn compile_decl(&mut self, decl: &Declaration) -> miette::Result<()> {
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
                locals,
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

                //info!("{name}({:?}) -> {returns:?}", params);

                let name = match name.as_str() {
                    "main" => "__main",
                    other => other,
                };

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

                let locals_size = params.iter().map(|(_, p)| self.type_size(p)).sum::<u32>()
                    + locals.iter().map(|l| self.type_size(&l.1)).sum::<u32>();
                let data = StackSlotData::new(StackSlotKind::ExplicitSlot, locals_size);
                let locals_slot = func_builder.create_sized_stack_slot(data);
                {
                    let fn_ctx = self.ctx.functions.get_mut(name).unwrap();
                    fn_ctx.init_locals(locals_slot);
                }

                for (idx, (arg, type_)) in params.iter().enumerate() {
                    let params = func_builder.block_params(fn_entry);
                    let param = params[idx];
                    let ty_size = self.type_size(type_);
                    let fn_ctx = self.ctx.functions.get_mut(name).unwrap();
                    fn_ctx.define_local(&mut func_builder, arg, type_.clone(), ty_size, param)?;
                }

                for local in locals {
                    let ty_size = self.type_size(&local.1);
                    let fn_ctx = self.ctx.functions.get_mut(name).unwrap();
                    fn_ctx.declare_local(&local.0, local.1.clone(), ty_size)?;
                }

                for stmt in body {
                    self.compile_stmt(&mut func_builder, stmt)?;
                }
                if body.last().is_none() || !matches!(body.last().unwrap(), Statement::Return(_)) {
                    self.compile_stmt(&mut func_builder, &Statement::Return(None))?;
                }

                func_builder.seal_all_blocks();
                //info!("{}", func_builder.func.display());
                func_builder.finalize();

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

                let fn_ctx = self.ctx.functions.get(name).unwrap().clone();
                self.build_wrapper(name, fn_ctx)?;
            }
            Declaration::Struct { name, members } => self.ctx.create_struct_type(
                name,
                members,
                self.obj_module.isa().pointer_bytes() as usize,
            ),

            Declaration::Global { name, type_, value } => {
                let data_id = self
                    .obj_module
                    .declare_data(name, cranelift_module::Linkage::Export, true, false)
                    .into_diagnostic()?;
                let mut data_desc = DataDescription::new();
                match value {
                    Expression::Literal(Literal::Int(i)) => {
                        let bytes = i.to_le_bytes();
                        data_desc.init = Init::Bytes {
                            contents: Box::new(bytes),
                        }
                    }
                    _ => {
                        data_desc.init = Init::Zeros {
                            size: self.type_size(type_) as usize,
                        };
                    }
                }
                self.obj_module
                    .define_data(data_id, &data_desc)
                    .into_diagnostic()?;
                self.ctx.globals.insert(
                    name.into(),
                    GlobalContext {
                        id: data_id,
                        type_: type_.clone(),
                        initializer: Some(value.clone()),
                    },
                );
            }
            Declaration::ExternGlobal { name, type_ } => {
                let data_id = self
                    .obj_module
                    .declare_data(name, cranelift_module::Linkage::Import, true, false)
                    .into_diagnostic()?;

                self.ctx.globals.insert(
                    name.into(),
                    GlobalContext {
                        id: data_id,
                        type_: type_.clone(),
                        initializer: None,
                    },
                );
            }
            Declaration::Directive { .. } => {
                unreachable!("Directives are handled by the preprocessor")
            }
            Declaration::Use { .. } => {}
        }
        Ok(())
    }

    fn build_wrapper(&mut self, name: &str, fn_ctx: FunctionContext) -> miette::Result<()> {
        let params_name = format!("__{name}_args");
        self.ctx.create_struct_type(
            &params_name,
            &fn_ctx.params,
            self.obj_module.isa().pointer_bytes() as usize,
        );

        let params_ty = self.ctx.types[&params_name].clone();

        let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
        sig.params.push(AbiParam::new(
            self.obj_module.target_config().pointer_type(),
        ));

        let func_name = format!("__{name}_wrapper");

        let func_id = self
            .obj_module
            .declare_function(&func_name, cranelift_module::Linkage::Export, &sig)
            .into_diagnostic()?;

        let fn_idx = self.insert_func(
            &func_name,
            func_id,
            vec![(
                String::from("args"),
                TblType::Pointer(Box::new(TblType::Named(params_name))),
            )],
            None,
            false,
            false,
            Location::default(),
        );
        let fn_name = UserFuncName::user(0, fn_idx as u32);

        let mut func = Function::with_name_signature(fn_name, sig);
        let mut func_ctx = FunctionBuilderContext::new();
        let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let fn_entry = func_builder.create_block();
        func_builder.append_block_params_for_function_params(fn_entry);
        func_builder.switch_to_block(fn_entry);
        func_builder.seal_block(fn_entry);

        let param = func_builder.block_params(fn_entry)[0];

        let wrapped_func = self
            .obj_module
            .declare_func_in_func(fn_ctx.func_id, func_builder.func);
        let mut args = vec![];
        for StructMember {
            offset,
            name: _,
            type_,
        } in &params_ty.members
        {
            let arg = func_builder.ins().load(
                self.to_cranelift_type(type_).unwrap(),
                MemFlags::new(),
                param,
                Offset32::new(*offset as i32),
            );
            args.push(arg);
        }
        func_builder.ins().call(wrapped_func, &args);
        func_builder.ins().return_(&[]);

        //info!("{func_name}");
        //info!("{}", func_builder.func.display());
        func_builder.finalize();

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

        Ok(())
    }

    fn build_start(&mut self) -> miette::Result<()> {
        let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
        sig.returns.push(AbiParam::new(types::I32));
        let func_id = self
            .obj_module
            .declare_function("main", cranelift_module::Linkage::Export, &sig)
            .into_diagnostic()?;
        let mut func = Function::new();
        func.signature = sig;

        let mut func_ctx = FunctionBuilderContext::new();
        let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let fn_entry = func_builder.create_block();
        func_builder.append_block_params_for_function_params(fn_entry);
        func_builder.switch_to_block(fn_entry);
        func_builder.seal_block(fn_entry);

        let globals = self.ctx.globals.clone();
        for global in globals.values() {
            if let Some(init) = &global.initializer {
                let data = self
                    .obj_module
                    .declare_data_in_func(global.id, func_builder.func);
                let addr = func_builder
                    .ins()
                    .global_value(self.obj_module.isa().pointer_type(), data);
                self.store_expr(&mut func_builder, &global.type_, addr, 0, init)?;
            }
        }

        let main_ctx = self.ctx.functions.get("__main").unwrap();
        let main_ref = self
            .obj_module
            .declare_func_in_func(main_ctx.func_id, func_builder.func);

        let sig = self.obj_module.make_signature();
        let sched_run_id = self
            .obj_module
            .declare_function("sched_run", cranelift_module::Linkage::Export, &sig)
            .into_diagnostic()?;

        let sched_run = self
            .obj_module
            .declare_func_in_func(sched_run_id, func_builder.func);

        let main_inst = func_builder.ins().call(main_ref, &[]);
        let main_ret = func_builder
            .inst_results(main_inst)
            .first()
            .copied()
            .unwrap_or_else(|| func_builder.ins().iconst(types::I32, 0));
        func_builder.ins().call(sched_run, &[]);

        func_builder.ins().return_(&[main_ret]);

        //info!("{}", func.display());

        let mut ctx = Context::for_function(func);
        self.obj_module
            .define_function(func_id, &mut ctx)
            .into_diagnostic()?;
        Ok(())
    }

    fn compile_stmt(
        &mut self,
        func_builder: &mut FunctionBuilder,
        stmt: &Statement,
    ) -> miette::Result<()> {
        let fn_idx = func_builder.func.name.get_user().unwrap().index;
        match stmt {
            Statement::Conditional { test, then, else_ } => {
                let then_block = func_builder.create_block();
                let else_block = func_builder.create_block();
                let after_block = func_builder.create_block();

                let test_val = self.compile_expr(func_builder, Some(TblType::Bool), test)?;
                if else_.is_empty() {
                    func_builder
                        .ins()
                        .brif(test_val, then_block, &[], after_block, &[]);
                } else {
                    func_builder
                        .ins()
                        .brif(test_val, then_block, &[], else_block, &[]);
                }

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
                self.compile_expr(
                    func_builder,
                    None,
                    &Expression::Call {
                        task: Box::new(Expression::Var("sched_exit".into())),
                        args: vec![],
                    },
                )?;
            }
            Statement::Expression(expr) => {
                self.compile_expr(func_builder, None, expr)?;
            }
            Statement::Return(v) => {
                let ctx = self.ctx.func_by_idx(fn_idx as usize);
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
                let ty_name = format!("__{task}_args");
                let arg_ty = self.ctx.types.get(&ty_name).unwrap().clone();
                let arg_tbl_ty = TblType::Named(ty_name.clone());
                let ty_size = self.type_size(&arg_tbl_ty);

                let data = StackSlotData::new(StackSlotKind::ExplicitSlot, ty_size);
                let slot = func_builder.create_sized_stack_slot(data);
                let addr = func_builder.ins().stack_addr(
                    self.obj_module.isa().pointer_type(),
                    slot,
                    Offset32::new(0),
                );

                for (
                    arg,
                    StructMember {
                        offset,
                        name: _,
                        type_,
                    },
                ) in args.iter().zip(&arg_ty.members)
                {
                    self.store_expr(func_builder, type_, addr, *offset as i32, arg)?;
                }

                let task_name = format!("__{task}_wrapper");
                let called_task_ctx = self.ctx.functions.get(&task_name).unwrap();
                let called_task = self
                    .obj_module
                    .declare_func_in_func(called_task_ctx.func_id, func_builder.func);
                let called_task_addr = func_builder
                    .ins()
                    .func_addr(self.obj_module.isa().pointer_type(), called_task);

                let args_size = func_builder
                    .ins()
                    .iconst(self.obj_module.isa().pointer_type(), ty_size as i64);

                let sched_enqeue_ctx = self.ctx.functions.get("sched_enqueue").unwrap();
                let sched_enqueue = self
                    .obj_module
                    .declare_func_in_func(sched_enqeue_ctx.func_id, func_builder.func);

                func_builder
                    .ins()
                    .call(sched_enqueue, &[called_task_addr, addr, args_size]);
            }
            Statement::Assign { location, value } => {
                let ctx = self.ctx.func_by_idx(fn_idx as usize);
                let type_ = self.type_of(ctx, location).unwrap();
                let loc = self.compile_lvalue(func_builder, location)?;
                self.store_expr(func_builder, &type_, loc, 0, value)?;
            }
            Statement::Loop { body } => {
                let block = func_builder.create_block();
                func_builder.ins().jump(block, &[]);
                func_builder.switch_to_block(block);
                for stmt in body {
                    self.compile_stmt(func_builder, stmt)?;
                }
                func_builder.ins().jump(block, &[]);
                let next = func_builder.create_block();
                func_builder.seal_block(next);
                func_builder.switch_to_block(next);
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

                    for (member, expr) in members {
                        let member_idx = struct_ty.member_idx(member);
                        let member_offset = struct_ty.members[member_idx].offset;
                        let member_ty = &struct_ty.members[member_idx].type_;
                        self.store_expr(
                            func_builder,
                            member_ty,
                            addr,
                            offset + member_offset as i32,
                            expr,
                        )?;
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
                let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                match self
                    .ctx
                    .resolve_name(ctx, v)
                    .ok_or(miette!("Cannot resolve name `{v}`"))?
                {
                    Symbol::Local(var) => {
                        let offset = ctx.locals().offset_of(&var.name);
                        Ok(builder.ins().stack_load(
                            self.to_cranelift_type(&var.type_).unwrap(),
                            ctx.locals().slot,
                            Offset32::new(offset as i32),
                        ))
                    }
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
            Expression::Call { task, args } => match &**task {
                Expression::Var(task_name) => match self.ctx.functions.get(task_name) {
                    Some(func_ctx) => {
                        let func = self
                            .obj_module
                            .declare_func_in_func(func_ctx.func_id, builder.func);

                        if func_ctx.is_variadic {
                            let mut arg_vals = vec![];
                            for arg in args {
                                let ty_hint = {
                                    let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                                    self.type_of(ctx, arg)
                                };
                                let v = self.compile_expr(builder, ty_hint, arg)?;
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
                        let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                        let func_var = ctx
                            .locals()
                            .find(task_name)
                            .ok_or(miette!("Could not find task `{task_name}`"))?;
                        let TblType::TaskPtr { params, returns } = &func_var.type_ else {
                            unreachable!()
                        };
                        let offset = ctx.locals().offset_of(task_name);
                        let callee = builder.ins().stack_load(
                            self.obj_module.isa().pointer_type(),
                            ctx.locals().slot,
                            Offset32::new(offset as i32),
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
                },
                expr => {
                    let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                    let mut sig = self.obj_module.make_signature();
                    for arg in args {
                        sig.params.push(AbiParam::new(
                            self.to_cranelift_type(&self.type_of(ctx, arg).unwrap())
                                .unwrap(),
                        ));
                    }

                    let callee = self.compile_lvalue(builder, expr)?;
                    let mut arg_vals = vec![];
                    for arg in args {
                        let arg_val = self.compile_expr(builder, None, arg)?;
                        arg_vals.push(arg_val);
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
            },
            Expression::BinaryOperation {
                left,
                right,
                operator,
            } => {
                let ctx = &self.ctx.func_by_idx(fn_idx as usize);
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
                if matches!(operator, UnaryOperator::Reference) {
                    return self.compile_lvalue(builder, value);
                }

                let val = self.compile_expr(builder, type_hint.clone(), value)?;
                match operator {
                    UnaryOperator::Dereference => Ok(builder.ins().load(
                        self.to_cranelift_type(&type_hint.unwrap()).unwrap(),
                        MemFlags::new(),
                        val,
                        Offset32::new(0),
                    )),
                    UnaryOperator::Not => Ok(builder.ins().bnot(val)),
                    UnaryOperator::Minus => Ok(builder.ins().ineg(val)),
                    UnaryOperator::Reference => {
                        unreachable!()
                    }
                }
            }
            Expression::StructAccess { value, member } => {
                let ctx = &self.ctx.func_by_idx(fn_idx as usize);
                let ty_name = self.type_of(ctx, value).unwrap().name();
                let ty = &self.ctx.types[&ty_name];

                let offset = ty
                    .members
                    .iter()
                    .find_map(|m| {
                        if &m.name == member {
                            Some(m.offset)
                        } else {
                            None
                        }
                    })
                    .unwrap() as i32;

                let member_ty = self
                    .to_cranelift_type(ty.member_ty(ty.member_idx(member)))
                    .ok_or(miette!("Cannot convert TBL type to cranelift type"))?;

                let Expression::Var(ref val) = **value else {
                    unreachable!()
                };

                match self
                    .ctx
                    .resolve_name(ctx, val)
                    .ok_or(miette!("Failed to resolve name `{val}`"))?
                {
                    Symbol::Local(var) => {
                        let var_offset = ctx.locals().offset_of(&var.name);
                        Ok(builder.ins().stack_load(
                            member_ty,
                            ctx.locals().slot,
                            Offset32::new(var_offset as i32 + offset),
                        ))
                    }
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
                            Offset32::new(offset),
                        ))
                    }
                }
            }
            Expression::Cast { value, to } => {
                let ctx = &self.ctx.func_by_idx(fn_idx as usize);
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
                    (TblType::Array { .. }, TblType::Pointer(_)) => {
                        self.compile_lvalue(builder, value)
                    }
                    _ => unimplemented!(),
                }
            }
            Expression::Index { value, at } => {
                let ctx = &self.ctx.func_by_idx(fn_idx as usize);
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
            Expression::SizeOf { value } => Ok(builder.ins().iconst(
                self.obj_module.isa().pointer_type(),
                self.type_size(value) as i64,
            )),
        }
    }

    fn compile_lvalue(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expression,
    ) -> miette::Result<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        let ctx = self.ctx.func_by_idx(fn_idx as usize).clone();
        match expr {
            Expression::Var(v) => {
                match self
                    .ctx
                    .resolve_name(&ctx, v)
                    .ok_or(miette!("Failed to resolve name `{v}`"))?
                {
                    Symbol::Local(var) => {
                        let offset = ctx.locals().offset_of(&var.name);
                        Ok(builder.ins().stack_addr(
                            self.obj_module.isa().pointer_type(),
                            ctx.locals().slot,
                            Offset32::new(offset as i32),
                        ))
                    }
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
                let ty = self.type_of(&ctx, value).unwrap();
                let ty_members = &self.ctx.types[&ty.name()].members;

                let offset = ty_members
                    .iter()
                    .find_map(|m| {
                        if &m.name == member {
                            Some(m.offset)
                        } else {
                            None
                        }
                    })
                    .unwrap() as i32;

                let lhs = self.compile_lvalue(builder, value)?;

                let offset_val = builder.ins().iconst(
                    self.obj_module.target_config().pointer_type(),
                    offset as i64,
                );
                Ok(builder.ins().iadd(lhs, offset_val))
            }
            Expression::Index { value, at } => {
                let TblType::Array { item, .. } = self.type_of(&ctx, value).unwrap() else {
                    panic!()
                };
                let addr = self.compile_lvalue(builder, value)?;

                match &**at {
                    Expression::Literal(Literal::Int(i)) => Ok(builder
                        .ins()
                        .iadd_imm(addr, *i * self.type_size(&item) as i64)),
                    _ => {
                        let item_offset = self.compile_expr(
                            builder,
                            Some(TblType::Integer {
                                signed: false,
                                width: self.obj_module.isa().pointer_bits(),
                            }),
                            at,
                        )?;
                        let offset = builder
                            .ins()
                            .imul_imm(item_offset, self.type_size(&item) as i64);

                        Ok(builder.ins().iadd(addr, offset))
                    }
                }
            }
            Expression::UnaryOperation {
                value,
                operator: UnaryOperator::Dereference,
            } => self.compile_expr(builder, self.type_of(&ctx, value), value),
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
                Symbol::Local(var) => var.type_.clone(),
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
            Expression::UnaryOperation { value, operator } => match operator {
                UnaryOperator::Dereference => {
                    if let TblType::Pointer(t) = self.type_of(ctx, value)? {
                        Some(*t)
                    } else {
                        unreachable!("{value:?}")
                    }
                }
                UnaryOperator::Not | UnaryOperator::Minus => self.type_of(ctx, value),
                UnaryOperator::Reference => {
                    Some(TblType::Pointer(Box::new(self.type_of(ctx, value)?)))
                }
            },
            Expression::StructAccess { value, member } => {
                let struct_name = match self.type_of(ctx, value).unwrap() {
                    TblType::Named(n) => n,
                    t => unimplemented!("{t:?}"),
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
            Expression::SizeOf { .. } => Some(TblType::Integer {
                signed: false,
                width: self.obj_module.isa().pointer_bits(),
            }),
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
            TblType::Named(name) => self.ctx.types[name].size as u32,
            t => self.to_cranelift_type(t).unwrap().bytes(),
        }
    }
}
