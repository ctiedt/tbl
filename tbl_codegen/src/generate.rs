use std::{collections::HashMap, fs::File, sync::Arc};

use cranelift::{
    codegen::{
        control::ControlPlane,
        ir::{immediates::Offset32, Function, UserFuncName},
        isa::TargetIsa,
        Context,
    },
    frontend::Switch,
    prelude::*,
};
use cranelift_module::{DataDescription, FuncId, Init, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use tracing::info;

use tbl_parser::{
    module::TblModule,
    types::{
        BinaryOperator, Declaration, DeclarationKind, Expression, ExpressionKind, ExternTaskParams,
        Literal, Statement, StatementKind, Type as TblType, UnaryOperator,
    },
    Span,
};

use crate::{
    context::{StructMember, TypeContext},
    error::{CodegenError, CodegenResult},
};

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
    ) -> CodegenResult<Self> {
        let obj_builder = ObjectBuilder::new(
            target.clone(),
            mod_name.bytes().collect::<Vec<_>>(),
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| CodegenError::new(0..0, e.into()))?;
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
        span: Span,
    ) {
        self.ctx.insert_function(
            name.to_string(),
            FunctionContext::new(
                id,
                params,
                returns,
                is_variadic,
                is_external,
                span,
                self.ctx.global_scope.clone(),
            ),
        )
    }

    fn pointer_type(&self) -> Type {
        self.obj_module.isa().pointer_type()
    }

    pub fn compile(mut self, module: &TblModule) -> CodegenResult<String> {
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
        let mut file = File::create(&obj_name).map_err(|e| CodegenError::new(0..0, e.into()))?;
        res.object.write_stream(&mut file).unwrap();

        Ok(obj_name)
    }

    pub fn compile_decl(&mut self, decl: &Declaration) -> CodegenResult<()> {
        let kind = &decl.kind;
        match kind {
            DeclarationKind::ExternTask {
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
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;

                let _fn_idx = self.insert_func(
                    name,
                    id,
                    args.to_arg_vec(),
                    returns.clone(),
                    args.is_variadic(),
                    true,
                    decl.span.clone(),
                );
            }
            DeclarationKind::Task {
                name,
                params,
                returns,
                body,
            } => {
                let mut params = params.clone();
                params.push(("__tcb".to_string(), TblType::any_ptr()));

                let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                for (_, ty) in &params {
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
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;

                self.insert_func(
                    name,
                    func_id,
                    params.to_vec(),
                    returns.clone(),
                    false,
                    false,
                    decl.span.clone(),
                );
                let fn_name = UserFuncName::user(0, func_id.as_u32());

                let mut func = Function::with_name_signature(fn_name, sig);
                let mut func_ctx = FunctionBuilderContext::new();
                let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

                let fn_entry = func_builder.create_block();
                func_builder.append_block_params_for_function_params(fn_entry);
                func_builder.switch_to_block(fn_entry);
                func_builder.seal_block(fn_entry);

                let locals_size = params.iter().map(|(_, p)| self.type_size(p)).sum::<u32>()
                    + self.task_stack_size(body);
                let data = StackSlotData::new(StackSlotKind::ExplicitSlot, locals_size, 0);
                let locals_slot = func_builder.create_sized_stack_slot(data);
                {
                    let mut fn_ctx = self
                        .ctx
                        .get_function_mut(name)
                        .ok_or(CodegenError::unknown_task(decl.span.clone(), name))?;
                    fn_ctx.init_locals(locals_slot, locals_size);
                }

                for (idx, (arg, type_)) in params.iter().enumerate() {
                    let params = func_builder.block_params(fn_entry);
                    let param = params[idx];
                    let ty_size = self.type_size(type_);
                    let mut fn_ctx = self
                        .ctx
                        .get_function_mut(name)
                        .ok_or(CodegenError::unknown_task(decl.span.clone(), name))?;
                    fn_ctx
                        .define_local(&mut func_builder, arg, type_.clone(), ty_size, param)
                        .map_err(|e| CodegenError::new(decl.span.clone(), e))?;
                }

                let tcb = {
                    let fn_ctx = self.ctx.get_function(name).unwrap();
                    let offset = fn_ctx.locals().offset_of("__tcb");
                    func_builder.ins().stack_load(
                        self.pointer_type(),
                        fn_ctx.locals().slot,
                        Offset32::new(offset as i32),
                    )
                };

                let tcb_is_nonzero = func_builder.ins().icmp_imm(IntCC::NotEqual, tcb, 0);

                let then_block = func_builder.create_block();
                let check_block = func_builder.create_block();
                let else_block = func_builder.create_block();

                func_builder
                    .ins()
                    .brif(tcb_is_nonzero, check_block, &[], else_block, &[]);

                func_builder.switch_to_block(check_block);

                let should_copy_locals =
                    func_builder
                        .ins()
                        .load(types::I8, MemFlags::new(), tcb, Offset32::new(0));
                func_builder
                    .ins()
                    .brif(should_copy_locals, then_block, &[], else_block, &[]);

                func_builder.switch_to_block(then_block);

                let saved_local_member = func_builder.ins().iadd_imm(tcb, 16);
                let saved_locals_addr = func_builder.ins().load(
                    self.pointer_type(),
                    MemFlags::new(),
                    saved_local_member,
                    Offset32::new(0),
                );
                let locals_addr = func_builder.ins().stack_addr(
                    self.pointer_type(),
                    locals_slot,
                    Offset32::new(0),
                );
                let locals_size_val = func_builder
                    .ins()
                    .iconst(self.pointer_type(), locals_size as i64);
                func_builder.call_memcpy(
                    self.obj_module.target_config(),
                    locals_addr,
                    saved_locals_addr,
                    locals_size_val,
                );
                func_builder.ins().jump(else_block, &[]);

                func_builder.switch_to_block(else_block);

                for stmt in body {
                    self.compile_stmt(&mut func_builder, stmt)?;
                }
                if body.last().is_none()
                    || !matches!(body.last().unwrap().kind, StatementKind::Return(_))
                {
                    self.compile_stmt(
                        &mut func_builder,
                        &StatementKind::Return(None).with_span(0..0),
                    )?;
                }

                func_builder.seal_all_blocks();
                info!("{}", name);
                info!("{}", func_builder.func.display());
                func_builder.finalize();

                let mut ctx = Context::for_function(func);

                ctx.compute_cfg();
                ctx.compute_domtree();
                ctx.verify(self.obj_module.isa())
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                // ctx.dce(self.obj_module.isa())
                //     .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                ctx.eliminate_unreachable_code(self.obj_module.isa())
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                ctx.replace_redundant_loads()
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                // ctx.egraph_pass(self.obj_module.isa())
                //     .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                self.obj_module
                    .define_function(func_id, &mut ctx)
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                let fn_ctx = self
                    .ctx
                    .get_function(name)
                    .ok_or(CodegenError::unknown_task(decl.span.clone(), name))?
                    .clone();
                self.build_wrapper(name, fn_ctx, decl.span.clone())?;
            }
            DeclarationKind::Struct { name, members } => self.ctx.create_struct_type(
                name,
                members,
                self.obj_module.isa().pointer_bytes() as usize,
            ),

            DeclarationKind::Enum { name, variants } => self.ctx.create_enum_type(
                name,
                variants,
                self.obj_module.isa().pointer_bytes() as usize,
            ),

            DeclarationKind::Global { name, type_, value } => {
                let data_id = self
                    .obj_module
                    .declare_data(name, cranelift_module::Linkage::Export, true, false)
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                let mut data_desc = DataDescription::new();
                match &value.kind {
                    ExpressionKind::Literal(Literal::Int(i)) => {
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
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;
                self.ctx.global_scope.insert_global(
                    name,
                    GlobalContext {
                        id: data_id,
                        type_: type_.clone(),
                        initializer: Some(value.clone()),
                    },
                );
            }
            DeclarationKind::ExternGlobal { name, type_ } => {
                let data_id = self
                    .obj_module
                    .declare_data(name, cranelift_module::Linkage::Import, true, false)
                    .map_err(|e| CodegenError::new(decl.span.clone(), e.into()))?;

                self.ctx.global_scope.insert_global(
                    name,
                    GlobalContext {
                        id: data_id,
                        type_: type_.clone(),
                        initializer: None,
                    },
                );
            }
            DeclarationKind::Directive { .. } => {
                unreachable!("Directives are handled by the preprocessor")
            }
            DeclarationKind::Use { .. } => {}
        }
        Ok(())
    }

    fn build_wrapper(
        &mut self,
        name: &str,
        fn_ctx: FunctionContext,
        span: Span,
    ) -> CodegenResult<()> {
        let params_name = format!("__{name}_args");
        let len = fn_ctx.params.len() - 1;
        let params = fn_ctx.params.into_iter().take(len).collect::<Vec<_>>();
        self.ctx.create_struct_type(
            &params_name,
            &params,
            self.obj_module.isa().pointer_bytes() as usize,
        );

        let params_ty = self.ctx.types[&params_name].unwrap_struct().clone();

        let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
        sig.params.push(AbiParam::new(
            self.obj_module.target_config().pointer_type(),
        ));
        sig.params.push(AbiParam::new(
            self.obj_module.target_config().pointer_type(),
        ));

        let func_name = format!("__{name}_wrapper");

        let func_id = self
            .obj_module
            .declare_function(&func_name, cranelift_module::Linkage::Export, &sig)
            .map_err(|e| CodegenError::new(span.clone(), e.into()))?;

        self.insert_func(
            &func_name,
            func_id,
            vec![
                (
                    String::from("args"),
                    TblType::Pointer(Box::new(TblType::Named(params_name))),
                ),
                (String::from("tcb"), TblType::any_ptr()),
            ],
            None,
            false,
            false,
            span.clone(),
        );
        let fn_name = UserFuncName::user(0, func_id.as_u32());

        let mut func = Function::with_name_signature(fn_name, sig);
        let mut func_ctx = FunctionBuilderContext::new();
        let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let fn_entry = func_builder.create_block();
        func_builder.append_block_params_for_function_params(fn_entry);
        func_builder.switch_to_block(fn_entry);
        func_builder.seal_block(fn_entry);

        let param = func_builder.block_params(fn_entry)[0];
        let tcb = func_builder.block_params(fn_entry)[1];

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
        args.push(tcb);
        func_builder.ins().call(wrapped_func, &args);
        func_builder.ins().return_(&[]);

        //info!("{func_name}");
        //info!("{}", func_builder.func.display());
        func_builder.finalize();

        let mut ctx = Context::for_function(func);

        ctx.compute_cfg();
        ctx.compute_domtree();
        let span_clone = span.clone();
        ctx.verify(self.obj_module.isa())
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;
        let span_clone = span.clone();
        ctx.eliminate_unreachable_code(self.obj_module.isa())
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;
        let span_clone = span.clone();
        ctx.eliminate_unreachable_code(self.obj_module.isa())
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;
        let span_clone = span.clone();
        ctx.replace_redundant_loads()
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;
        let span_clone = span.clone();
        ctx.egraph_pass(self.obj_module.isa(), &mut ControlPlane::default())
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;
        let span_clone = span.clone();
        self.obj_module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CodegenError::new(span_clone, e.into()))?;

        Ok(())
    }

    fn build_start(&mut self) -> CodegenResult<()> {
        let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(self.pointer_type()));
        sig.returns.push(AbiParam::new(types::I32));
        let func_id = self
            .obj_module
            .declare_function("main", cranelift_module::Linkage::Export, &sig)
            .map_err(|e| CodegenError::new(0..0, e.into()))?;
        let mut func = Function::new();
        func.signature = sig;

        let mut func_ctx = FunctionBuilderContext::new();
        let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let fn_entry = func_builder.create_block();
        func_builder.append_block_params_for_function_params(fn_entry);
        func_builder.switch_to_block(fn_entry);
        func_builder.seal_block(fn_entry);

        let globals = self
            .ctx
            .global_scope
            .symbols()
            .into_iter()
            .filter_map(|(_, g)| {
                if let Symbol::Global(global) = g {
                    Some(global)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        for global in globals {
            let global = global.borrow();
            if let Some(init) = &global.initializer {
                let data = self
                    .obj_module
                    .declare_data_in_func(global.id, func_builder.func);
                let addr = func_builder.ins().global_value(self.pointer_type(), data);
                self.store_expr(&mut func_builder, &global.type_, addr, 0, init)?;
            }
        }

        let main_ctx = self.ctx.get_function("__main").unwrap();
        let main_ref = self
            .obj_module
            .declare_func_in_func(main_ctx.func_id, func_builder.func);

        let sig = self.obj_module.make_signature();
        let sched_run_id = self
            .obj_module
            .declare_function("sched_run", cranelift_module::Linkage::Export, &sig)
            .map_err(|e| CodegenError::new(0..0, e.into()))?;

        let sched_run = self
            .obj_module
            .declare_func_in_func(sched_run_id, func_builder.func);

        let mut params = vec![];
        if main_ctx.params.len() > 1 {
            params.extend(func_builder.block_params(fn_entry));
        }
        let zero = func_builder.ins().iconst(types::I64, 0);
        params.push(zero);

        let main_inst = func_builder.ins().call(main_ref, &params);
        let main_ret = func_builder
            .inst_results(main_inst)
            .first()
            .copied()
            .unwrap_or_else(|| func_builder.ins().iconst(types::I32, 0));
        func_builder.ins().call(sched_run, &[]);

        func_builder.ins().return_(&[main_ret]);

        info!("main");
        info!("{}", func.display());

        let mut ctx = Context::for_function(func);
        self.obj_module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CodegenError::new(0..0, e.into()))?;
        Ok(())
    }

    fn compile_stmt(
        &mut self,
        func_builder: &mut FunctionBuilder,
        stmt: &Statement,
    ) -> CodegenResult<()> {
        let fn_idx = func_builder.func.name.get_user().unwrap().index;
        match &stmt.kind {
            StatementKind::Conditional { test, then, else_ } => {
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
                    if !matches!(last.kind, StatementKind::Return(_) | StatementKind::Break) {
                        func_builder.ins().jump(after_block, &[]);
                    }
                }
                func_builder.switch_to_block(else_block);
                for stmt in else_ {
                    self.compile_stmt(func_builder, stmt)?;
                }
                if let Some(last) = else_.last() {
                    if !matches!(last.kind, StatementKind::Return(_) | StatementKind::Break) {
                        func_builder.ins().jump(after_block, &[]);
                    }
                }

                func_builder.seal_block(after_block);
                func_builder.switch_to_block(after_block);
            }
            StatementKind::Exit => {
                self.compile_expr(
                    func_builder,
                    None,
                    &ExpressionKind::Call {
                        task: Box::new(ExpressionKind::Var("sched_exit".into()).with_span(0..0)),
                        args: vec![],
                    }
                    .with_span(0..0),
                )?;
            }
            StatementKind::Expression(expr) => {
                self.compile_expr(func_builder, None, expr)?;
            }
            StatementKind::Return(v) => {
                let tcb = {
                    let fn_ctx = self.ctx.func_by_idx(fn_idx).unwrap();
                    let offset = fn_ctx.locals().offset_of("__tcb");
                    func_builder.ins().stack_load(
                        self.pointer_type(),
                        fn_ctx.locals().slot,
                        Offset32::new(offset as i32),
                    )
                };

                let should_copy_locals = func_builder.ins().icmp_imm(IntCC::NotEqual, tcb, 0);

                let then_block = func_builder.create_block();
                let else_block = func_builder.create_block();

                func_builder
                    .ins()
                    .brif(should_copy_locals, then_block, &[], else_block, &[]);

                func_builder.switch_to_block(then_block);

                let one = func_builder.ins().iconst(types::I8, 1);
                func_builder
                    .ins()
                    .store(MemFlags::new(), one, tcb, Offset32::new(0));
                let saved_local_member = func_builder.ins().iadd_imm(tcb, 16);
                let saved_locals_addr = func_builder.ins().load(
                    self.pointer_type(),
                    MemFlags::new(),
                    saved_local_member,
                    Offset32::new(0),
                );
                let locals_addr = {
                    let fn_ctx = self.ctx.func_by_idx(fn_idx).unwrap();
                    func_builder.ins().stack_addr(
                        self.pointer_type(),
                        fn_ctx.locals().slot,
                        Offset32::new(0),
                    )
                };
                let locals_size_val = {
                    let fn_ctx = self.ctx.func_by_idx(fn_idx).unwrap();
                    func_builder
                        .ins()
                        .iconst(self.pointer_type(), fn_ctx.locals().size as i64)
                };
                func_builder.call_memcpy(
                    self.obj_module.target_config(),
                    saved_locals_addr,
                    locals_addr,
                    locals_size_val,
                );

                func_builder.ins().jump(else_block, &[]);

                func_builder.switch_to_block(else_block);
                let returns = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                    ctx.returns.clone()
                };
                let mut return_values = vec![];
                if let Some(expr) = v {
                    return_values.push(self.compile_expr(func_builder, returns, expr)?);
                }
                func_builder.ins().return_(&return_values);
            }
            StatementKind::Assign { location, value } => {
                let type_ = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                    self.type_of(&ctx, location).unwrap()
                };
                let loc = self.compile_lvalue(func_builder, location)?;
                self.store_expr(func_builder, &type_, loc, 0, value)?;
            }
            StatementKind::Definition { name, type_, value } => {
                let val = self.compile_expr(func_builder, Some(type_.clone()), value)?;
                let size = self.type_size(type_);
                let mut ctx = self
                    .ctx
                    .func_by_idx_mut(fn_idx)
                    .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                ctx.define_local(func_builder, name, type_.clone(), size, val)
                    .unwrap();
            }
            StatementKind::Declaration { name, type_ } => {
                let size = self.type_size(type_);
                let mut ctx = self
                    .ctx
                    .func_by_idx_mut(fn_idx)
                    .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                ctx.declare_local(name, type_.clone(), size).unwrap();
            }
            StatementKind::Loop { body } => {
                let block = func_builder.create_block();
                func_builder.ins().jump(block, &[]);
                func_builder.switch_to_block(block);
                let next = func_builder.create_block();
                {
                    let mut ctx = self
                        .ctx
                        .func_by_idx_mut(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                    ctx.loop_labels.push(next);
                }
                for stmt in body {
                    self.compile_stmt(func_builder, stmt)?;
                }
                {
                    let mut ctx = self
                        .ctx
                        .func_by_idx_mut(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                    ctx.loop_labels
                        .pop()
                        .ok_or(CodegenError::empty_loop_stack(stmt.span.clone()))?;
                }
                func_builder.ins().jump(block, &[]);
                func_builder.seal_block(next);
                func_builder.switch_to_block(next);
            }
            StatementKind::Block { statements } => {
                for stmt in statements {
                    self.compile_stmt(func_builder, stmt)?;
                }
            }
            StatementKind::Match { value, branches } => {
                let mut switch = Switch::new();
                let ty = {
                    let ctx = &self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                    self.type_of(ctx, value).unwrap()
                };
                let TblType::Named(ty_name) = ty else {
                    unreachable!()
                };
                let enum_ty = self.ctx.get_enum_type(&ty_name).unwrap().clone();
                let then = func_builder.create_block();
                let otherwise = func_builder.create_block();
                let mut block_branches = HashMap::new();
                for (pat, branch) in branches {
                    match pat {
                        tbl_parser::types::MatchPattern::Ident(id) => {
                            let block = func_builder.create_block();
                            let variant = enum_ty.variant_tag(&id);
                            switch.set_entry(variant as u128, block);
                            block_branches.insert(variant, branch);
                        }
                        tbl_parser::types::MatchPattern::Expr(_) => unimplemented!(),
                        tbl_parser::types::MatchPattern::Any => {}
                    }
                }
                let discriminant = self.compile_expr(func_builder, None, value)?;
                let entries = switch.entries().clone();
                switch.emit(func_builder, discriminant, otherwise);
                for (pat, branch) in branches {
                    match pat {
                        tbl_parser::types::MatchPattern::Ident(id) => {
                            let variant = enum_ty.variant_tag(id) as u128;
                            let block = *entries.get(&variant).unwrap();
                            func_builder.seal_block(block);
                            func_builder.switch_to_block(block);
                            self.compile_stmt(func_builder, branch)?;
                            func_builder.ins().jump(then, &[]);
                        }
                        tbl_parser::types::MatchPattern::Expr(_) => unimplemented!(),
                        tbl_parser::types::MatchPattern::Any => {
                            func_builder.seal_block(otherwise);
                            func_builder.switch_to_block(otherwise);
                            self.compile_stmt(func_builder, branch)?;
                            func_builder.ins().jump(then, &[]);
                        }
                    }
                }
                func_builder.switch_to_block(then);
            }
            StatementKind::Break => {
                let ctx = self
                    .ctx
                    .func_by_idx(fn_idx)
                    .ok_or(CodegenError::unknown_task_id(stmt.span.clone(), fn_idx))?;
                let block = ctx
                    .loop_labels
                    .last()
                    .ok_or(CodegenError::empty_loop_stack(stmt.span.clone()))?;
                func_builder.ins().jump(*block, &[]);
            }
            StatementKind::Attach { handle, task } => {
                let handle_val = self.compile_expr(func_builder, Some(TblType::Handle), handle)?;
                let task_val = self.compile_expr(func_builder, None, task)?;

                let sched_attach_ctx = self.ctx.get_function("sched_attach").unwrap();
                let sched_attach = self
                    .obj_module
                    .declare_func_in_func(sched_attach_ctx.func_id, func_builder.func);

                func_builder
                    .ins()
                    .call(sched_attach, &[handle_val, task_val]);
            }
            StatementKind::Once { stmt } => {
                let tcb = {
                    let fn_ctx = self.ctx.func_by_idx(fn_idx).unwrap();
                    let offset = fn_ctx.locals().offset_of("__tcb");
                    func_builder.ins().stack_load(
                        self.pointer_type(),
                        fn_ctx.locals().slot,
                        Offset32::new(offset as i32),
                    )
                };

                let once_idx = self.ctx.func_by_idx_mut(fn_idx).unwrap().add_once();

                let then_block = func_builder.create_block();
                let check_block = func_builder.create_block();
                let else_block = func_builder.create_block();

                let tcb_is_null = func_builder.ins().icmp_imm(IntCC::Equal, tcb, 0);
                func_builder
                    .ins()
                    .brif(tcb_is_null, else_block, &[], check_block, &[]);

                func_builder.switch_to_block(check_block);
                let enter_once =
                    func_builder
                        .ins()
                        .load(types::I64, MemFlags::new(), tcb, Offset32::new(8));
                let idx = 1i64 << once_idx;
                let enter_once_set = func_builder.ins().band_imm(enter_once, idx);
                let cond = func_builder.ins().icmp_imm(IntCC::Equal, enter_once_set, 0);
                func_builder
                    .ins()
                    .brif(cond, then_block, &[], else_block, &[]);

                func_builder.switch_to_block(then_block);

                let enter_once =
                    func_builder
                        .ins()
                        .load(types::I64, MemFlags::new(), tcb, Offset32::new(8));
                let replace_enter_once = func_builder.ins().bor_imm(enter_once, idx);
                func_builder.ins().store(
                    MemFlags::new(),
                    replace_enter_once,
                    tcb,
                    Offset32::new(8),
                );
                self.compile_stmt(func_builder, stmt)?;

                func_builder.ins().jump(else_block, &[]);
                func_builder.switch_to_block(else_block);
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
    ) -> CodegenResult<()> {
        match type_ {
            TblType::Named(name) => match &value.kind {
                ExpressionKind::Literal(Literal::Struct(members)) => {
                    let struct_ty = self.ctx.types[name].unwrap_struct().clone();

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
                ExpressionKind::Literal(Literal::Enum { variant, members }) => {
                    let enum_ty = self.ctx.types[name].unwrap_enum().clone();
                    let tag = func_builder
                        .ins()
                        .iconst(types::I8, enum_ty.variant_tag(&variant) as i64);
                    func_builder.ins().store(MemFlags::new(), tag, addr, offset);

                    let variant_ty = enum_ty.get_variant(variant).unwrap();
                    for (member, expr) in members {
                        let member_idx = variant_ty.member_idx(member);
                        let member_offset = variant_ty.members[member_idx].offset;
                        let member_ty = &variant_ty.members[member_idx].type_;
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
                    let src = self.compile_lvalue(
                        func_builder,
                        &expr.clone().with_span(value.span.clone()),
                    )?;
                    let addr = func_builder.ins().iadd_imm(addr, offset as i64);
                    let size = func_builder
                        .ins()
                        .iconst(types::I64, self.type_size(type_) as i64);
                    func_builder.call_memcpy(self.obj_module.target_config(), addr, src, size);
                }
            },
            TblType::Array { item, length } => match &value.kind {
                ExpressionKind::Literal(Literal::Array(values)) => {
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
                    let src = self.compile_lvalue(
                        func_builder,
                        &expr.clone().with_span(value.span.clone()),
                    )?;
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
    ) -> CodegenResult<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        match &expr.kind {
            ExpressionKind::Literal(literal) => match literal {
                Literal::Int(i) => {
                    if let Some(type_hint) = type_hint {
                        let ty = self.to_cranelift_type(&type_hint).unwrap();
                        Ok(builder.ins().iconst(ty, *i))
                    } else {
                        Err(CodegenError::missing_type_hint(expr.span.clone()))
                    }
                }
                Literal::Time(t) => Ok(builder.ins().iconst(types::I64, *t as i64)),
                Literal::String(s) => {
                    let data_id = self
                        .obj_module
                        .declare_anonymous_data(true, false)
                        .map_err(|e| CodegenError::new(expr.span.clone(), e.into()))?;
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
                        .map_err(|e| CodegenError::new(expr.span.clone(), e.into()))?;
                    let data = self.obj_module.declare_data_in_func(data_id, builder.func);
                    Ok(builder
                        .ins()
                        .global_value(self.obj_module.target_config().pointer_type(), data))
                }
                Literal::Bool(v) => match v {
                    true => Ok(builder.ins().iconst(types::I8, 1)),
                    false => Ok(builder.ins().iconst(types::I8, 0)),
                },
                Literal::Struct(_) | Literal::Array(_) | Literal::Enum { .. } => {
                    // Composite literals cannot be created "freestanding". They can only be used for assignments.
                    Err(CodegenError::freestanding_composite_literal(
                        expr.span.clone(),
                    ))
                }
            },
            ExpressionKind::Var(v) => {
                let ctx = &self
                    .ctx
                    .func_by_idx(fn_idx)
                    .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                match ctx
                    .scope
                    .get(v)
                    .ok_or(CodegenError::unknown_symbol(expr.span.clone(), v))?
                {
                    Symbol::Local(var) => {
                        let var = var.borrow();
                        let offset = ctx.locals().offset_of(&var.name);
                        Ok(builder.ins().stack_load(
                            self.to_cranelift_type(&var.type_).unwrap(),
                            ctx.locals().slot,
                            Offset32::new(offset as i32),
                        ))
                    }
                    Symbol::Function(fun) => {
                        let fun = fun.borrow();
                        let func_id = self
                            .obj_module
                            .declare_func_in_func(fun.func_id, builder.func);
                        Ok(builder.ins().func_addr(self.pointer_type(), func_id))
                    }
                    Symbol::Global(global) => {
                        let global = global.borrow();
                        let data = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);

                        let addr = builder.ins().global_value(self.pointer_type(), data);

                        Ok(builder.ins().load(
                            self.to_cranelift_type(&global.type_).unwrap(),
                            MemFlags::new(),
                            addr,
                            Offset32::new(0),
                        ))
                    }
                }
            }
            ExpressionKind::Call { task, args } => match &task.kind {
                ExpressionKind::Var(task_name) => {
                    let sym = {
                        let ctx = self.ctx.func_by_idx(fn_idx).unwrap();
                        ctx.scope
                            .get(task_name)
                            .ok_or(CodegenError::unknown_task(expr.span.clone(), task_name))?
                    };
                    match sym {
                        Symbol::Function(func_ctx) => {
                            let func_ctx = func_ctx.borrow();
                            let func = self
                                .obj_module
                                .declare_func_in_func(func_ctx.func_id, builder.func);

                            if func_ctx.is_variadic {
                                let mut arg_vals = vec![];
                                for arg in args {
                                    let ty_hint = {
                                        let ctx = &self.ctx.func_by_idx(fn_idx).ok_or(
                                            CodegenError::unknown_task_id(
                                                expr.span.clone(),
                                                fn_idx,
                                            ),
                                        )?;
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
                        func_var => {
                            let TblType::TaskPtr { params, returns } = &func_var.type_() else {
                                unreachable!()
                            };
                            let callee = {
                                let ctx = self.ctx.func_by_idx(fn_idx).ok_or(
                                    CodegenError::unknown_task_id(expr.span.clone(), fn_idx),
                                )?;
                                let offset = ctx.locals().offset_of(task_name);
                                builder.ins().stack_load(
                                    self.pointer_type(),
                                    ctx.locals().slot,
                                    Offset32::new(offset as i32),
                                )
                            };
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
                expr_kind => {
                    let mut sig = self.obj_module.make_signature();
                    {
                        let ctx = &self
                            .ctx
                            .func_by_idx(fn_idx)
                            .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                        for arg in args {
                            sig.params.push(AbiParam::new(
                                self.to_cranelift_type(&self.type_of(ctx, arg).unwrap())
                                    .unwrap(),
                            ));
                        }
                    }

                    let callee = self
                        .compile_lvalue(builder, &expr_kind.clone().with_span(task.span.clone()))?;
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
            ExpressionKind::BinaryOperation {
                left,
                right,
                operator,
            } => {
                let (type_hint_left, type_hint_right) = {
                    let ctx = &self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    (self.type_of(ctx, left), self.type_of(ctx, right))
                };

                // Pointer arithmetic
                match (type_hint_left.clone(), type_hint_right.clone()) {
                    (Some(TblType::Pointer(p)), Some(TblType::Integer { signed, width })) => {
                        let left = self.compile_expr(builder, Some(TblType::Pointer(p)), left)?;
                        let right = self.compile_expr(
                            builder,
                            Some(TblType::Integer { signed, width }),
                            right,
                        )?;
                        let scale = builder.ins().iconst(
                            self.pointer_type(),
                            self.obj_module.isa().pointer_bytes() as i64,
                        );
                        let addend = builder.ins().imul(right, scale);
                        return Ok(builder.ins().iadd(left, addend));
                    }
                    (Some(TblType::Integer { signed, width }), Some(TblType::Pointer(p))) => {
                        let left = self.compile_expr(
                            builder,
                            Some(TblType::Integer { signed, width }),
                            left,
                        )?;
                        let right = self.compile_expr(builder, Some(TblType::Pointer(p)), right)?;
                        let scale = builder.ins().iconst(
                            self.pointer_type(),
                            self.obj_module.isa().pointer_bytes() as i64,
                        );
                        let addend = builder.ins().imul(left, scale);
                        return Ok(builder.ins().iadd(addend, right));
                    }
                    (Some(TblType::Pointer(p)), None) => {
                        let left =
                            self.compile_expr(builder, Some(TblType::Pointer(p.clone())), left)?;
                        let right = self.compile_expr(builder, Some(TblType::Pointer(p)), right)?;
                        let scale = builder.ins().iconst(
                            self.pointer_type(),
                            self.obj_module.isa().pointer_bytes() as i64,
                        );
                        let addend = builder.ins().imul(right, scale);
                        return Ok(builder.ins().iadd(left, addend));
                    }
                    (None, Some(TblType::Pointer(p))) => {
                        let left =
                            self.compile_expr(builder, Some(TblType::Pointer(p.clone())), left)?;
                        let right = self.compile_expr(builder, Some(TblType::Pointer(p)), right)?;
                        let scale = builder.ins().iconst(
                            self.pointer_type(),
                            self.obj_module.isa().pointer_bytes() as i64,
                        );
                        let addend = builder.ins().imul(left, scale);
                        return Ok(builder.ins().iadd(addend, right));
                    }
                    _ => {}
                }

                let th = match (type_hint_left, type_hint_right) {
                    (None, None) => type_hint,
                    (Some(l), None) => Some(l),
                    (None, Some(r)) => Some(r),
                    (Some(l), Some(r)) => {
                        if l == r {
                            Some(l)
                        } else {
                            return Err(CodegenError::conflicting_types(expr.span.clone(), l, r));
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
            ExpressionKind::UnaryOperation { value, operator } => {
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
            ExpressionKind::StructAccess { value, member } => {
                let ctx = &self
                    .ctx
                    .func_by_idx(fn_idx)
                    .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                let ty_name = self.type_of(ctx, value).unwrap().name();
                let ty = &self.ctx.types[&ty_name].unwrap_struct();

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
                    .ok_or(CodegenError::cannot_convert_to_cranelift_type(
                        expr.span.clone(),
                        ty.member_ty(ty.member_idx(member)).clone(),
                    ))?;

                let ExpressionKind::Var(ref val) = &value.kind else {
                    unreachable!()
                };

                match ctx
                    .scope
                    .get(val)
                    .ok_or(CodegenError::unknown_symbol(expr.span.clone(), val))?
                {
                    Symbol::Local(var) => {
                        let var = var.borrow();
                        let var_offset = ctx.locals().offset_of(&var.name);
                        Ok(builder.ins().stack_load(
                            member_ty,
                            ctx.locals().slot,
                            Offset32::new(var_offset as i32 + offset),
                        ))
                    }
                    Symbol::Function(_) => unimplemented!(),
                    Symbol::Global(global) => {
                        let global = global.borrow();
                        let data = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);
                        let addr = builder.ins().global_value(self.pointer_type(), data);
                        Ok(builder.ins().load(
                            member_ty,
                            MemFlags::new(),
                            addr,
                            Offset32::new(offset),
                        ))
                    }
                }
            }
            ExpressionKind::Cast { value, to } => {
                let current_ty = {
                    let ctx = &self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    self.type_of(ctx, value)
                        .ok_or(CodegenError::unknown_cast_from(expr.span.clone()))?
                };
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
                    (
                        TblType::Pointer(_),
                        TblType::Integer {
                            signed: false,
                            width,
                        },
                    ) => self.compile_expr(
                        builder,
                        Some(TblType::Integer {
                            signed: false,
                            width,
                        }),
                        value,
                    ),
                    (TblType::Integer { signed: false, .. }, TblType::Pointer(p)) => {
                        self.compile_expr(builder, Some(TblType::Pointer(p)), value)
                    }
                    (TblType::Pointer(p1), TblType::Pointer(p2)) => {
                        self.compile_expr(builder, Some(*p2), value)
                    }
                    (t1, t2) => Err(CodegenError::illegal_cast(expr.span.clone(), t1, t2)),
                }
            }
            ExpressionKind::Index { value, at } => {
                let ty = {
                    let ctx = &self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    self.type_of(ctx, value).unwrap()
                };
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
            ExpressionKind::SizeOf { value } => Ok(builder
                .ins()
                .iconst(self.pointer_type(), self.type_size(value) as i64)),
            ExpressionKind::Schedule { task, args, period } => {
                let ty_name = format!("__{task}_args");
                let arg_ty = self
                    .ctx
                    .types
                    .get(&ty_name)
                    .unwrap()
                    .unwrap_struct()
                    .clone();
                let arg_tbl_ty = TblType::Named(ty_name.clone());
                let ty_size = self.type_size(&arg_tbl_ty);

                let period_val = self.compile_expr(builder, Some(TblType::Duration), period)?;

                let data = StackSlotData::new(StackSlotKind::ExplicitSlot, ty_size, 0);
                let slot = builder.create_sized_stack_slot(data);
                let addr = builder
                    .ins()
                    .stack_addr(self.pointer_type(), slot, Offset32::new(0));

                for (
                    arg,
                    StructMember {
                        offset,
                        name: _,
                        type_,
                    },
                ) in args.iter().zip(&arg_ty.members)
                {
                    self.store_expr(builder, type_, addr, *offset as i32, arg)?;
                }

                let task_name = format!("__{task}_wrapper");
                let called_task_ctx = self
                    .ctx
                    .get_function(&task_name)
                    .ok_or(CodegenError::unknown_task(expr.span.clone(), &task_name))?;
                let called_task = self
                    .obj_module
                    .declare_func_in_func(called_task_ctx.func_id, builder.func);
                let called_task_addr = builder.ins().func_addr(self.pointer_type(), called_task);

                let args_size = builder.ins().iconst(self.pointer_type(), ty_size as i64);

                let sched_enqueue_ctx = self.ctx.get_function("sched_enqueue").unwrap();
                let sched_enqueue = self
                    .obj_module
                    .declare_func_in_func(sched_enqueue_ctx.func_id, builder.func);

                let call_inst = builder.ins().call(
                    sched_enqueue,
                    &[called_task_addr, addr, args_size, period_val],
                );

                let res = builder.inst_results(call_inst)[0];

                Ok(res)
            }
        }
    }

    fn compile_lvalue(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expression,
    ) -> CodegenResult<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        match &expr.kind {
            ExpressionKind::Var(v) => {
                let sym = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    ctx.scope
                        .get(v)
                        .ok_or(CodegenError::unknown_symbol(expr.span.clone(), v))?
                };
                match sym {
                    Symbol::Local(var) => {
                        let var = var.borrow();
                        let (offset, slot) = {
                            let ctx = self
                                .ctx
                                .func_by_idx(fn_idx)
                                .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                            (ctx.locals().offset_of(&var.name), ctx.locals().slot)
                        };
                        Ok(builder.ins().stack_addr(
                            self.pointer_type(),
                            slot,
                            Offset32::new(offset as i32),
                        ))
                    }
                    Symbol::Function(_) => todo!(),
                    Symbol::Global(global) => {
                        let global = global.borrow();
                        let value = self
                            .obj_module
                            .declare_data_in_func(global.id, builder.func);
                        Ok(builder.ins().global_value(self.pointer_type(), value))
                    }
                }
            }
            ExpressionKind::StructAccess { value, member } => {
                let ty = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    self.type_of(&ctx, value).unwrap()
                };
                let ty_members = &self.ctx.types[&ty.name()].unwrap_struct().members;

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
            ExpressionKind::Index { value, at } => {
                let item = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;

                    let TblType::Array { item, .. } = self.type_of(&ctx, value).unwrap() else {
                        panic!()
                    };
                    item
                };
                let addr = self.compile_lvalue(builder, value)?;

                match &at.kind {
                    ExpressionKind::Literal(Literal::Int(i)) => Ok(builder
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
            ExpressionKind::UnaryOperation {
                value,
                operator: UnaryOperator::Dereference,
            } => {
                let ty = {
                    let ctx = self
                        .ctx
                        .func_by_idx(fn_idx)
                        .ok_or(CodegenError::unknown_task_id(expr.span.clone(), fn_idx))?;
                    self.type_of(&ctx, value)
                };
                self.compile_expr(builder, ty, value)
            }
            e => Err(CodegenError::illegal_lvalue(expr.span.clone(), e.clone())),
        }
    }

    fn type_of(&self, ctx: &FunctionContext, value: &Expression) -> Option<TblType> {
        match &value.kind {
            ExpressionKind::Literal(l) => match l {
                Literal::Int(_) => None,
                Literal::Time(_) => Some(TblType::Duration),
                Literal::String(_) => Some(TblType::Pointer(Box::new(TblType::Integer {
                    signed: false,
                    width: 8,
                }))),
                Literal::Bool(_) => Some(TblType::Bool),
                Literal::Struct(_) => None,
                Literal::Enum { .. } => None,
                Literal::Array(inner) => match inner.first() {
                    Some(first) => self.type_of(ctx, first).map(|ty| TblType::Array {
                        item: Box::new(ty),
                        length: inner.len() as u64,
                    }),
                    None => None,
                },
            },
            ExpressionKind::Var(v) => ctx.scope.get(v).map(|r| match r {
                Symbol::Local(var) => {
                    let var = var.borrow();
                    var.type_.clone()
                }
                Symbol::Function(fun) => {
                    let fun = fun.borrow();
                    TblType::TaskPtr {
                        params: fun.params.iter().map(|(_, ty)| ty.clone()).collect(),
                        returns: fun.returns.clone().map(Box::new),
                    }
                }
                Symbol::Global(global) => {
                    let global = global.borrow();
                    global.type_.clone()
                }
            }),
            ExpressionKind::Call { task, .. } => match &task.kind {
                ExpressionKind::Var(t) => self.ctx.get_function(t).unwrap().returns.clone(),
                _ => None,
            },
            ExpressionKind::BinaryOperation { left, right, .. } => {
                if let Some(ty) = self.type_of(ctx, left) {
                    Some(ty)
                } else {
                    self.type_of(ctx, right)
                }
            }
            ExpressionKind::UnaryOperation { value, operator } => match operator {
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
            ExpressionKind::StructAccess { value, member } => {
                let struct_name = match self.type_of(ctx, value).unwrap() {
                    TblType::Named(n) => n,
                    t => unimplemented!("{t:?}"),
                };
                let struct_ty = &self.ctx.types[&struct_name].unwrap_struct();
                let member_idx = struct_ty.member_idx(member);
                Some(struct_ty.member_ty(member_idx).clone())
            }
            ExpressionKind::Cast { to, .. } => Some(to.clone()),
            ExpressionKind::Index { value, .. } => {
                let val_ty = self.type_of(ctx, value);
                match val_ty {
                    Some(TblType::Array { item, .. }) => Some(*item),
                    _ => None,
                }
            }
            ExpressionKind::SizeOf { .. } => Some(TblType::Integer {
                signed: false,
                width: self.obj_module.isa().pointer_bits(),
            }),
            ExpressionKind::Schedule { .. } => Some(TblType::Handle),
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
            TblType::Named(name) => match &self.ctx.types[name] {
                TypeContext::Struct(_) => None,
                TypeContext::Enum(_) => Some(types::I8),
            },
            TblType::TaskPtr { .. } => Some(self.obj_module.target_config().pointer_type()),
            TblType::Handle => Some(self.obj_module.target_config().pointer_type()),
            TblType::Duration => Some(types::I64),
        }
    }

    fn type_size(&self, type_: &TblType) -> u32 {
        self.ctx
            .type_size(type_, self.obj_module.isa().pointer_bytes() as usize) as u32
    }

    fn task_stack_size(&self, func: &Vec<Statement>) -> u32 {
        let mut size = 0;
        for stmt in func {
            match &stmt.kind {
                StatementKind::Definition { type_, .. } => {
                    size += self.type_size(type_);
                }
                StatementKind::Declaration { type_, .. } => {
                    size += self.type_size(type_);
                }
                _ => {}
            }
        }
        size
    }
}
