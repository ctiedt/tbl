use std::{collections::HashMap, fs::File, sync::Arc};

use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        isa::TargetIsa,
        Context,
    },
    prelude::*,
};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::{miette, IntoDiagnostic};
use tracing::info;

use crate::ast::{Declaration, Expression, Program, Statement};

pub struct CodeGen {
    mod_name: String,
    obj_module: ObjectModule,
    functions: Vec<FunctionContext>,
}

#[derive(Clone, Debug)]
struct FunctionContext {
    name: String,
    param_types: Vec<String>,
    returns: Option<String>,
    var_idx: usize,
    vars: HashMap<String, VarInfo>,
    func_id: FuncId,
}

impl FunctionContext {
    fn new(name: String, id: FuncId, param_types: Vec<String>, returns: Option<String>) -> Self {
        Self {
            name,
            param_types,
            returns,
            var_idx: 0,
            vars: HashMap::new(),
            func_id: id,
        }
    }

    fn declare_var(&mut self, name: &str, type_: String) -> VarInfo {
        self.vars.insert(
            name.to_string(),
            VarInfo {
                var: Variable::new(self.var_idx),
                type_,
            },
        );
        self.var_idx += 1;
        self.vars[name].clone()
    }
}

#[derive(Debug, Clone)]
struct VarInfo {
    var: Variable,
    type_: String,
}

impl CodeGen {
    pub fn new(mod_name: String, target: Arc<dyn TargetIsa>) -> miette::Result<Self> {
        let obj_builder = ObjectBuilder::new(
            target,
            mod_name.clone().bytes().collect::<Vec<_>>(),
            cranelift_module::default_libcall_names(),
        )
        .into_diagnostic()?;
        Ok(Self {
            mod_name,
            obj_module: ObjectModule::new(obj_builder),
            functions: Vec::new(),
        })
    }

    fn insert_func<S: ToString>(
        &mut self,
        name: S,
        id: FuncId,
        param_types: Vec<String>,
        returns: Option<String>,
    ) -> usize {
        self.functions.push(FunctionContext::new(
            name.to_string(),
            id,
            param_types,
            returns,
        ));
        self.functions.len() - 1
    }

    pub fn compile(mut self, program: Program) -> miette::Result<()> {
        for decl in &program.declarations {
            match decl {
                Declaration::ExternTask {
                    name,
                    args,
                    returns,
                } => {
                    let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                    for (_, ty) in args {
                        sig.params
                            .push(AbiParam::new(self.to_cranelift_type(ty).unwrap()))
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
                        args.iter().map(|(_, ty)| ty.clone()).collect(),
                        returns.clone(),
                    );
                }
                Declaration::Task {
                    name,
                    args,
                    returns,
                    body,
                } => {
                    let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                    for (_, ty) in args {
                        sig.params
                            .push(AbiParam::new(self.to_cranelift_type(ty).unwrap()))
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(self.to_cranelift_type(ret).unwrap()));
                    }

                    let func_id = self
                        .obj_module
                        .declare_function(name, cranelift_module::Linkage::Export, &sig)
                        .into_diagnostic()?;

                    let fn_idx = self.insert_func(
                        name,
                        func_id,
                        args.iter().map(|(_, ty)| ty.clone()).collect(),
                        returns.clone(),
                    );
                    let fn_name = UserFuncName::user(0, fn_idx as u32);

                    let mut func = Function::with_name_signature(fn_name, sig);
                    let mut func_ctx = FunctionBuilderContext::new();
                    let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

                    let fn_entry = func_builder.create_block();
                    func_builder.append_block_params_for_function_params(fn_entry);
                    func_builder.switch_to_block(fn_entry);
                    func_builder.seal_block(fn_entry);

                    for (idx, (arg, ty)) in args.iter().enumerate() {
                        let fn_ctx = &mut self.functions[fn_idx];
                        let var = fn_ctx.declare_var(arg, ty.clone());
                        func_builder.declare_var(var.var, self.to_cranelift_type(ty).unwrap());
                        let params = func_builder.block_params(fn_entry);
                        func_builder.def_var(var.var, params[idx]);
                    }

                    for stmt in body {
                        self.compile_stmt(&mut func_builder, stmt)?;
                    }

                    func_builder.finalize();
                    info!("{}", func.display());

                    let mut ctx = Context::for_function(func);
                    self.obj_module
                        .define_function(func_id, &mut ctx)
                        .into_diagnostic()?;
                }
            }
        }

        let res = self.obj_module.finish();
        let obj_name = format!("{}.o", self.mod_name.trim_end_matches(".tbl"));
        let mut file = File::create(&obj_name).into_diagnostic()?;
        res.object.write_stream(&mut file).unwrap();

        let elf_name = self.mod_name.trim_end_matches(".tbl");
        std::process::Command::new("ld.lld")
            .args([
                "-o",
                elf_name,
                "-dynamic-linker",
                "/lib/ld-linux-x86-64.so.2",
                "/usr/lib/crt1.o",
                "/usr/lib/crti.o",
                "-L/usr/lib",
                "-lc",
                &obj_name,
                "/usr/lib/crtn.o",
            ])
            .spawn()
            .into_diagnostic()?
            .wait()
            .into_diagnostic()?;

        Ok(())
    }

    fn compile_stmt(
        &mut self,
        func_builder: &mut FunctionBuilder,
        stmt: &Statement,
    ) -> miette::Result<()> {
        let fn_idx = func_builder.func.name.get_user().unwrap().index;
        let ctx = &self.functions[fn_idx as usize];
        match stmt {
            crate::ast::Statement::Conditional { test, then, else_ } => {
                let then_block = func_builder.create_block();
                let else_block = func_builder.create_block();
                let after_block = func_builder.create_block();

                let test_val = self.compile_expr(func_builder, Some("bool".to_string()), test)?;
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
            crate::ast::Statement::Exit => {
                func_builder.ins().return_(&[]);
            }
            crate::ast::Statement::Expression(expr) => {
                self.compile_expr(func_builder, None, expr)?;
            }
            crate::ast::Statement::Return(v) => {
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
            crate::ast::Statement::Schedule { task, args } => {
                func_builder.ins().return_(&[]);
            }
            crate::ast::Statement::VarDecl { name, type_, value } => {
                let var = {
                    let fn_ctx = &mut self.functions[fn_idx as usize];
                    fn_ctx.declare_var(name, type_.clone())
                };
                func_builder.declare_var(var.var, self.to_cranelift_type(type_).unwrap());
                let value = self.compile_expr(func_builder, Some(type_.clone()), value)?;
                func_builder.def_var(var.var, value);
            }
            crate::ast::Statement::VarAssign { name, value } => {
                let var = {
                    let fn_ctx = &self.functions[fn_idx as usize];
                    fn_ctx.vars[name].clone()
                };
                let val = self.compile_expr(func_builder, Some(var.type_), value)?;
                func_builder.def_var(var.var, val)
            }
        }
        Ok(())
    }

    fn compile_expr(
        &mut self,
        builder: &mut FunctionBuilder,
        type_hint: Option<String>,
        expr: &Expression,
    ) -> miette::Result<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        let ctx = &self.functions[fn_idx as usize];
        match expr {
            Expression::Literal(literal) => match literal {
                crate::ast::Literal::Int(i) => {
                    if let Some(type_hint) = type_hint {
                        let ty = self.to_cranelift_type(&type_hint).unwrap();
                        Ok(builder.ins().iconst(ty, *i))
                    } else {
                        Err(miette!("No type hint for int literal `{i}`"))
                    }
                }
                crate::ast::Literal::String(s) => {
                    let data_id = self
                        .obj_module
                        .declare_anonymous_data(true, false)
                        .into_diagnostic()?;
                    let mut description = DataDescription::new();
                    description.define(s.clone().into_bytes().into_iter().collect::<Box<[u8]>>());
                    self.obj_module
                        .define_data(data_id, &description)
                        .into_diagnostic()?;
                    let data = self.obj_module.declare_data_in_func(data_id, builder.func);
                    Ok(builder
                        .ins()
                        .global_value(self.obj_module.target_config().pointer_type(), data))
                }
                crate::ast::Literal::Bool(v) => match v {
                    true => Ok(builder.ins().iconst(types::I8, 1)),
                    false => Ok(builder.ins().iconst(types::I8, 0)),
                },
            },
            Expression::Var(v) => {
                let var = &ctx.vars[v];
                Ok(builder.use_var(var.var))
            }
            Expression::Call { task, args } => {
                let func_ctx = self.functions.iter().find(|f| &f.name == task).unwrap();
                let func = self
                    .obj_module
                    .declare_func_in_func(func_ctx.func_id, builder.func);
                let mut arg_vals = vec![];
                let param_types = func_ctx.param_types.clone();
                for (arg, ty) in args.iter().zip(param_types) {
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
                            return Err(miette!("Conflicting types `{l}` and `{r}`"));
                        }
                    }
                };
                let left = self.compile_expr(builder, th.clone(), left)?;
                let right = self.compile_expr(builder, th, right)?;
                match operator {
                    crate::ast::BinaryOperator::Equal => {
                        Ok(builder.ins().icmp(IntCC::Equal, left, right))
                    }
                    crate::ast::BinaryOperator::Unequal => {
                        Ok(builder.ins().icmp(IntCC::NotEqual, left, right))
                    }
                    crate::ast::BinaryOperator::LessThan => {
                        Ok(builder.ins().icmp(IntCC::SignedLessThan, left, right))
                    }
                    crate::ast::BinaryOperator::LessOrEqual => {
                        Ok(builder
                            .ins()
                            .icmp(IntCC::SignedLessThanOrEqual, left, right))
                    }
                    crate::ast::BinaryOperator::GreaterThan => {
                        Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left, right))
                    }
                    crate::ast::BinaryOperator::GreaterOrEqual => {
                        Ok(builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, left, right))
                    }
                    crate::ast::BinaryOperator::And => Ok(builder.ins().band(left, right)),
                    crate::ast::BinaryOperator::Or => Ok(builder.ins().bor(left, right)),
                    crate::ast::BinaryOperator::Add => Ok(builder.ins().iadd(left, right)),
                    crate::ast::BinaryOperator::Subtract => Ok(builder.ins().isub(left, right)),
                    crate::ast::BinaryOperator::Multiply => Ok(builder.ins().imul(left, right)),
                    crate::ast::BinaryOperator::Divide => Ok(builder.ins().sdiv(left, right)),
                }
            }
            Expression::UnaryOperation { value, operator } => todo!(),
        }
    }

    fn type_of(&self, ctx: &FunctionContext, value: &Expression) -> Option<String> {
        match value {
            Expression::Literal(_) => None,
            Expression::Var(v) => ctx.vars.get(v).map(|var| var.type_.clone()),
            Expression::Call { task, .. } => self
                .functions
                .iter()
                .find(|f| &f.name == task)
                .map(|t| t.returns.clone())?,
            Expression::BinaryOperation { left, right, .. } => {
                if let Some(ty) = self.type_of(ctx, left) {
                    Some(ty)
                } else {
                    self.type_of(ctx, right)
                }
            }
            Expression::UnaryOperation { value, .. } => self.type_of(ctx, value),
        }
    }

    fn to_cranelift_type(&self, type_: &str) -> Option<Type> {
        match type_ {
            "u8" | "i8" | "bool" => Some(types::I8),
            "u16" | "i16" => Some(types::I16),
            "u32" | "i32" => Some(types::I32),
            "u64" | "i64" => Some(types::I64),
            t if t.starts_with('&') => Some(self.obj_module.target_config().pointer_type()),
            _ => None,
        }
    }
}
