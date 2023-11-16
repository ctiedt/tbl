use std::{collections::HashMap, fs::File, sync::Arc};

use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        isa::TargetIsa,
        Context,
    },
    prelude::*,
};
use cranelift_module::{FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::IntoDiagnostic;
use tracing::info;

use crate::ast::{Declaration, Expression, Program};

pub struct CodeGen {
    mod_name: String,
    obj_module: ObjectModule,
    functions: Vec<FunctionContext>,
}

#[derive(Clone, Debug)]
struct FunctionContext {
    name: String,
    var_idx: usize,
    vars: HashMap<String, Variable>,
    func_id: FuncId,
}

impl FunctionContext {
    fn new(name: String, id: FuncId) -> Self {
        Self {
            name,
            var_idx: 0,
            vars: HashMap::new(),
            func_id: id,
        }
    }

    fn new_var(&mut self, name: &str) -> Variable {
        self.vars
            .insert(name.to_string(), Variable::new(self.var_idx));
        self.var_idx += 1;
        self.vars[name]
    }
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

    fn insert_func<S: ToString>(&mut self, name: S, id: FuncId) -> usize {
        self.functions
            .push(FunctionContext::new(name.to_string(), id));
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
                            .push(AbiParam::new(to_cranelift_type(ty).unwrap()))
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(to_cranelift_type(ret).unwrap()));
                    }

                    let id = self
                        .obj_module
                        .declare_function(name, cranelift_module::Linkage::Import, &sig)
                        .into_diagnostic()?;

                    let _fn_idx = self.insert_func(name, id);
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
                            .push(AbiParam::new(to_cranelift_type(ty).unwrap()))
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(to_cranelift_type(ret).unwrap()));
                    }

                    let func_id = self
                        .obj_module
                        .declare_function(name, cranelift_module::Linkage::Export, &sig)
                        .into_diagnostic()?;

                    let fn_idx = self.insert_func(name, func_id);
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
                        let var = fn_ctx.new_var(arg);
                        func_builder.declare_var(var, to_cranelift_type(ty).unwrap());
                        let params = func_builder.block_params(fn_entry);
                        func_builder.def_var(var, params[idx]);
                    }

                    for stmt in body {
                        match stmt {
                            crate::ast::Statement::Exit => {
                                func_builder.ins().return_(&[]);
                            }
                            crate::ast::Statement::Expression(expr) => {
                                self.compile_expr(&mut func_builder, expr)?;
                            }
                            crate::ast::Statement::Return(v) => {
                                let mut returns = vec![];
                                if let Some(expr) = v {
                                    returns.push(self.compile_expr(&mut func_builder, expr)?);
                                }
                                func_builder.ins().return_(&returns);
                            }
                            crate::ast::Statement::Schedule { task, args } => {
                                func_builder.ins().return_(&[]);
                            }
                            crate::ast::Statement::VarDecl { name, type_, value } => {
                                let var = {
                                    let fn_ctx = &mut self.functions[fn_idx];
                                    fn_ctx.new_var(name)
                                };
                                func_builder.declare_var(var, to_cranelift_type(type_).unwrap());
                                let value = self.compile_expr(&mut func_builder, value)?;
                                func_builder.def_var(var, value);
                            }
                        }
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

    fn compile_expr(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expression,
    ) -> miette::Result<Value> {
        let fn_idx = builder.func.name.get_user().unwrap().index;
        let ctx = &self.functions[fn_idx as usize];
        match expr {
            Expression::Literal(literal) => match literal {
                crate::ast::Literal::Int(i) => Ok(builder.ins().iconst(types::I32, *i)),
                crate::ast::Literal::String(_) => todo!(),
                crate::ast::Literal::Bool(_) => todo!(),
            },
            Expression::Var(v) => {
                let var = ctx.vars[v];
                Ok(builder.use_var(var))
            }
            Expression::Call { task, args } => {
                let func_ctx = self.functions.iter().find(|f| &f.name == task).unwrap();
                let func = self
                    .obj_module
                    .declare_func_in_func(func_ctx.func_id, builder.func);
                let mut arg_vals = vec![];
                for arg in args {
                    let v = self.compile_expr(builder, arg)?;
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
                let left = self.compile_expr(builder, left)?;
                let right = self.compile_expr(builder, right)?;
                match operator {
                    crate::ast::BinaryOperator::Equal => todo!(),
                    crate::ast::BinaryOperator::Unequal => todo!(),
                    crate::ast::BinaryOperator::LessThan => todo!(),
                    crate::ast::BinaryOperator::LessOrEqual => todo!(),
                    crate::ast::BinaryOperator::GreaterThan => todo!(),
                    crate::ast::BinaryOperator::GreaterOrEqual => todo!(),
                    crate::ast::BinaryOperator::And => todo!(),
                    crate::ast::BinaryOperator::Or => todo!(),
                    crate::ast::BinaryOperator::Add => Ok(builder.ins().iadd(left, right)),
                    crate::ast::BinaryOperator::Subtract => todo!(),
                    crate::ast::BinaryOperator::Multiply => todo!(),
                    crate::ast::BinaryOperator::Divide => todo!(),
                }
            }
            Expression::UnaryOperation { value, operator } => todo!(),
        }
    }
}

fn to_cranelift_type(type_: &str) -> Option<Type> {
    match type_ {
        "u8" | "i8" | "bool" => Some(types::I8),
        "u16" | "i16" => Some(types::I16),
        "u32" | "i32" => Some(types::I32),
        "u64" | "i64" => Some(types::I64),
        _ => None,
    }
}
