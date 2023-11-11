use std::{collections::HashMap, fs::File, sync::Arc};

use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        Context,
    },
    prelude::{isa::TargetIsa, *},
};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::IntoDiagnostic;
use tracing::info;

use crate::ast::{Declaration, Expression, Program};

pub struct CodeGen {
    obj_module: ObjectModule,
    functions: Vec<String>,
}

impl CodeGen {
    pub fn new(target: Arc<dyn TargetIsa>) -> miette::Result<Self> {
        let obj_builder =
            ObjectBuilder::new(target, "module", cranelift_module::default_libcall_names())
                .into_diagnostic()?;
        Ok(Self {
            obj_module: ObjectModule::new(obj_builder),
            functions: Vec::new(),
        })
    }

    fn insert_func<S: ToString>(&mut self, name: S) -> u32 {
        self.functions.push(name.to_string());
        self.functions.len() as u32
    }

    pub fn compile(mut self, program: Program) -> miette::Result<()> {
        for decl in &program.declarations {
            match decl {
                Declaration::ExternTask {
                    name,
                    args,
                    returns,
                } => {
                    let _fn_idx = self.insert_func(name);
                    let mut sig = Signature::new(self.obj_module.isa().default_call_conv());
                    for (_, ty) in args {
                        sig.params
                            .push(AbiParam::new(to_cranelift_type(ty).unwrap()))
                    }
                    if let Some(ret) = returns {
                        sig.returns
                            .push(AbiParam::new(to_cranelift_type(ret).unwrap()));
                    }

                    self.obj_module
                        .declare_function(name, cranelift_module::Linkage::Import, &sig)
                        .into_diagnostic()?;
                }
                Declaration::Task {
                    name,
                    args,
                    returns,
                    body,
                } => {
                    let fn_idx = self.insert_func(name);
                    let fn_name = UserFuncName::user(0, fn_idx);
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

                    let mut func = Function::with_name_signature(fn_name, sig);
                    let mut func_ctx = FunctionBuilderContext::new();
                    let mut func_builder = FunctionBuilder::new(&mut func, &mut func_ctx);

                    let fn_entry = func_builder.create_block();
                    func_builder.append_block_params_for_function_params(fn_entry);
                    func_builder.switch_to_block(fn_entry);
                    func_builder.seal_block(fn_entry);

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
                                func_builder.ins().return_(&[]);
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

        let mut file = File::create("a.out").into_diagnostic()?;
        res.object.write_stream(&mut file).unwrap();
        Ok(())
    }

    fn compile_expr(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expression,
    ) -> miette::Result<Value> {
        match expr {
            Expression::Literal(literal) => match literal {
                crate::ast::Literal::Int(i) => Ok(builder.ins().iconst(types::I32, *i)),
                crate::ast::Literal::String(_) => todo!(),
                crate::ast::Literal::Bool(_) => todo!(),
            },
            Expression::Var(v) => todo!(),
            Expression::Call { task, args } => todo!(),
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
