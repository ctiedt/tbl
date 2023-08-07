use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, IntType},
    AddressSpace,
};

use crate::ast::{Program, TblType};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            context,
            module,
            builder,
        }
    }

    pub fn compile(&mut self, ast: Program) {
        for node in ast {
            match node {
                crate::ast::AstNode::Task(task) => self.compile_task(task),
                crate::ast::AstNode::Schedule(schedule) => self.compile_schedule(schedule),
            }
        }
    }

    fn compile_task(&self, task: crate::ast::Task) {
        let void_type = self.context.void_type();
        let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let state_fields: Vec<_> = task
            .state
            .iter()
            .map(|field| self.int_type(field.type_).as_basic_type_enum())
            .collect();
        let state_type = self.context.struct_type(state_fields.as_slice(), false);

        let enter_args: Vec<_> = task
            .arguments
            .iter()
            .map(|arg| self.int_type(arg.type_).as_basic_type_enum())
            .chain(std::iter::once(ptr_type))
            .collect();

        let enter_type = void_type.fn_type(enter_args.as_slice(), false);
        let enter_fn = self
            .module
            .add_function(&format!("{}_enter", task.name), enter_type, None);
    }

    fn compile_schedule(&self, schedule: crate::ast::Schedule) {
        todo!()
    }

    fn int_type(&self, ty: TblType) -> IntType {
        match ty {
            crate::ast::TblType::U8 | crate::ast::TblType::I8 => self.context.i8_type(),
            crate::ast::TblType::U16 | crate::ast::TblType::I16 => self.context.i16_type(),
            crate::ast::TblType::U32 | crate::ast::TblType::I32 => self.context.i32_type(),
            crate::ast::TblType::U64 | crate::ast::TblType::I64 => self.context.i64_type(),
        }
    }
}
