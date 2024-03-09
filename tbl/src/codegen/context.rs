use std::collections::HashMap;

use cranelift::{
    codegen::ir::{immediates::Offset32, InstBuilder, StackSlot, Value},
    frontend::FunctionBuilder,
};
use cranelift_module::FuncId;

use tbl_parser::{
    types::{Expression, Type as TblType},
    Location,
};

#[derive(Default)]
pub struct CodeGenContext {
    pub types: HashMap<String, StructContext>,
    pub functions: HashMap<String, FunctionContext>,
    pub globals: HashMap<String, GlobalContext>,
    func_indices: Vec<String>,
}

impl CodeGenContext {
    pub fn insert_function(&mut self, name: String, function: FunctionContext) -> usize {
        self.func_indices.push(name.clone());
        self.functions.insert(name, function);
        self.functions.len() - 1
    }

    pub fn func_by_idx(&self, idx: usize) -> &FunctionContext {
        &self.functions[&self.func_indices[idx]]
    }

    pub fn type_size(&self, ty: &TblType, ptr_size: usize) -> usize {
        match ty {
            TblType::Any => 0,
            TblType::Bool => 1,
            TblType::Integer { width, .. } => (width / 8) as usize,
            TblType::Array { item, length } => self.type_size(item, ptr_size) * (*length as usize),
            TblType::Pointer(_) => ptr_size,
            TblType::Named(name) => {
                let struct_ty = &self.types[name];
                struct_ty
                    .members
                    .iter()
                    .map(|m| self.type_size(&m.type_, ptr_size))
                    .sum()
            }
            TblType::TaskPtr { .. } => ptr_size,
        }
    }

    pub fn resolve_name<'a>(&'a self, ctx: &'a FunctionContext, name: &str) -> Option<Symbol<'a>> {
        match ctx.locals.as_ref().unwrap().find(name) {
            Some(var) => Some(Symbol::Local(var)),
            None => match self.globals.get(name) {
                Some(global) => Some(Symbol::Global(global)),
                None => self.functions.get(name).map(Symbol::Function),
            },
        }
    }

    pub fn create_struct_type(
        &mut self,
        ty_name: &str,
        members: &[(String, TblType)],
        ptr_size: usize,
    ) {
        fn padding_needed_for(offset: usize, alignment: usize) -> usize {
            let misalignment = offset % alignment;
            if misalignment > 0 {
                // round up to next multiple of `alignment`
                alignment - misalignment
            } else {
                // already a multiple of `alignment`
                0
            }
        }

        let mut layout = vec![];
        let mut offset = 0;
        for (name, member) in members {
            let size = self.type_size(member, ptr_size);
            offset += padding_needed_for(offset, size.next_power_of_two());
            layout.push(StructMember {
                offset,
                name: name.clone(),
                type_: member.clone(),
            });
            offset += size;
        }

        let mut size = offset;
        if let Some(align) = members
            .iter()
            .map(|(_, m)| self.type_size(m, ptr_size))
            .max()
        {
            size += padding_needed_for(size, align);
        }

        self.types.insert(
            ty_name.to_string(),
            StructContext {
                size,
                members: layout,
            },
        );
    }
}

pub enum Symbol<'a> {
    Local(&'a Local),
    Function(&'a FunctionContext),
    Global(&'a GlobalContext),
}

#[derive(Clone, Debug)]
pub struct StructContext {
    pub size: usize,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug)]
pub struct StructMember {
    pub offset: usize,
    pub name: String,
    pub type_: TblType,
}

impl StructContext {
    pub fn member_ty(&self, idx: usize) -> &TblType {
        &self.members[idx].type_
    }

    pub fn member_idx(&self, member: &str) -> usize {
        self.members.iter().position(|m| m.name == member).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContext {
    pub params: Vec<(String, TblType)>,
    pub returns: Option<TblType>,
    pub locals: Option<Locals>,
    pub func_id: FuncId,
    pub is_variadic: bool,
    pub is_external: bool,
    pub location: Location,
}

impl FunctionContext {
    pub fn new(
        id: FuncId,
        params: Vec<(String, TblType)>,
        returns: Option<TblType>,
        is_variadic: bool,
        is_external: bool,
        location: Location,
    ) -> Self {
        Self {
            params,
            returns,
            locals: None,
            func_id: id,
            is_variadic,
            is_external,
            location,
        }
    }

    pub fn init_locals(&mut self, slot: StackSlot) {
        self.locals.replace(Locals { slot, vars: vec![] });
    }

    pub fn locals(&self) -> &Locals {
        self.locals.as_ref().unwrap()
    }

    pub fn declare_local(&mut self, name: &str, type_: TblType, size: u32) -> miette::Result<()> {
        match self.locals.as_mut() {
            Some(locals) => {
                locals.vars.push(Local {
                    name: name.to_string(),
                    type_,
                    size,
                });
                Ok(())
            }
            None => miette::bail!("Tried to insert locals into an external task"),
        }
    }

    pub fn define_local(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        type_: TblType,
        size: u32,
        value: Value,
    ) -> miette::Result<()> {
        match self.locals.as_mut() {
            Some(locals) => {
                let idx = locals.next_idx();
                builder
                    .ins()
                    .stack_store(value, locals.slot, Offset32::new(idx as i32));
                locals.vars.push(Local {
                    name: name.to_string(),
                    type_,
                    size,
                });
                Ok(())
            }
            None => miette::bail!("Tried to insert locals into an external task"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub type_: TblType,
    pub size: u32,
}

#[derive(Debug, Clone)]
pub struct Locals {
    pub slot: StackSlot,
    pub vars: Vec<Local>,
}

impl Locals {
    pub fn find(&self, name: &str) -> Option<&Local> {
        self.vars.iter().find(|v| v.name == name)
    }

    fn next_idx(&self) -> u32 {
        self.vars.iter().map(|l| l.size).sum()
    }

    pub fn offset_of(&self, name: &str) -> u32 {
        self.vars
            .iter()
            .take_while(|l| l.name != name)
            .map(|l| l.size)
            .sum()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalContext {
    pub id: cranelift_module::DataId,
    pub type_: TblType,
    pub initializer: Expression,
}
