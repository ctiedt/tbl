use std::collections::HashMap;

use cranelift::codegen::ir::StackSlot;
use cranelift_module::FuncId;

use crate::ast::{Location, Type as TblType};

#[derive(Default)]
pub struct CodeGenContext {
    pub types: HashMap<String, StructContext>,
    pub functions: HashMap<String, FunctionContext>,
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

    pub fn func_by_idx_mut(&mut self, idx: usize) -> Option<&mut FunctionContext> {
        self.functions.get_mut(&self.func_indices[idx])
    }

    pub fn insert_type(&mut self, name: String, type_: StructContext) {
        self.types.insert(name, type_);
    }

    pub fn type_size(&self, ty: &TblType, ptr_size: u8) -> u8 {
        match ty {
            TblType::Bool => 1,
            TblType::Integer { width, .. } => width / 8,
            TblType::Array { item, length } => self.type_size(item, ptr_size) * (*length as u8),
            TblType::Pointer(_) => ptr_size,
            TblType::Named(name) => {
                let struct_ty = &self.types[name];
                struct_ty
                    .members
                    .iter()
                    .map(|(_, t)| self.type_size(t, ptr_size))
                    .sum()
            }
            TblType::TaskPtr { .. } => ptr_size,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructContext {
    pub members: Vec<(String, TblType)>,
}

impl StructContext {
    pub fn member_ty(&self, idx: usize) -> &TblType {
        &self.members[idx].1
    }

    pub fn member_idx(&self, member: &str) -> usize {
        self.members.iter().position(|(m, _)| m == member).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContext {
    pub params: Vec<(String, TblType)>,
    pub returns: Option<TblType>,
    pub vars: HashMap<String, VarInfo>,
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
            vars: HashMap::new(),
            func_id: id,
            is_variadic,
            is_external,
            location,
        }
    }

    pub fn declare_var(&mut self, name: &str, type_: TblType, slot: StackSlot) -> VarInfo {
        self.vars.insert(name.to_string(), VarInfo { slot, type_ });
        self.vars[name].clone()
    }
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub slot: StackSlot,
    pub type_: TblType,
}
