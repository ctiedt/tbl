use std::collections::HashMap;

use cranelift::codegen::ir::StackSlot;
use cranelift_module::FuncId;

use crate::ast::Type as TblType;

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
}

#[derive(Clone, Debug)]
pub struct StructContext {
    pub members: Vec<(String, TblType)>,
}

impl StructContext {
    pub fn member_ty(&self, idx: usize) -> &TblType {
        &self.members[idx].1
    }
}

#[derive(Clone, Debug)]
pub struct FunctionContext {
    pub param_types: Vec<TblType>,
    pub returns: Option<TblType>,
    pub vars: HashMap<String, VarInfo>,
    pub func_id: FuncId,
    pub is_variadic: bool,
}

impl FunctionContext {
    pub fn new(
        id: FuncId,
        param_types: Vec<TblType>,
        returns: Option<TblType>,
        is_variadic: bool,
    ) -> Self {
        Self {
            param_types,
            returns,
            vars: HashMap::new(),
            func_id: id,
            is_variadic,
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
