use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
};

use cranelift::{
    codegen::ir::{immediates::Offset32, Block, InstBuilder, StackSlot, Value},
    frontend::FunctionBuilder,
};
use cranelift_module::FuncId;

use tbl_parser::{
    types::{Expression, Path, Type as TblType},
    Span,
};

use crate::error::{CodegenError, CodegenErrorKind, CodegenResult};

#[derive(Default)]
pub struct CodeGenContext {
    pub types: HashMap<Path, TypeContext>,
    pub global_scope: Scope,
}

impl CodeGenContext {
    pub fn insert_function(&mut self, path: Path, function: FunctionContext) {
        self.global_scope.insert_function(path, function);
    }

    pub fn func_by_idx(&self, idx: u32) -> Option<Ref<'_, FunctionContext>> {
        self.global_scope.functions().find_map(|(_, f)| {
            if f.func_id.as_u32() == idx {
                Some(f)
            } else {
                None
            }
        })
    }

    pub fn func_by_idx_mut(&mut self, idx: u32) -> Option<RefMut<'_, FunctionContext>> {
        self.global_scope.functions_mut().find_map(|(_, f)| {
            if f.func_id.as_u32() == idx {
                Some(f)
            } else {
                None
            }
        })
    }

    pub fn functions(&self) -> impl Iterator<Item = (&Path, Ref<'_, FunctionContext>)> {
        self.global_scope.functions()
    }

    pub fn get_function(&self, path: &Path) -> Option<Ref<'_, FunctionContext>> {
        self.global_scope
            .functions()
            .find_map(|(n, f)| if n == path { Some(f) } else { None })
    }

    pub fn get_function_mut(&mut self, path: &Path) -> Option<RefMut<'_, FunctionContext>> {
        self.global_scope
            .functions_mut()
            .find_map(|(n, f)| if n == path { Some(f) } else { None })
    }

    pub fn type_size(&self, ty: &TblType, ptr_size: usize) -> usize {
        match ty {
            TblType::Any => 0,
            TblType::Bool => 1,
            TblType::Integer { width, .. } => (width / 8) as usize,
            TblType::Array { item, length } => self.type_size(item, ptr_size) * (*length as usize),
            TblType::Pointer(_) => ptr_size,
            TblType::Path(p) => match &self.types[p] {
                TypeContext::Struct(struct_ty) => struct_ty
                    .members
                    .iter()
                    .map(|m| self.type_size(&m.type_, ptr_size))
                    .sum(),
                TypeContext::Enum(enum_ty) => {
                    enum_ty
                        .variants
                        .iter()
                        .map(|(_, v)| {
                            v.members
                                .iter()
                                .map(|m| self.type_size(&m.type_, ptr_size))
                                .sum::<usize>()
                        })
                        .max()
                        .unwrap()
                        + 1
                }
            },
            TblType::TaskPtr { .. } => ptr_size,
            TblType::Handle => ptr_size,
            TblType::Duration => 8,
        }
    }

    pub fn create_struct_type(
        &mut self,
        ty_name: Path,
        members: &[(String, TblType)],
        ptr_size: usize,
    ) {
        self.types.insert(
            ty_name,
            TypeContext::Struct(self.create_anonymous_struct_type(members, ptr_size)),
        );
    }

    fn create_anonymous_struct_type(
        &self,
        members: &[(String, TblType)],
        ptr_size: usize,
    ) -> StructContext {
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

        StructContext {
            size,
            members: layout,
        }
    }

    pub fn create_enum_type(
        &mut self,
        ty_name: Path,
        variants: &[(String, Vec<(String, TblType)>)],
        ptr_size: usize,
    ) {
        let mut layout = vec![];
        for (name, ty) in variants {
            let variant = self.create_anonymous_struct_type(ty, ptr_size);
            layout.push((name.clone(), variant));
        }

        self.types
            .insert(ty_name, TypeContext::Enum(EnumContext { variants: layout }));
    }

    pub fn get_enum_type(&self, path: &Path) -> Option<&EnumContext> {
        match self.types.get(path)? {
            TypeContext::Struct(_) => None,
            TypeContext::Enum(t) => Some(t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbol {
    Local(Rc<RefCell<Local>>),
    Function(Rc<RefCell<FunctionContext>>),
    Global(Rc<RefCell<GlobalContext>>),
}

impl Symbol {
    pub fn type_(&self) -> TblType {
        match self {
            Symbol::Local(l) => l.borrow().type_.clone(),
            Symbol::Function(f) => TblType::TaskPtr {
                params: f.borrow().params.iter().map(|(_, t)| t).cloned().collect(),
                returns: f.borrow().returns.clone().map(Box::new),
            },
            Symbol::Global(g) => g.borrow().type_.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeContext {
    Struct(StructContext),
    Enum(EnumContext),
}

impl TypeContext {
    pub fn unwrap_struct(&self) -> &StructContext {
        match self {
            TypeContext::Struct(s) => s,
            _ => panic!("Tried to unwrap non-struct type as struct"),
        }
    }

    pub fn unwrap_enum(&self) -> &EnumContext {
        match self {
            TypeContext::Enum(e) => e,
            _ => panic!("Tried to unwrap non-enum type as enum"),
        }
    }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionContext {
    pub params: Vec<(String, TblType)>,
    pub returns: Option<TblType>,
    pub locals: Option<Locals>,
    pub func_id: FuncId,
    pub is_variadic: bool,
    pub is_external: bool,
    pub span: Span,
    pub loop_labels: Vec<Block>,
    pub scope: Scope,
    pub once_blocks: u8,
}

impl FunctionContext {
    pub fn new(
        id: FuncId,
        params: Vec<(String, TblType)>,
        returns: Option<TblType>,
        is_variadic: bool,
        is_external: bool,
        span: Span,
        parent: Scope,
    ) -> Self {
        Self {
            params,
            returns,
            locals: None,
            func_id: id,
            is_variadic,
            is_external,
            span,
            loop_labels: vec![],
            scope: Scope::create_child(parent),
            once_blocks: 0,
        }
    }

    pub fn add_once(&mut self) -> u8 {
        self.once_blocks += 1;
        self.once_blocks - 1
    }

    pub fn init_locals(&mut self, slot: StackSlot, locals_size: u32) {
        self.locals.replace(Locals {
            slot,
            vars: vec![],
            size: locals_size,
        });
    }

    pub fn locals(&self) -> &Locals {
        self.locals.as_ref().unwrap()
    }

    pub fn declare_local(
        &mut self,
        name: &str,
        type_: TblType,
        foreign: bool,
        size: u32,
    ) -> Result<(), CodegenErrorKind> {
        match self.locals.as_mut() {
            Some(locals) => {
                let local = Local {
                    name: name.into(),
                    type_,
                    foreign,
                    size,
                };
                self.scope.insert_local(name, local.clone());
                locals.vars.push(local);
                Ok(())
            }
            None => Err(CodegenErrorKind::ExternTaskError),
        }
    }

    pub fn define_local(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        type_: TblType,
        foreign: bool,
        size: u32,
        value: Value,
    ) -> Result<(), CodegenErrorKind> {
        match self.locals.as_mut() {
            Some(locals) => {
                let idx = locals.next_idx();
                builder
                    .ins()
                    .stack_store(value, locals.slot, Offset32::new(idx as i32));
                let local = Local {
                    name: name.into(),
                    type_,
                    foreign,
                    size,
                };
                self.scope.insert_local(name, local.clone());
                locals.vars.push(local);
                Ok(())
            }
            None => Err(CodegenErrorKind::ExternTaskError),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Local {
    pub name: Path,
    pub type_: TblType,
    pub foreign: bool,
    pub size: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Locals {
    pub slot: StackSlot,
    pub vars: Vec<Local>,
    pub size: u32,
}

impl Locals {
    fn next_idx(&self) -> u32 {
        self.vars.iter().map(|l| l.size).sum()
    }

    pub fn offset_of(&self, path: &Path) -> u32 {
        self.vars
            .iter()
            .take_while(|l| &l.name != path)
            .map(|l| l.size)
            .sum()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalContext {
    pub id: cranelift_module::DataId,
    pub type_: TblType,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct EnumContext {
    pub variants: Vec<(String, StructContext)>,
}

impl EnumContext {
    pub fn variant_tag(&self, variant: &str) -> usize {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, (v, _))| v == variant)
            .map(|(idx, _)| idx)
            .unwrap()
    }

    pub fn get_variant(&self, variant: &str) -> CodegenResult<StructContext> {
        self.variants
            .iter()
            .find_map(|(name, ctx)| {
                if name == variant {
                    Some(ctx.clone())
                } else {
                    None
                }
            })
            .ok_or(CodegenError::no_such_variant(0..0, variant))
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<Path, Symbol>,
}

impl Scope {
    // pub fn create_child(self: Rc<Self>) -> Self {
    //     Scope {
    //         parent: Some(Rc::clone(&self)),
    //         ..Default::default()
    //     }
    // }

    pub fn create_child(parent: Self) -> Self {
        Scope {
            parent: Some(Box::new(parent)),
            ..Default::default()
        }
    }

    pub fn get(&self, key: &Path) -> Option<Symbol> {
        match self.variables.get(key) {
            Some(value) => Some(value.clone()),
            None => self.parent.as_ref().map(|p| p.get(key)).flatten(),
        }
    }

    pub fn get_tcb(&self) -> Option<Ref<'_, Local>> {
        match self.variables.get(&"__tcb".into()) {
            Some(Symbol::Local(tcb)) => Some(tcb.borrow()),
            _ => None,
        }
    }

    pub fn symbols(&self) -> HashMap<Path, Symbol> {
        let mut symbols = HashMap::new();
        match &self.parent {
            Some(p) => {
                for (path, sym) in p.symbols() {
                    symbols.insert(path, sym);
                }
                for (path, sym) in self.variables.iter() {
                    symbols.insert(path.clone(), sym.clone());
                }
            }
            None => {
                symbols = self.variables.clone();
            }
        }
        symbols
    }

    pub fn insert_global(&mut self, key: impl Into<Path>, value: GlobalContext) {
        self.variables
            .insert(key.into(), Symbol::Global(Rc::new(RefCell::new(value))));
    }

    pub fn insert_local(&mut self, key: impl Into<Path>, value: Local) {
        let key = key.into();
        self.variables
            .insert(key.into(), Symbol::Local(Rc::new(RefCell::new(value))));
    }

    pub fn insert_function(&mut self, key: impl Into<Path>, value: FunctionContext) {
        self.variables
            .insert(key.into(), Symbol::Function(Rc::new(RefCell::new(value))));
    }

    pub fn functions(&self) -> impl Iterator<Item = (&Path, Ref<'_, FunctionContext>)> {
        self.variables.iter().filter_map(|(n, v)| match v {
            Symbol::Function(f) => Some((n, f.borrow())),
            _ => None,
        })
    }

    pub fn functions_mut(&mut self) -> impl Iterator<Item = (&Path, RefMut<'_, FunctionContext>)> {
        self.variables.iter_mut().filter_map(|(n, v)| match v {
            Symbol::Function(f) => Some((n, f.borrow_mut())),
            _ => None,
        })
    }
}
