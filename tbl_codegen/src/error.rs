use cranelift::codegen::verifier::VerifierErrors;
use cranelift_module::ModuleError;
use std::{fmt::Display, ops::Range};
use tbl_parser::types::{ExpressionKind, Type};
use thiserror::Error;

#[derive(Debug, Error)]
pub struct CodegenError {
    pub span: Range<usize>,
    #[source]
    pub kind: CodegenErrorKind,
}

impl CodegenError {
    #[inline]
    pub fn new(span: Range<usize>, kind: CodegenErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn unknown_task(span: Range<usize>, name: impl Into<String>) -> Self {
        Self::new(span, CodegenErrorKind::UnknownTask(name.into()))
    }

    pub fn unknown_task_id(span: Range<usize>, id: u32) -> Self {
        Self::new(span, CodegenErrorKind::UnknownTaskId(id))
    }

    pub fn unknown_symbol(span: Range<usize>, name: impl Into<String>) -> Self {
        Self::new(span, CodegenErrorKind::UnknownSymbol(name.into()))
    }

    pub fn missing_type_hint(span: Range<usize>) -> Self {
        Self::new(span, CodegenErrorKind::MissingTypeHint)
    }

    pub fn freestanding_composite_literal(span: Range<usize>) -> Self {
        Self::new(span, CodegenErrorKind::FreestandingCompositeLiteral)
    }

    pub fn conflicting_types(span: Range<usize>, left: Type, right: Type) -> Self {
        Self::new(span, CodegenErrorKind::ConflictingTypes { left, right })
    }

    pub fn illegal_cast(span: Range<usize>, left: Type, right: Type) -> Self {
        Self::new(span, CodegenErrorKind::IllegalCast { left, right })
    }

    pub fn illegal_lvalue(span: Range<usize>, expr: ExpressionKind) -> Self {
        Self::new(span, CodegenErrorKind::IllegalLvalue(expr))
    }

    pub fn empty_loop_stack(span: Range<usize>) -> Self {
        Self::new(span, CodegenErrorKind::EmptyLoopStack)
    }

    pub fn cannot_convert_to_cranelift_type(span: Range<usize>, type_: Type) -> Self {
        Self::new(span, CodegenErrorKind::CannotConvertToCraneliftType(type_))
    }

    pub fn unknown_cast_from(span: Range<usize>) -> Self {
        Self::new(span, CodegenErrorKind::UnknownCastFrom)
    }

    pub fn no_such_variant(span: Range<usize>, variant: impl Into<String>) -> Self {
        Self::new(span, CodegenErrorKind::NoSuchVariant(variant.into()))
    }
}

impl Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Codegen Error at {}..{}: {}",
            self.span.start, self.span.end, self.kind
        )
    }
}

#[derive(Debug, Error)]
pub enum CodegenErrorKind {
    #[error("no task named `{0}`")]
    UnknownTask(String),
    #[error("no task with id `{0}`")]
    UnknownTaskId(u32),
    #[error("unknown symbol: `{0}`")]
    UnknownSymbol(String),
    #[error("module error: {0}")]
    ModuleError(#[from] ModuleError),
    #[error("cannot insert local into extern task")]
    ExternTaskError,
    #[error("cranelift codegen error: {0}")]
    CraneliftCodegenError(#[from] cranelift::codegen::CodegenError),
    #[error("verifier errors: {0}")]
    VerifierErrors(#[from] VerifierErrors),
    #[error("io error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("missing type hint")]
    MissingTypeHint,
    #[error("freestanding composite literal")]
    FreestandingCompositeLiteral,
    #[error("conflicting types `{left:?}` and `{right:?}`")]
    ConflictingTypes { left: Type, right: Type },
    #[error("cannot cast from `{left:?}` to `{right:?}`")]
    IllegalCast { left: Type, right: Type },
    #[error("illegal expression for lvalue: {0:?}")]
    IllegalLvalue(ExpressionKind),
    #[error("empty loop stack")]
    EmptyLoopStack,
    #[error("cannot convert type {0:?} to cranelift type")]
    CannotConvertToCraneliftType(Type),
    #[error("type to cast from must be known")]
    UnknownCastFrom,
    #[error("dwarf error: {0}")]
    DwarfError(#[from] cranelift::codegen::gimli::write::Error),
    #[error("no variant named `{0}`")]
    NoSuchVariant(String),
}

pub type CodegenResult<T> = Result<T, CodegenError>;
