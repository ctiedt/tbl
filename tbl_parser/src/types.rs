use std::fmt::Display;

use crate::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Bool,
    Duration,
    Integer {
        signed: bool,
        width: u8,
    },
    Array {
        item: Box<Type>,
        length: u64,
    },
    Pointer(Box<Type>),
    Path(Path),
    TaskPtr {
        params: Vec<Type>,
        returns: Option<Box<Type>>,
    },
    Handle,
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Any => "any".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Duration => "duration".to_string(),
            Type::Integer { signed, width } => {
                format!("{}{width}", if *signed { "i" } else { "u" })
            }
            Type::Array { item, length } => format!("[{}; {length}]", item.name()),
            Type::Pointer(t) => format!("&{}", t.name()),
            Type::Path(p) => p.to_string(),
            Type::TaskPtr { params, returns } => {
                format!(
                    "task({}){}",
                    params
                        .iter()
                        .map(|p| p.name())
                        .collect::<Vec<_>>()
                        .join(", "),
                    if let Some(returns) = returns {
                        format!(" -> {}", returns.name())
                    } else {
                        "".to_string()
                    }
                )
            }
            Type::Handle => "handle".to_string(),
        }
    }

    pub fn path(&self) -> Option<&Path> {
        if let Type::Path(p) = self {
            Some(p)
        } else {
            None
        }
    }

    pub fn any_ptr() -> Self {
        Type::Pointer(Box::new(Type::Any))
    }

    pub fn is_composite(&self) -> bool {
        match self {
            Type::Any => false,
            Type::Bool => false,
            Type::Duration => false,
            Type::Integer { signed, width } => false,
            Type::Array { item, length } => true,
            Type::Pointer(_) => false,
            Type::Path(_) => true,
            Type::TaskPtr { params, returns } => false,
            Type::Handle => false,
        }
    }
}

pub enum ParamKind {
    Named(String, Type),
    Variadic,
    SelfRef,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternTaskParams {
    Variadic,
    WellKnown(Vec<(String, Type)>),
}

impl ExternTaskParams {
    pub fn is_variadic(&self) -> bool {
        matches!(self, ExternTaskParams::Variadic)
    }

    pub fn to_arg_vec(&self) -> Vec<(String, Type)> {
        match self {
            ExternTaskParams::Variadic => vec![],
            ExternTaskParams::WellKnown(params) => params.to_vec(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Path {
    segments: Vec<String>,
}

impl Path {
    pub fn prefixed<P: Into<Path>>(&self, prefix: P) -> Path {
        let mut segments = prefix.into().segments;
        segments.extend_from_slice(&self.segments);
        Path { segments }
    }

    pub fn extend<P: Into<Path>>(&mut self, suffix: P) {
        self.segments.extend(suffix.into().segments);
    }

    pub fn truncate(&mut self, n: usize) {
        self.segments.truncate(self.segments.len() - n);
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn item(&self) -> Option<&str> {
        self.segments.last().map(|x| x.as_str())
    }

    pub fn replace_item<S: ToString>(&mut self, item: S) {
        *self.segments.last_mut().unwrap() = item.to_string();
    }

    pub fn from_segments(segments: Vec<String>) -> Path {
        Path { segments }
    }

    pub fn from_ident(ident: &str) -> Path {
        Path {
            segments: vec![ident.to_string()],
        }
    }
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Path {
            segments: vec![value],
        }
    }
}

impl From<&str> for Path {
    fn from(value: &str) -> Self {
        Path {
            segments: vec![value.to_string()],
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.join("::"))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub span: Span,
    pub kind: DeclarationKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationKind {
    ExternTask {
        path: Path,
        params: ExternTaskParams,
        returns: Option<Type>,
    },
    Task {
        path: Path,
        params: Vec<(String, Type)>,
        returns: Option<Type>,
        body: Vec<Statement>,
    },
    Struct {
        path: Path,
        members: Vec<(String, Type)>,
    },
    Enum {
        path: Path,
        variants: Vec<(String, Vec<(String, Type)>)>,
    },
    Global {
        path: Path,
        type_: Type,
        value: Expression,
    },
    Directive {
        name: String,
        args: Vec<Literal>,
    },
    Use {
        module: String,
    },
    ExternGlobal {
        path: Path,
        type_: Type,
    },
}

impl DeclarationKind {
    pub fn with_span(self, span: Span) -> Declaration {
        Declaration { span, kind: self }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Local {
    pub name: String,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Conditional {
        test: Expression,
        then: Box<Statement>,
        else_: Box<Statement>,
    },
    Exit,
    Expression(Expression),
    Return(Option<Expression>),
    Assign {
        location: Expression,
        value: Expression,
    },
    Definition {
        name: String,
        type_: Type,
        value: Expression,
    },
    Declaration {
        name: String,
        type_: Type,
    },
    Loop {
        body: Box<Statement>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Match {
        value: Expression,
        branches: Vec<(MatchPattern, Statement)>,
    },
    Break,
    Attach {
        handle: Expression,
        task: Expression,
    },
    Once {
        stmt: Box<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MatchPattern {
    Ident(String),
    Expr(Expression),
    Any,
}

impl StatementKind {
    pub fn with_span(self, span: Span) -> Statement {
        Statement { span, kind: self }
    }
}

impl Statement {
    pub fn referenced_vars(&self) -> Vec<(&Path, Span)> {
        match &self.kind {
            StatementKind::Conditional { test, then, else_ } => {
                let mut vars = test.referenced_vars();
                vars.extend(then.referenced_vars());
                vars.extend(else_.referenced_vars());
                vars
            }
            StatementKind::Exit => vec![],
            StatementKind::Expression(e) => e.referenced_vars(),
            StatementKind::Return(v) => {
                if let Some(v) = v {
                    v.referenced_vars()
                } else {
                    vec![]
                }
            }
            StatementKind::Assign { location, value } => {
                let mut vars = location.referenced_vars();
                vars.extend(value.referenced_vars());
                vars
            }
            StatementKind::Definition {
                name: _,
                type_: _,
                value,
            } => value.referenced_vars(),
            StatementKind::Declaration { .. } => vec![],
            StatementKind::Loop { body } => body.referenced_vars(),
            StatementKind::Block { statements } => {
                let mut vars = vec![];
                for stmt in statements {
                    vars.append(&mut stmt.referenced_vars());
                }
                vars
            }
            StatementKind::Match { value, branches } => {
                let mut vars = value.referenced_vars();
                for (_, stmt) in branches {
                    vars.append(&mut stmt.referenced_vars());
                }
                vars
            }
            StatementKind::Attach { handle, task } => {
                let mut vars = handle.referenced_vars();
                vars.extend(task.referenced_vars());
                vars
            }
            StatementKind::Break => vec![],
            StatementKind::Once { stmt } => stmt.referenced_vars(),
        }
    }

    pub fn ends_with_jump(&self) -> bool {
        match &self.kind {
            StatementKind::Conditional { .. } => false,
            StatementKind::Exit => true,
            StatementKind::Expression(_expression) => false,
            StatementKind::Return(_expression) => true,
            StatementKind::Assign { .. } => false,
            StatementKind::Definition { .. } => false,
            StatementKind::Declaration { .. } => false,
            StatementKind::Loop { body } => body.ends_with_jump(),
            StatementKind::Block { statements } => {
                if let Some(stmt) = statements.last() {
                    stmt.ends_with_jump()
                } else {
                    false
                }
            }
            StatementKind::Match { .. } => false,
            StatementKind::Break => true,
            StatementKind::Attach { .. } => false,
            StatementKind::Once { stmt } => stmt.ends_with_jump(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Literal(Literal),
    Path(Path),
    Call {
        task: Box<Expression>,
        args: Vec<Expression>,
    },
    MethodCall {
        var: Box<Expression>,
        task: Box<Expression>,
        args: Vec<Expression>,
    },
    BinaryOperation {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: BinaryOperator,
    },
    UnaryOperation {
        value: Box<Expression>,
        operator: UnaryOperator,
    },
    StructAccess {
        value: Box<Expression>,
        member: String,
    },
    Cast {
        value: Box<Expression>,
        to: Type,
    },
    Index {
        value: Box<Expression>,
        at: Box<Expression>,
    },
    SizeOf {
        value: Type,
    },
    Schedule {
        task: String,
        args: Vec<Expression>,
        period: Box<Expression>,
    },
}

impl ExpressionKind {
    pub fn with_span(self, span: Span) -> Expression {
        Expression { span, kind: self }
    }
}

impl Expression {
    pub fn referenced_vars(&self) -> Vec<(&Path, Span)> {
        match &self.kind {
            ExpressionKind::Literal(_) => vec![],
            ExpressionKind::Path(p) => vec![(p, self.span.clone())],
            ExpressionKind::Call { task, args } => {
                let mut vars = task.referenced_vars();
                for arg in args {
                    vars.extend(arg.referenced_vars());
                }
                vars
            }
            ExpressionKind::MethodCall { var, task, args } => {
                let mut vars = task.referenced_vars();
                vars.extend(var.referenced_vars());
                for arg in args {
                    vars.extend(arg.referenced_vars());
                }
                vars
            }
            ExpressionKind::BinaryOperation { left, right, .. } => {
                let mut vars = left.referenced_vars();
                vars.extend(right.referenced_vars());
                vars
            }
            ExpressionKind::UnaryOperation { value, .. } => value.referenced_vars(),
            ExpressionKind::StructAccess { value, .. } => value.referenced_vars(),
            ExpressionKind::Cast { value, .. } => value.referenced_vars(),
            ExpressionKind::Index { value, .. } => value.referenced_vars(),
            ExpressionKind::SizeOf { .. } => vec![],
            ExpressionKind::Schedule { args, .. } => {
                let mut vars = vec![];
                for arg in args {
                    vars.extend(arg.referenced_vars());
                }
                vars
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Equal,
    Unequal,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    And,
    Or,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Dereference,
    Not,
    Minus,
    Reference,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
    Struct(Vec<(String, Expression)>),
    Array(Vec<Expression>),
    Time(u64),
    Enum {
        variant: String,
        members: Vec<(String, Expression)>,
    },
}

#[derive(Clone, Debug)]
pub enum PostfixOperator {
    StructAccess { member: String },
    Index { at: Expression },
    Cast { to: Type },
    Call { args: Vec<Expression> },
}
