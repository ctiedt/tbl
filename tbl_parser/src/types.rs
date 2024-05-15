use crate::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Any,
    Bool,
    Integer {
        signed: bool,
        width: u8,
    },
    Array {
        item: Box<Type>,
        length: u64,
    },
    Pointer(Box<Type>),
    Named(String),
    TaskPtr {
        params: Vec<Type>,
        returns: Option<Box<Type>>,
    },
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Any => "any".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Integer { signed, width } => {
                format!("{}{width}", if *signed { "i" } else { "u" })
            }
            Type::Array { item, length } => format!("[{}; {length}]", item.name()),
            Type::Pointer(t) => format!("&{}", t.name()),
            Type::Named(n) => n.to_string(),
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
        }
    }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub span: Span,
    pub kind: DeclarationKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationKind {
    ExternTask {
        name: String,
        params: ExternTaskParams,
        returns: Option<Type>,
    },
    Task {
        name: String,
        params: Vec<(String, Type)>,
        returns: Option<Type>,
        locals: Vec<(String, Type)>,
        body: Vec<Statement>,
    },
    Struct {
        name: String,
        members: Vec<(String, Type)>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<(String, Type)>)>,
    },
    Global {
        name: String,
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
        name: String,
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
        then: Vec<Statement>,
        else_: Vec<Statement>,
    },
    Exit,
    Expression(Expression),
    Return(Option<Expression>),
    Schedule {
        task: String,
        args: Vec<Expression>,
    },
    Assign {
        location: Expression,
        value: Expression,
    },
    Loop {
        body: Vec<Statement>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Match {
        value: Expression,
        branches: Vec<(MatchPattern, Statement)>,
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
    pub fn referenced_vars(&self) -> Vec<(&str, Span)> {
        match &self.kind {
            StatementKind::Conditional { test, then, else_ } => {
                let mut vars = test.referenced_vars();
                for stmt in then {
                    vars.extend(stmt.referenced_vars());
                }
                for stmt in else_ {
                    vars.extend(stmt.referenced_vars());
                }
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
            StatementKind::Schedule { args, .. } => {
                let mut vars = vec![];
                for arg in args {
                    vars.extend(arg.referenced_vars());
                }
                vars
            }
            StatementKind::Assign { location, value } => {
                let mut vars = location.referenced_vars();
                vars.extend(value.referenced_vars());
                vars
            }
            StatementKind::Loop { body } => {
                let mut vars = vec![];
                for stmt in body {
                    vars.extend(stmt.referenced_vars());
                }
                vars
            }
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
    Var(String),
    Call {
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
}

impl ExpressionKind {
    pub fn with_span(self, span: Span) -> Expression {
        Expression { span, kind: self }
    }
}

impl Expression {
    pub fn referenced_vars(&self) -> Vec<(&str, Span)> {
        match &self.kind {
            ExpressionKind::Literal(_) => vec![],
            ExpressionKind::Var(v) => vec![(v, self.span.clone())],
            ExpressionKind::Call { task, args } => {
                let mut vars = task.referenced_vars();
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
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    Enum {
        variant: String,
        members: Vec<(String, Expression)>,
    },
}
