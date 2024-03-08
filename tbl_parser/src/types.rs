use std::fmt::Display;

use super::Location;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in &self.declarations {
            write!(f, "{decl}\n\n")?;
        }
        Ok(())
    }
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternTaskParams {
    Variadic,
    WellKnown(Vec<(String, Type)>),
}

impl Display for ExternTaskParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExternTaskParams::Variadic => write!(f, "..."),
            ExternTaskParams::WellKnown(params) => write!(
                f,
                "{}",
                params
                    .iter()
                    .map(|(s, t)| format!("{s}: {t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
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
pub enum Declaration {
    ExternTask {
        name: String,
        params: ExternTaskParams,
        returns: Option<Type>,
    },
    Task {
        location: Location,
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
    Global {
        name: String,
        type_: Type,
        value: Expression,
    },
    Directive {
        name: String,
        args: Vec<Literal>,
    },
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::ExternTask {
                name,
                params,
                returns,
            } => write!(
                f,
                "extern task {name}({params}){};",
                if let Some(returns) = returns {
                    format!(" -> {returns}")
                } else {
                    "".to_string()
                }
            ),
            Declaration::Task {
                location: _,
                name,
                params,
                returns,
                locals,
                body,
            } => {
                write!(
                    f,
                    "task {name}({})",
                    params
                        .iter()
                        .map(|(s, t)| format!("{s}: {t}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                if let Some(returns) = returns {
                    write!(f, " -> {returns}")?;
                }
                if !locals.is_empty() {
                    write!(f, "\n<")?;
                    for (name, ty) in locals {
                        write!(f, "{name}: {ty}, ")?;
                    }
                    write!(f, ">\n")?;
                }
                write!(f, "{{\n")?;
                for stmt in body {
                    writeln!(f, "{stmt}")?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Declaration::Struct { name, members } => todo!(),
            Declaration::Global { name, type_, value } => todo!(),
            Declaration::Directive { name, args } => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Local {
    pub name: String,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
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
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Conditional { test, then, else_ } => write!(
                f,
                r#"if {test} {{
{}
}}"#,
                then.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Statement::Exit => write!(f, "exit;"),
            Statement::Expression(e) => write!(f, "{e};"),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Return(Some(val)) => write!(f, "return {val};"),
            Statement::Schedule { task, args } => todo!(),
            Statement::Assign { location, value } => write!(f, "{location} = {value};"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Var(v) => write!(f, "{v}"),
            Expression::Call { task, args } => write!(
                f,
                "{task}({})",
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::BinaryOperation {
                left,
                right,
                operator,
            } => todo!(),
            Expression::UnaryOperation { value, operator } => todo!(),
            Expression::StructAccess { value, member } => todo!(),
            Expression::Cast { value, to } => todo!(),
            Expression::Index { value, at } => todo!(),
            Expression::SizeOf { value } => todo!(),
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
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{i}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Struct(s) => {
                write!(
                    f,
                    "{{{}}}",
                    s.iter()
                        .map(|(s, t)| format!("{s}: {t}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Literal::Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
