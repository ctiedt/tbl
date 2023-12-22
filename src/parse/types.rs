use std::str::FromStr;

use super::{Location, ParseError};

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
    VarDecl {
        name: String,
        type_: Type,
        value: Expression,
    },
    Assign {
        location: Expression,
        value: Expression,
    },
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

impl FromStr for BinaryOperator {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "==" => Ok(BinaryOperator::Equal),
            "!=" => Ok(BinaryOperator::Unequal),
            ">" => Ok(BinaryOperator::GreaterThan),
            ">=" => Ok(BinaryOperator::GreaterOrEqual),
            "<" => Ok(BinaryOperator::LessThan),
            "<=" => Ok(BinaryOperator::LessOrEqual),
            "&&" => Ok(BinaryOperator::And),
            "||" => Ok(BinaryOperator::Or),
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Subtract),
            "*" => Ok(BinaryOperator::Multiply),
            "/" => Ok(BinaryOperator::Divide),
            other => Err(ParseError::UnknownOperator(other.into())),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Dereference,
    Not,
    Minus,
    Reference,
}

impl FromStr for UnaryOperator {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "*" => Ok(UnaryOperator::Dereference),
            "!" => Ok(UnaryOperator::Not),
            "-" => Ok(UnaryOperator::Minus),
            "&" => Ok(UnaryOperator::Reference),
            other => Err(ParseError::UnknownOperator(other.into())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
    Struct(Vec<(String, Expression)>),
    Array(Vec<Expression>),
}
