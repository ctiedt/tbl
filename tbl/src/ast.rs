use std::string::ParseError;

#[derive(Debug)]
pub enum AstNode {
    Task(Task),
    Schedule(Schedule),
}

#[derive(Debug, Default)]
pub struct Task {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub enter: Body,
    pub repeat: Body,
    pub exit: Body,
}

#[derive(Debug, Default)]
pub struct Schedule {
    pub name: String,
    pub body: Body,
}

#[derive(Debug)]
pub enum TblType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

impl TryFrom<&str> for TblType {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            _ => Err("Failed to parse type".into()),
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub type_: TblType,
}

pub type Body = Vec<Expression>;

#[derive(Debug)]
pub enum Expression {
    Value(TblValue),
    Exit,
    BinaryOperation {
        operator: BinaryOperator,
        left: TblValue,
        right: TblValue,
    },
    UnaryOperation {
        operator: UnaryOperator,
        value: TblValue,
    },
    Conditional {
        condition: Box<Expression>,
        then: Body,
        otherwise: Body,
    },
    VarAssign {
        var: String,
        value: TblValue,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Divide,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    Unequal,
    And,
    Or,
}

#[derive(Debug)]
pub enum TblValue {
    Variable(String),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    Int(u64),
    String(String),
}
