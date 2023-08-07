pub type Program = Vec<AstNode>;

#[derive(Debug)]
pub enum AstNode {
    Task(Task),
    Schedule(Schedule),
}

#[derive(Debug, Default)]
pub struct Task {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub state: Vec<Argument>,
    pub enter: Body,
    pub repeat: Body,
    pub exit: Body,
}

#[derive(Debug, Default)]
pub struct Schedule {
    pub name: String,
    pub body: Body,
}

#[derive(Debug, Clone, Copy)]
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
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        value: Box<Expression>,
    },
    Conditional {
        condition: Box<Expression>,
        then: Body,
        otherwise: Body,
    },
    VarAssign {
        var: String,
        value: Box<Expression>,
    },
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    Ref,
    Deref,
}

impl TryFrom<&str> for UnaryOperator {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "!" => Ok(Self::Not),
            "&" => Ok(Self::Ref),
            "*" => Ok(Self::Deref),
            _ => Err(format!("`{value}` is not a unary operator")),
        }
    }
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

impl TryFrom<&str> for BinaryOperator {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "+" => Ok(BinaryOperator::Plus),
            "-" => Ok(BinaryOperator::Minus),
            "*" => Ok(BinaryOperator::Times),
            "/" => Ok(BinaryOperator::Divide),
            "<" => Ok(BinaryOperator::Less),
            "<=" => Ok(BinaryOperator::LessEqual),
            "==" => Ok(BinaryOperator::Equal),
            "!=" => Ok(BinaryOperator::Unequal),
            ">" => Ok(BinaryOperator::Greater),
            ">=" => Ok(BinaryOperator::GreaterEqual),
            "&&" => Ok(BinaryOperator::And),
            "||" => Ok(BinaryOperator::Or),
            _ => Err(format!("`{value}` is not a valid binary operator")),
        }
    }
}

#[derive(Debug)]
pub enum TblValue {
    Variable(String),
    Int(u64),
    String(String),
}
