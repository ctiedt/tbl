use std::str::FromStr;

use miette::{miette, Diagnostic, IntoDiagnostic};
use pest::iterators::{Pair, Pairs};
use thiserror::Error;

use crate::Rule;

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("unknown operator")]
    UnknownOperator(String),
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Location {
    pub line: u64,
    pub column: u64,
}

impl From<pest::Span<'_>> for Location {
    fn from(value: pest::Span) -> Self {
        let (line, column) = value.start_pos().line_col();
        Self {
            line: line as u64,
            column: column as u64,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Integer { signed: bool, width: u8 },
    Array { item: Box<Type>, length: u64 },
    Pointer(Box<Type>),
    Named(String),
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Bool => "bool".to_string(),
            Type::Integer { signed, width } => {
                format!("{}{width}", if *signed { "i" } else { "u" })
            }
            Type::Array { item, length } => format!("[{}; {length}]", item.name()),
            Type::Pointer(t) => format!("&{}", t.name()),
            Type::Named(n) => n.to_string(),
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
    PointerAssign {
        ptr: String,
        value: Expression,
    },
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
    VarAssign {
        name: String,
        value: Expression,
    },
    StructAssign {
        var: Expression,
        member: String,
        value: Expression,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Var(String),
    Call {
        task: String,
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
}

pub fn parse_type(tokens: Pair<'_, Rule>) -> miette::Result<Type> {
    let pair = tokens
        .into_inner()
        .next()
        .ok_or(miette!("Declaration should contain one inner pair"))?;
    match pair.as_rule() {
        Rule::bool => Ok(Type::Bool),
        Rule::integer => {
            let (signed, width) = pair.as_str().split_at(1);
            let signed = match signed {
                "u" => false,
                "i" => true,
                _ => unreachable!(),
            };
            Ok(Type::Integer {
                signed,
                width: width.parse().into_diagnostic()?,
            })
        }
        Rule::array => {
            let inner = pair.into_inner();
            let item = parse_type(
                inner
                    .find_first_tagged("item")
                    .ok_or(miette!("Array must have a type"))?,
            )?;
            let length = inner
                .find_first_tagged("len")
                .ok_or(miette!("Array must have a length"))?
                .as_str()
                .parse()
                .into_diagnostic()?;

            Ok(Type::Array {
                item: Box::new(item),
                length,
            })
        }
        Rule::pointer_to => {
            let inner = parse_type(
                pair.into_inner()
                    .next()
                    .ok_or(miette!("Pointer must reference a type"))?,
            )?;

            Ok(Type::Pointer(Box::new(inner)))
        }
        Rule::ident => Ok(Type::Named(pair.as_str().to_owned())),
        _ => unreachable!(),
    }
}

pub fn parse_decl(tokens: Pair<'_, Rule>) -> miette::Result<Declaration> {
    let pair = tokens
        .into_inner()
        .next()
        .ok_or(miette!("Declaration should contain one inner pair"))?;
    match pair.as_rule() {
        Rule::task => {
            let location = pair.as_span().into();
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Task has no name"))?
                .as_str()
                .to_string();
            let returns = pairs
                .find_first_tagged("returns")
                .map(|p| parse_type(p))
                .transpose()?;
            let param_names = pairs
                .clone()
                .find_tagged("param")
                .map(|p| p.as_str().to_string());
            let param_types: miette::Result<Vec<Type>> = pairs
                .clone()
                .find_tagged("type")
                .map(|p| parse_type(p))
                .collect();
            let params = param_names.zip(param_types?).collect();
            let body: miette::Result<Vec<Statement>> =
                pairs.find_tagged("body").map(parse_stmt).collect();
            let body = body?;

            Ok(Declaration::Task {
                location,
                name,
                params,
                returns,
                body,
            })
        }
        Rule::extern_task => {
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Task has no name"))?
                .as_str()
                .to_string();
            let returns = pairs
                .find_first_tagged("returns")
                .map(|p| parse_type(p))
                .transpose()?;
            let params = if pairs.clone().find_first_tagged("variadic").is_some() {
                ExternTaskParams::Variadic
            } else {
                let param_names = pairs
                    .clone()
                    .find_tagged("param")
                    .map(|p| p.as_str().to_string());
                let param_types: miette::Result<Vec<Type>> = pairs
                    .clone()
                    .find_tagged("type")
                    .map(|p| parse_type(p))
                    .collect();
                let params = param_names.zip(param_types?).collect();
                ExternTaskParams::WellKnown(params)
            };

            Ok(Declaration::ExternTask {
                name,
                params,
                returns,
            })
        }
        Rule::r#struct => {
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Struct has no name"))?
                .as_str()
                .to_string();
            let param_names = pairs
                .clone()
                .find_tagged("param")
                .map(|p| p.as_str().to_string());
            let param_types: miette::Result<Vec<Type>> = pairs
                .clone()
                .find_tagged("type")
                .map(|p| parse_type(p))
                .collect();
            let members = param_names.zip(param_types?).collect();

            Ok(Declaration::Struct { name, members })
        }
        r => unreachable!("{r:?}"),
    }
}

pub fn parse_stmt(tokens: Pair<'_, Rule>) -> miette::Result<Statement> {
    let pair = tokens
        .into_inner()
        .next()
        .ok_or(miette!("Statement should contain one inner pair"))?;

    match pair.as_node_tag() {
        Some("cond") => {
            let pairs = pair.into_inner();
            let test = parse_expr(
                pairs
                    .find_first_tagged("test")
                    .ok_or(miette!("Condition has no test"))?,
            )?;
            let then: miette::Result<Vec<Statement>> =
                pairs.clone().find_tagged("then").map(parse_stmt).collect();
            let else_: miette::Result<Vec<Statement>> =
                pairs.clone().find_tagged("else").map(parse_stmt).collect();
            Ok(Statement::Conditional {
                test,
                then: then?,
                else_: else_?,
            })
        }
        Some("sched") => {
            let pairs = pair.into_inner();
            let task = pairs
                .find_first_tagged("task")
                .ok_or(miette!("Task has no name"))?
                .as_str()
                .to_string();
            let args: miette::Result<Vec<Expression>> =
                pairs.clone().find_tagged("arg").map(parse_expr).collect();
            Ok(Statement::Schedule { task, args: args? })
        }
        Some("exit") => Ok(Statement::Exit),
        Some("expr") => {
            let pair = pair
                .into_inner()
                .next()
                .ok_or(miette!("Expression should contain one inner pair"))?;
            let expr = parse_expr(pair)?;
            Ok(Statement::Expression(expr))
        }
        Some("return") => {
            let value = pair.into_inner().next().map(parse_expr).transpose()?;
            Ok(Statement::Return(value))
        }
        Some("var_decl") => {
            let inner = pair.into_inner();
            let name = inner
                .find_first_tagged("name")
                .ok_or(miette!("Variable has no name"))?
                .as_str()
                .to_string();
            let type_ = parse_type(
                inner
                    .find_first_tagged("type")
                    .ok_or(miette!("Variable has no type"))?,
            )?;
            let value = parse_expr(
                inner
                    .find_first_tagged("value")
                    .ok_or(miette!("Variable has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Variable value should contain a pair"))?,
            )?;
            Ok(Statement::VarDecl { name, type_, value })
        }
        Some("var_assign") => {
            let inner = pair.into_inner();
            let name = inner
                .find_first_tagged("name")
                .ok_or(miette!("Variable has no name"))?
                .as_str()
                .to_string();

            let value = parse_expr(
                inner
                    .find_first_tagged("value")
                    .ok_or(miette!("Variable has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Variable value should contain a pair"))?,
            )?;
            Ok(Statement::VarAssign { name, value })
        }
        Some("ptr_assign") => {
            let inner = pair.into_inner();
            let ptr = inner
                .find_first_tagged("ptr")
                .ok_or(miette!("Variable has no name"))?
                .as_str()
                .to_string();
            let value = parse_expr(
                inner
                    .find_first_tagged("value")
                    .ok_or(miette!("Variable has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Variable value should contain a pair"))?,
            )?;
            Ok(Statement::PointerAssign { ptr, value })
        }
        Some("struct_assign") => {
            let inner = pair.into_inner();
            let var = Expression::Var(
                inner
                    .find_first_tagged("struct")
                    .ok_or(miette!("Variable has no name"))?
                    .as_str()
                    .to_string(),
            );
            let member = inner
                .find_first_tagged("member")
                .ok_or(miette!("Struct access has no member"))?
                .as_str()
                .to_string();
            let value = parse_expr(
                inner
                    .find_first_tagged("value")
                    .ok_or(miette!("Variable has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Variable value should contain a pair"))?,
            )?;
            Ok(Statement::StructAssign { var, member, value })
        }
        Some(t) => Err(miette!("Unknown tag for statement `{t}`")),
        None => Err(miette!("Statement `{}` is untagged", pair.as_str())),
    }
}

pub fn parse_expr(tokens: Pair<'_, Rule>) -> miette::Result<Expression> {
    match tokens.as_node_tag() {
        Some("test") | None => match tokens.as_rule() {
            Rule::equality | Rule::comparison | Rule::term | Rule::factor => {
                parse_binop(tokens.into_inner())
            }
            Rule::unary => {
                let mut pairs = tokens.into_inner();
                match pairs.len() {
                    1 => parse_expr(pairs.next().unwrap()),
                    2 => {
                        let operator: UnaryOperator = pairs.next().unwrap().as_str().parse()?;
                        let value = Box::new(parse_expr(pairs.next().unwrap())?);
                        Ok(Expression::UnaryOperation { value, operator })
                    }
                    _ => unreachable!(),
                }
            }
            Rule::primary => {
                let inner = tokens.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::ident => Ok(Expression::Var(inner.as_str().to_string())),
                    Rule::value => {
                        let val = inner.as_str();
                        if val.starts_with('"') && val.ends_with('"') {
                            let unescaped = snailquote::unescape(val).into_diagnostic()?;
                            Ok(Expression::Literal(Literal::String(unescaped)))
                        } else if val.starts_with('\'') && val.ends_with('\'') {
                            let c = val.trim_matches('\'').chars().next().unwrap();
                            Ok(Expression::Literal(Literal::Int(c as i64)))
                        } else if val.chars().all(|c| c.is_ascii_digit()) {
                            Ok(Expression::Literal(Literal::Int(
                                val.parse().into_diagnostic()?,
                            )))
                        } else if val == "true" {
                            Ok(Expression::Literal(Literal::Bool(true)))
                        } else if val == "false" {
                            Ok(Expression::Literal(Literal::Bool(false)))
                        } else if val.starts_with('{') && val.ends_with('}') {
                            let struct_val = inner
                                .into_inner()
                                .next()
                                .ok_or(miette!("Struct should contain an inner pair"))?
                                .into_inner();

                            let member_names: Vec<String> = struct_val
                                .clone()
                                .find_tagged("member")
                                .map(|p| p.as_str().to_string())
                                .collect();
                            let member_values: miette::Result<Vec<Expression>> = struct_val
                                .clone()
                                .find_tagged("value")
                                .map(|p| parse_expr(p.into_inner().next().unwrap()))
                                .collect();
                            let members = member_names.into_iter().zip(member_values?).collect();

                            Ok(Expression::Literal(Literal::Struct(members)))
                        } else {
                            Err(miette!("unknown literal `{val}`"))
                        }
                    }
                    Rule::expression | Rule::call | Rule::struct_access => parse_expr(inner),
                    _ => unreachable!(),
                }
            }
            Rule::expression => {
                let inner = tokens.into_inner().next().unwrap();
                parse_expr(inner)
            }
            Rule::call => {
                let pairs = tokens.into_inner();
                let task = pairs
                    .find_first_tagged("task")
                    .ok_or(miette!("Task has no name"))?
                    .as_str()
                    .to_string();
                let args: miette::Result<Vec<Expression>> = pairs
                    .clone()
                    .find_tagged("arg")
                    .map(|p| parse_expr(p.into_inner().next().unwrap()))
                    .collect();
                Ok(Expression::Call { task, args: args? })
            }
            Rule::struct_access => {
                let pairs = tokens.into_inner();
                let var = Expression::Var(
                    pairs
                        .find_first_tagged("var")
                        .ok_or(miette!("Struct access needs a variable"))?
                        .as_str()
                        .to_string(),
                );
                let member = pairs
                    .find_first_tagged("member")
                    .ok_or(miette!("Struct access needs a member"))?
                    .as_str()
                    .to_string();

                Ok(Expression::StructAccess {
                    value: Box::new(var),
                    member,
                })
            }
            r => unimplemented!("{r:?}"),
        },
        Some(tag) => Err(miette!("tag `{tag}` is unknown for expression")),
    }
}

fn parse_binop(mut pairs: Pairs<'_, Rule>) -> miette::Result<Expression> {
    match pairs.len() {
        1 => parse_expr(pairs.next().unwrap()),
        n => {
            assert_eq!(n % 2, 1);
            // TODO: Check if this respects precedence
            let left = Box::new(parse_expr(pairs.next().unwrap())?);
            let operator: BinaryOperator = pairs.next().unwrap().as_str().parse()?;
            let right = parse_binop(pairs)?;
            Ok(Expression::BinaryOperation {
                left,
                right: Box::new(right),
                operator,
            })
        }
    }
}
