use std::str::FromStr;

use miette::{miette, Diagnostic, IntoDiagnostic};
use pest::iterators::Pair;
use thiserror::Error;

use crate::Rule;

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("unknown operator")]
    UnknownOperator(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    ExternTask {
        name: String,
        params: Vec<(String, String)>,
        returns: Option<String>,
    },
    Task {
        name: String,
        params: Vec<(String, String)>,
        returns: Option<String>,
        body: Vec<Statement>,
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
        type_: String,
        value: Expression,
    },
    VarAssign {
        name: String,
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
}

pub fn parse_decl(tokens: Pair<'_, Rule>) -> miette::Result<Declaration> {
    let pair = tokens
        .into_inner()
        .next()
        .ok_or(miette!("Declaration should contain one inner pair"))?;
    match pair.as_rule() {
        Rule::task => {
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Task has no name"))?
                .as_str()
                .to_string();
            let returns = pairs
                .find_first_tagged("returns")
                .map(|p| p.as_str().to_string());
            let param_names = pairs
                .clone()
                .find_tagged("param")
                .map(|p| p.as_str().to_string());
            let param_types = pairs
                .clone()
                .find_tagged("type")
                .map(|p| p.as_str().to_string());
            let params = param_names.zip(param_types).collect();
            let body: miette::Result<Vec<Statement>> =
                pairs.find_tagged("body").map(parse_stmt).collect();
            let body = body?;

            Ok(Declaration::Task {
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
                .map(|p| p.as_str().to_string());
            let param_names = pairs
                .clone()
                .find_tagged("param")
                .map(|p| p.as_str().to_string());
            let param_types = pairs
                .clone()
                .find_tagged("type")
                .map(|p| p.as_str().to_string());
            let params = param_names.zip(param_types).collect();

            Ok(Declaration::ExternTask {
                name,
                params,
                returns,
            })
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
            let type_ = inner
                .find_first_tagged("type")
                .ok_or(miette!("Variable has no type"))?
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
        Some(t) => Err(miette!("Unknown tag for statement `{t}`")),
        None => Err(miette!("Statement `{}` is untagged", pair.as_str())),
    }
}

pub fn parse_expr(tokens: Pair<'_, Rule>) -> miette::Result<Expression> {
    match tokens.as_node_tag() {
        Some("test") | None => match tokens.as_rule() {
            Rule::equality | Rule::comparison | Rule::term | Rule::factor => parse_binop(tokens),
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
                            Ok(Expression::Literal(Literal::String(
                                val.trim_matches('"').to_string(),
                            )))
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
                        } else {
                            Err(miette!("unknown literal"))
                        }
                    }
                    Rule::expression => parse_expr(inner),
                    Rule::call => parse_expr(inner),
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
            r => unimplemented!("{r:?}"),
        },
        Some(tag) => Err(miette!("tag `{tag}` is unknown for expression")),
    }
}

fn parse_binop(tokens: Pair<'_, Rule>) -> miette::Result<Expression> {
    let mut pairs = tokens.into_inner();
    match pairs.len() {
        1 => parse_expr(pairs.next().unwrap()),
        3 => {
            let left = Box::new(parse_expr(pairs.next().unwrap())?);
            let operator: BinaryOperator = pairs.next().unwrap().as_str().parse()?;
            let right = Box::new(parse_expr(pairs.next().unwrap())?);
            Ok(Expression::BinaryOperation {
                left,
                right,
                operator,
            })
        }
        _ => unreachable!(),
    }
}
