use miette::{miette, IntoDiagnostic};
use pest::iterators::{Pair, Pairs};

use crate::{parse::types::BinaryOperator, Rule};

use super::types::{
    Declaration, Expression, ExternTaskParams, Literal, Statement, Type, UnaryOperator,
};

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
        Rule::task_ptr => {
            let inner = pair.into_inner();
            let types: miette::Result<Vec<Type>> =
                inner.clone().find_tagged("type").map(parse_type).collect();
            let returns = inner
                .find_first_tagged("ptr_returns")
                .map(parse_type)
                .transpose();
            Ok(Type::TaskPtr {
                params: types?,
                returns: returns?.map(Box::new),
            })
        }
        Rule::any => Ok(Type::Any),
        u => unreachable!("{u:?}"),
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
                .ok_or(miette!("Task has no name: {location:?}"))?
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
        Rule::global => {
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Global has no name"))?
                .as_str()
                .to_string();
            let type_ = parse_type(
                pairs
                    .find_first_tagged("type")
                    .ok_or(miette!("Global has no type"))?,
            )?;
            let value = parse_expr(
                pairs
                    .find_first_tagged("value")
                    .ok_or(miette!("Global has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Global value should contain one inner pair"))?,
            )?;
            Ok(Declaration::Global { name, type_, value })
        }
        Rule::directive => {
            let pairs = pair.into_inner();
            let name = pairs
                .find_first_tagged("name")
                .ok_or(miette!("Directive has no name"))?
                .as_str()
                .to_string();

            let args: miette::Result<Vec<Literal>> =
                pairs.find_tagged("arg").map(parse_literal).collect();

            Ok(Declaration::Directive { name, args: args? })
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
            let args: miette::Result<Vec<Expression>> = pairs
                .clone()
                .find_tagged("arg")
                .map(|a| parse_expr(a.into_inner().next().unwrap()))
                .collect();
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
        Some("assign") => {
            let inner = pair.into_inner();
            let location = parse_expr(
                inner
                    .find_first_tagged("location")
                    .ok_or(miette!("Assignment has no location"))?
                    .into_inner()
                    .next()
                    .unwrap(),
            )?;

            let value = parse_expr(
                inner
                    .find_first_tagged("value")
                    .ok_or(miette!("Assignment has no value"))?
                    .into_inner()
                    .next()
                    .ok_or(miette!("Variable value should contain a pair"))?,
            )?;
            Ok(Statement::Assign { location, value })
        }
        Some(t) => Err(miette!(
            "Unknown tag for statement `{t}` (at {:?})",
            pair.as_span()
        )),
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
            Rule::postfix => {
                let mut pairs = tokens.into_inner();
                match pairs.len() {
                    1 => parse_expr(pairs.next().unwrap()),
                    2 => {
                        let value = Box::new(parse_expr(pairs.next().unwrap())?);
                        let op = pairs.next().unwrap();
                        match op.as_node_tag() {
                            Some("struct_access") => {
                                let member: String = op
                                    .into_inner()
                                    .clone()
                                    .find_first_tagged("member")
                                    .ok_or(miette!("Struct access needs a member"))?
                                    .as_str()
                                    .to_string();

                                Ok(Expression::StructAccess { value, member })
                            }
                            Some("cast") => {
                                let to = parse_type(
                                    op.into_inner()
                                        .clone()
                                        .find_first_tagged("to")
                                        .ok_or(miette!("Cast needs a target type"))?,
                                )?;

                                Ok(Expression::Cast { value, to })
                            }
                            Some("call") => {
                                let args: miette::Result<Vec<Expression>> = op
                                    .into_inner()
                                    .clone()
                                    .find_tagged("arg")
                                    .map(|p| parse_expr(p.into_inner().next().unwrap()))
                                    .collect();

                                Ok(Expression::Call {
                                    task: value,
                                    args: args?,
                                })
                            }
                            Some("index") => {
                                let at =
                                    Box::new(parse_expr(op.into_inner().next().ok_or(
                                        miette!("Index operation should contain one pair"),
                                    )?)?);

                                Ok(Expression::Index { value, at })
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Rule::primary => {
                let inner = tokens.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::ident => Ok(Expression::Var(inner.as_str().to_string())),
                    Rule::value => Ok(Expression::Literal(parse_literal(inner)?)),
                    Rule::sizeof => {
                        let inner = inner.into_inner().next().unwrap();
                        Ok(Expression::SizeOf {
                            value: parse_type(inner)?,
                        })
                    }
                    Rule::expression | Rule::call | Rule::struct_access => parse_expr(inner),
                    _ => unreachable!(),
                }
            }
            Rule::expression => {
                let inner = tokens.into_inner().next().unwrap();
                parse_expr(inner)
            }
            r => unimplemented!("{r:?}"),
        },
        Some(tag) => Err(miette!("tag `{tag}` is unknown for expression")),
    }
}

fn parse_literal(inner: Pair<Rule>) -> miette::Result<Literal> {
    let val = inner.as_str();
    if val.starts_with('"') && val.ends_with('"') {
        let unescaped = snailquote::unescape(val).into_diagnostic()?;
        Ok(Literal::String(unescaped))
    } else if val.starts_with('\'') && val.ends_with('\'') {
        let c = val.trim_matches('\'').chars().next().unwrap();
        Ok(Literal::Int(c as i64))
    } else if val.chars().all(|c| c.is_ascii_digit()) {
        Ok(Literal::Int(val.parse().into_diagnostic()?))
    } else if val == "true" {
        Ok(Literal::Bool(true))
    } else if val == "false" {
        Ok(Literal::Bool(false))
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

        Ok(Literal::Struct(members))
    } else if val.starts_with('[') && val.ends_with(']') {
        let array_val = inner
            .into_inner()
            .next()
            .ok_or(miette!("Array should contain an inner pair"))?
            .into_inner();

        let values: miette::Result<Vec<Expression>> = array_val.map(parse_expr).collect();

        Ok(Literal::Array(values?))
    } else {
        Err(miette!("unknown literal `{val}`"))
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
