use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use crate::ast::{
    Argument, AstNode, BinaryOperator, Body, Expression, Schedule, Task, TblType, TblValue,
    UnaryOperator,
};

#[derive(Parser)]
#[grammar = "../tbl.pest"]
struct TblParser;

pub fn parse(source: &str) -> Result<Vec<AstNode>, Box<pest::error::Error<Rule>>> {
    let mut ast = vec![];

    let pairs = TblParser::parse(Rule::program, source)?;
    for pair in pairs {
        match pair.as_rule() {
            Rule::task => {
                let mut task = Task::default();
                let mut body_idx = 0;
                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        Rule::ident => {
                            if task.name.is_empty() {
                                task.name = pair.as_str().into();
                            } else {
                                panic!("Unexpected ident in task definition");
                            }
                        }
                        Rule::arg => {
                            let mut arg_pair = pair.into_inner();
                            let arg = Argument {
                                name: arg_pair.next().unwrap().as_str().into(),
                                type_: TblType::try_from(arg_pair.next().unwrap().as_str())
                                    .unwrap(),
                            };
                            task.arguments.push(arg);
                        }
                        Rule::struct_def => {
                            let mut state = vec![];
                            for field in pair.into_inner() {
                                let mut arg_pair = field.into_inner();
                                let arg = Argument {
                                    name: arg_pair.next().unwrap().as_str().into(),
                                    type_: TblType::try_from(arg_pair.next().unwrap().as_str())
                                        .unwrap(),
                                };
                                state.push(arg);
                            }
                            task.state = state;
                        }
                        Rule::body => {
                            let body = parse_body(pair.into_inner());
                            match body_idx {
                                0 => task.enter = body,
                                1 => task.repeat = body,
                                2 => task.exit = body,
                                _ => unreachable!(),
                            }
                            body_idx += 1;
                        }
                        _ => {}
                    }
                }
                ast.push(AstNode::Task(task));
            }
            Rule::schedule => {
                let mut schedule = Schedule::default();
                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        Rule::ident => {
                            schedule.name = pair.as_str().into();
                        }
                        Rule::body => {
                            schedule.body = parse_body(pair.into_inner());
                        }
                        _ => {}
                    }
                }
                ast.push(AstNode::Schedule(schedule));
            }
            Rule::EOI => {
                break;
            }
            _ => unreachable!("Did not expect rule {:?}", pair.as_rule()),
        }
    }
    Ok(ast)
}

fn parse_body(pairs: Pairs<'_, Rule>) -> Body {
    let mut body = vec![];
    for pair in pairs {
        assert!(matches!(pair.as_rule(), Rule::statement));
        if pair.as_str() == "exit;" {
            body.push(Expression::Exit);
            continue;
        }
        for pair in pair.into_inner() {
            let stmt = match pair.as_rule() {
                Rule::var_assign => {
                    let mut inner = pair.into_inner();
                    let var = inner.next().unwrap().as_str().into();
                    let value = parse_expr(inner.next().unwrap());

                    Expression::VarAssign { var, value }
                }
                Rule::expression => *parse_expr(pair),
                Rule::conditional => {
                    let mut inner = pair.into_inner();
                    let condition = inner.next().unwrap();
                    let if_path = inner.next().unwrap();
                    let else_path = inner.next().unwrap();
                    Expression::Conditional {
                        condition: parse_expr(condition),
                        then: parse_body(if_path.into_inner()),
                        otherwise: parse_body(else_path.into_inner()),
                    }
                }

                _ => todo!("{:?}", pair.as_rule()),
            };
            body.push(stmt);
        }
    }
    body
}

fn parse_expr(pair: Pair<'_, Rule>) -> Box<Expression> {
    let inner_pair = pair.into_inner().next().unwrap();
    let pair = inner_pair.clone();
    let pairs: Vec<_> = inner_pair.into_inner().collect();

    let expr = match pair.as_rule() {
        Rule::call => {
            let function = pairs[0].as_str().into();
            let arguments: Vec<_> = pairs[1..]
                .iter()
                .map(|arg| *parse_expr(arg.clone()))
                .collect();
            Expression::Call {
                function,
                arguments,
            }
        }
        Rule::builtin_operation => {
            if pairs.len() == 1 {
                *parse_expr(pairs[0].clone())
            } else if pairs.len() == 2 {
                let operator = UnaryOperator::try_from(pairs[0].as_str()).unwrap();
                let value = parse_expr(pairs[1].clone());
                Expression::UnaryOperation { operator, value }
            } else if pairs.len() == 3 {
                let left = parse_expr(pairs[0].clone());
                let operator = BinaryOperator::try_from(pairs[1].as_str()).unwrap();
                let right = parse_expr(pairs[2].clone());
                Expression::BinaryOperation {
                    operator,
                    left,
                    right,
                }
            } else {
                panic!()
            }
        }
        Rule::var => {
            if pairs.len() == 1 {
                let var = pairs[0].as_str().into();
                Expression::Value(TblValue::Variable(var))
            } else if pairs.len() == 2 {
                let env = pairs[0].as_str();
                let var = pairs[1].as_str();
                Expression::Value(TblValue::Variable(format!("{env}.{var}")))
            } else {
                panic!()
            }
        }
        Rule::num => {
            let n = pair.as_str();
            Expression::Value(TblValue::Int(n.parse().unwrap()))
        }
        Rule::string => {
            let s = pair.as_str();
            Expression::Value(TblValue::String(s.into()))
        }
        _ => unreachable!("{:?}", pair.as_rule()),
    };

    Box::new(expr)
}
