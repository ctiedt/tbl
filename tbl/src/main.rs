mod ast;

use ast::{Argument, AstNode, Body, Schedule, Task, TblType};
use pest::{iterators::Pairs, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../tbl.pest"]
struct TblParser;

fn parse(source: &str) -> Vec<AstNode> {
    let mut ast = vec![];

    let pairs = TblParser::parse(Rule::program, source).unwrap();
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
    ast
}

fn parse_body(pairs: Pairs<'_, Rule>) -> Body {
    let mut body = vec![];
    for pair in pairs {
        match pair.as_rule() {
            _ => panic!("Rule {:?} is not allowed in an expression", pair.as_rule()),
        }
    }
    body
}

fn main() -> color_eyre::Result<()> {
    let input = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../examples/counter.tbl"
    ));
    let parsed = parse(input);
    dbg!(parsed);
    Ok(())
}
