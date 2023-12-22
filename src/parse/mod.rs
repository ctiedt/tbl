use miette::miette;
use miette::Diagnostic;
use pest::iterators::Pair;
use thiserror::Error;

use crate::Rule;

use self::{parse::parse_decl, types::Program};

mod parse;
pub mod types;

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

pub fn parse_program(tokens: Pair<'_, Rule>) -> miette::Result<Program> {
    let mut declarations = vec![];
    for pair in tokens.into_inner() {
        if matches!(pair.as_rule(), Rule::decl) {
            let decl = parse_decl(pair)?;
            declarations.push(decl);
        } else if matches!(pair.as_rule(), Rule::EOI) {
            break;
        } else {
            return Err(miette!(
                "Unexpected element of type `{:?}` at top level",
                pair.as_rule()
            ));
        }
    }
    Ok(Program { declarations })
}
