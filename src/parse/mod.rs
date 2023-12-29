use crate::Rule;
use crate::TblParser;
use miette::miette;
use miette::Diagnostic;
use miette::IntoDiagnostic;

use pest::iterators::Pair;
use pest::Parser;
use pyo3::types::PyModule;
use pyo3::types::PyTuple;
use pyo3::Python;
use pyo3::ToPyObject;
use thiserror::Error;

use self::types::Declaration;
use self::types::Literal;
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

impl ToPyObject for Literal {
    fn to_object(&self, py: Python<'_>) -> pyo3::prelude::PyObject {
        match self {
            Literal::Int(i) => i.to_object(py),
            Literal::String(s) => s.to_object(py),
            Literal::Bool(b) => b.to_object(py),
            Literal::Struct(_) => unimplemented!(),
            Literal::Array(_) => unimplemented!(),
        }
    }
}

pub fn parse_program(tokens: Pair<'_, Rule>, preprocessor: &str) -> miette::Result<Program> {
    let mut declarations = vec![];
    for pair in tokens.into_inner() {
        if matches!(pair.as_rule(), Rule::decl) {
            let decl = parse_decl(pair)?;
            if let Declaration::Directive { name, args } = decl {
                let replacement = Python::with_gil(|py| {
                    let code = std::fs::read_to_string(preprocessor).unwrap();
                    let preprocess =
                        PyModule::from_code(py, &code, preprocessor, "preprocess").unwrap();
                    let py_args = PyTuple::new(py, args);
                    preprocess
                        .getattr(name.as_str())
                        .unwrap()
                        .call(py_args, None)
                        .unwrap()
                        .to_string()
                });

                let parsed = TblParser::parse(Rule::program, &replacement)
                    .into_diagnostic()?
                    .next()
                    .unwrap();

                let mut decls = parse_program(parsed, preprocessor)?;
                declarations.append(&mut decls.declarations);
            } else {
                declarations.push(decl);
            }
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
