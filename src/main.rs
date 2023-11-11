use std::path::PathBuf;

use ast::Program;
use clap::Parser as ArgParser;
use codegen::CodeGen;
use cranelift::prelude::{
    isa::lookup,
    settings::{self, Flags},
    Configurable,
};
use miette::{miette, IntoDiagnostic};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use tracing::info;
use tracing_subscriber::FmtSubscriber;

use crate::ast::parse_decl;

mod ast;
mod codegen;

#[derive(Parser)]
#[grammar = "tbl.pest"]
struct TblParser;

#[derive(ArgParser)]
#[command(author, version, about)]
struct Args {
    /// File to compile
    file: PathBuf,
}

fn parse_program(tokens: Pair<'_, Rule>) -> miette::Result<Program> {
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

fn main() -> miette::Result<()> {
    let args = Args::parse();

    tracing::subscriber::set_global_default(FmtSubscriber::builder().pretty().finish())
        .into_diagnostic()?;

    let source_file = std::fs::read_to_string(args.file).into_diagnostic()?;
    let parsed = TblParser::parse(Rule::program, &source_file)
        .into_diagnostic()?
        .next()
        .unwrap();
    let program = parse_program(parsed)?;

    let mut shared_builder = settings::builder();
    shared_builder.enable("is_pic").into_diagnostic()?;
    let shared_flags = Flags::new(shared_builder);
    let target = lookup(target_lexicon::DefaultToHost::default().0)
        .into_diagnostic()?
        .finish(shared_flags)
        .into_diagnostic()?;

    let codegen = CodeGen::new(target)?;
    codegen.compile(program)?;
    Ok(())
}
