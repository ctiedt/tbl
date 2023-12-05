use std::path::PathBuf;

use ast::Program;
use clap::Parser as ArgParser;
use codegen::{CodeGen, Config};
use cranelift::prelude::{
    isa::lookup,
    settings::{self, Flags},
    Configurable,
};
use miette::{miette, IntoDiagnostic};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use tracing_subscriber::FmtSubscriber;

use crate::ast::parse_decl;

mod ast;
mod codegen;
mod runtime;

#[derive(Parser)]
#[grammar = "tbl.pest"]
struct TblParser;

#[derive(ArgParser)]
#[command(author, version, about)]
struct Args {
    /// Whether to include debug info
    #[arg(short = 'g', default_value_t = true)]
    is_debug: bool,
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

fn link(file: &str) -> miette::Result<()> {
    let elf_name = file.trim_end_matches(".tbl");
    let obj_name = format!("{elf_name}.o");
    if cfg!(unix) {
        std::process::Command::new("ld.lld")
            .args([
                "-o",
                elf_name,
                "-dynamic-linker",
                //"/lib/ld-linux-x86-64.so.2",
                "/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2",
                "/usr/lib/x86_64-linux-gnu/crt1.o",
                "/usr/lib/x86_64-linux-gnu/crti.o",
                "-L/usr/lib/x86_64-linux-gnu",
                "-lc",
                &obj_name,
                "/usr/lib/x86_64-linux-gnu/crtn.o",
            ])
            .spawn()
            .into_diagnostic()?
            .wait()
            .into_diagnostic()?;
    } else if cfg!(windows) {
        std::process::Command::new("link.exe")
            .args([
                &format!("-out:{elf_name}.exe"),
                "-entry:main",
                &obj_name,
                "ucrt.lib",
                "legacy_stdio_definitions.lib",
            ])
            .spawn()
            .into_diagnostic()?
            .wait()
            .into_diagnostic()?;
    }
    Ok(())
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    tracing::subscriber::set_global_default(FmtSubscriber::builder().pretty().finish())
        .into_diagnostic()?;

    let source_file = std::fs::read_to_string(&args.file).into_diagnostic()?;
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

    let mod_name = args
        .file
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let codegen = CodeGen::new(mod_name.clone(), target)?;
    let config = Config {
        is_debug: args.is_debug,
    };
    codegen.compile(program, config)?;
    link(&mod_name)?;
    Ok(())
}
