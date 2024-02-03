use std::{backtrace::Backtrace, panic::PanicInfo, path::PathBuf};

use clap::Parser as ArgParser;
use codegen::{CodeGen, Config};
use cranelift::prelude::{
    isa::lookup,
    settings::{self, Flags},
    Configurable,
};
use miette::IntoDiagnostic;
use pest::Parser;
use pest_derive::Parser;
use tracing::error;
use tracing_subscriber::FmtSubscriber;

use parse::parse_program;

mod codegen;
mod parse;

#[derive(Parser)]
#[grammar = "tbl.pest"]
struct TblParser;

#[derive(ArgParser)]
#[command(author, version, about)]
struct Args {
    /// Whether to include debug info
    #[arg(short = 'g', default_value_t = false)]
    is_debug: bool,
    #[arg(short = 'c', default_value_t = false)]
    /// Whether to link the output with libc
    compile_only: bool,
    #[arg(short, long, default_value = "preprocess.py")]
    /// The preprocessor implementation to use
    preprocessor: String,
    /// File to compile
    file: PathBuf,
}

#[derive(Clone, Copy)]
pub enum TargetPlatform {
    Windows,
    Linux,
}

fn host_target() -> TargetPlatform {
    if cfg!(windows) {
        TargetPlatform::Windows
    } else if cfg!(unix) {
        TargetPlatform::Linux
    } else {
        unimplemented!("Unsupported platform")
    }
}

fn link(file: &str, target: TargetPlatform) -> miette::Result<()> {
    let elf_name = file.trim_end_matches(".tbl");
    let obj_name = format!("{elf_name}.o");
    match target {
        TargetPlatform::Windows => {
            std::process::Command::new("link.exe")
                .args([
                    &format!("-out:{elf_name}.exe"),
                    //"-entry:_tbl_start",
                    &obj_name,
                    "legacy_stdio_definitions.lib",
                    "libcmt.lib",
                    "libucrt.lib",
                    "libvcruntime.lib",
                ])
                .spawn()
                .into_diagnostic()?
                .wait()
                .into_diagnostic()?;
        }
        TargetPlatform::Linux => {
            std::process::Command::new("ld.lld")
                .args([
                    "-o",
                    elf_name,
                    "-L/usr/lib",
                    "-L/lib",
                    "-L/lib/x86_64-linux-gnu",
                    //"-e",
                    //"_tbl_start",
                    "-dynamic-linker",
                    "/lib64/ld-linux-x86-64.so.2",
                    "-l:crt1.o",
                    "-l:crti.o",
                    "-lc",
                    &obj_name,
                    "-l:crtn.o",
                ])
                .spawn()
                .into_diagnostic()?
                .wait()
                .into_diagnostic()?;
        }
    }
    Ok(())
}

fn report_compiler_panic(info: &PanicInfo) {
    if std::env::var("RUST_BACKTRACE").is_ok() {
        let bt = Backtrace::capture();
        //let trace = bt.frames().iter().map(|f| format!("{f:?}", f.)).join("\n");
        error!("{bt:?}");
    }

    let commit_hash = match std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
    {
        Ok(output) => String::from_utf8_lossy(&output.stdout).to_string(),
        Err(_) => String::from("Git info not available"),
    };
    let issue_info = format!(
        r#"## Description and Reproduction

Describe how the issue occurred.
    
## System Info

- Panic Message: `{info}`
- Host Triple: `{}`
- Current Commit Hash: `{}`"#,
        target_lexicon::HOST,
        commit_hash.trim()
    );
    error!(
        r#"It looks like you ran into a compiler panic.
The message is: `{info}`
If there is no issue referencing this panic, please open one:
https://github.com/ctiedt/tbl/issues/new?title=Compiler+Panic&body={}"#,
        urlencoding::encode(&issue_info)
    );
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    std::panic::set_hook(Box::new(report_compiler_panic));

    tracing::subscriber::set_global_default(FmtSubscriber::builder().pretty().finish())
        .into_diagnostic()?;

    let source_file = std::fs::read_to_string(&args.file).into_diagnostic()?;
    let parsed = TblParser::parse(Rule::program, &source_file)
        .into_diagnostic()?
        .next()
        .unwrap();

    let program = parse_program(parsed, &args.preprocessor)?;

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
    let config = Config {
        is_debug: args.is_debug,
        compile_only: args.compile_only,
        filename: args.file,
        link_target: host_target(),
    };
    let codegen = CodeGen::new(mod_name.clone(), target, config)?;
    codegen.compile(program)?;
    if !args.compile_only {
        let link_target = host_target();
        link(&mod_name, link_target)?;
    }
    Ok(())
}
