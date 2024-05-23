use std::{panic::PanicInfo, path::PathBuf, sync::Arc};

use ariadne::Label;
use clap::Parser as ArgParser;
use cranelift::{
    codegen::isa::{lookup_by_name, TargetIsa},
    prelude::{
        isa::lookup,
        settings::{self, Flags},
        Configurable,
    },
};
use miette::IntoDiagnostic;
use tbl_codegen::{error::CodegenResult, CodeGen, Config};

use tbl_parser::module::{parse_module_hierarchy, TblModule};
use tracing::error;
use tracing_subscriber::FmtSubscriber;

#[derive(ArgParser)]
#[command(author, version, about)]
struct Args {
    #[arg(short = 'g', default_value_t = false)]
    /// Whether to include debug info
    is_debug: bool,
    #[arg(short = 'c', default_value_t = false)]
    /// Whether to link the output with libc
    compile_only: bool,
    #[arg(long)]
    /// The target to compile for
    target: Option<String>,
    /// Object files to link with
    #[arg(short)]
    linked_objects: Vec<String>,
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

fn link<S: AsRef<str>>(
    file: &str,
    target: TargetPlatform,
    objects: &[S],
    linked_objects: &[S],
) -> miette::Result<()> {
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
            let mut args = vec![
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
                "-lreadline",
                //&obj_name,
                //"-l:crtn.o",
            ];
            args.extend(objects.iter().map(|f| f.as_ref()));
            args.extend(linked_objects.iter().map(|f| f.as_ref()));
            args.extend(&["-l:crtn.o"]);
            std::process::Command::new("ld")
                .args(args)
                .spawn()
                .into_diagnostic()?
                .wait()
                .into_diagnostic()?;
        }
    }
    Ok(())
}

fn report_compiler_panic(info: &PanicInfo) {
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

fn compile_module(
    module: &TblModule,
    target: Arc<dyn TargetIsa>,
    config: Config,
) -> CodegenResult<Vec<String>> {
    let mut modules = vec![];
    for dep in &module.dependencies {
        let mut cfg = config.clone();
        cfg.compile_only = true;
        let output = compile_module(dep, target.clone(), cfg)?;
        modules.extend(output);
    }
    let codegen = CodeGen::new(module.name.clone(), target, config)?;
    modules.push(codegen.compile(module)?);
    Ok(modules)
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    if std::env::var("RUST_BACKTRACE").is_err() {
        std::panic::set_hook(Box::new(report_compiler_panic));
    }

    tracing::subscriber::set_global_default(FmtSubscriber::builder().pretty().finish())
        .into_diagnostic()?;

    let (module, errors) = parse_module_hierarchy(&args.file, &[".", "lib"]).into_diagnostic()?;
    if !errors.is_empty() {
        for e in errors {
            let mod_name = e.mod_path.display().to_string();
            let src = std::fs::read_to_string(e.mod_path).into_diagnostic()?;
            match e.kind {
                tbl_parser::module::ModuleErrorKind::PathDoesNotExist => {
                    // ariadne::Report::build(ariadne::ReportKind::Error, mod_name.clone(), 0)
                    //     .with_message(e.kind.to_string())
                    //     .finish()
                    //     .eprint(mod)
                    //     .into_diagnostic()?;
                    unreachable!()
                }
                tbl_parser::module::ModuleErrorKind::ParseError(ref err) => {
                    ariadne::Report::build(ariadne::ReportKind::Error, &mod_name, err.span.start)
                        .with_message(err.kind.to_string())
                        .with_label(Label::new((&mod_name, err.span.clone())))
                        .finish()
                        .eprint((&mod_name, ariadne::Source::from(&src)))
                        .into_diagnostic()?;
                }
            }
        }

        miette::bail!("Errors, aborting compilation");
    }

    let mut shared_builder = settings::builder();
    shared_builder.enable("is_pic").into_diagnostic()?;
    let shared_flags = Flags::new(shared_builder);

    let mod_name = args
        .file
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let config = Config {
        is_debug: args.is_debug,
        compile_only: args.compile_only,
        filename: args.file.clone(),
    };

    let target = match &args.target {
        Some(t) => lookup_by_name(t),
        None => lookup(target_lexicon::DefaultToHost::default().0),
    }
    .into_diagnostic()?
    .finish(shared_flags)
    .into_diagnostic()?;

    match compile_module(&module, target, config) {
        Ok(outputs) => {
            if !args.compile_only {
                let link_target = host_target();
                link(
                    &mod_name,
                    link_target,
                    &outputs,
                    args.linked_objects.as_slice(),
                )?;
            }
        }
        Err(err) => {
            let src = std::fs::read_to_string(&args.file).into_diagnostic()?;
            ariadne::Report::build(ariadne::ReportKind::Error, &mod_name, err.span.start)
                .with_message(err.kind.to_string())
                .with_label(Label::new((&mod_name, err.span.clone())))
                .finish()
                .eprint((&mod_name, ariadne::Source::from(&src)))
                .into_diagnostic()?;
        }
    }
    Ok(())
}
