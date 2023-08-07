mod ast;
mod codegen;
mod parse;

use ariadne::{Label, Report, Source};

use codegen::CodeGen;
use inkwell::context::Context;
use parse::parse;

fn main() -> color_eyre::Result<()> {
    let input_path = std::env::args().nth(1).unwrap();

    let input = std::fs::read_to_string(&input_path)?;
    match parse(&input) {
        Ok(ast) => {
            let context = Context::create();
            let module = context.create_module(&input_path);
            let builder = context.create_builder();
            let mut codegen = CodeGen::new(&context, module, builder);
            codegen.compile(ast);
        }
        Err(err) => match err.location {
            pest::error::InputLocation::Pos(pos) => {
                Report::build(ariadne::ReportKind::Error, &input_path, pos)
                    .with_label(
                        Label::new((&input_path, pos..pos + 1))
                            .with_message(err.variant.to_string()),
                    )
                    .finish()
                    .eprint((&input_path, Source::from(input)))?;
            }
            pest::error::InputLocation::Span((start, end)) => {
                Report::build(ariadne::ReportKind::Error, &input_path, start)
                    .with_label(
                        Label::new((&input_path, start..end)).with_message(err.variant.to_string()),
                    )
                    .finish()
                    .eprint((&input_path, Source::from(input)))?;
            }
        },
    }

    Ok(())
}
