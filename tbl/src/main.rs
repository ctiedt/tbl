mod ast;
mod parse;

use ariadne::{Label, Report, Source};

use parse::parse;

fn main() -> color_eyre::Result<()> {
    let input_path = std::env::args().nth(1).unwrap();

    let input = std::fs::read_to_string(&input_path)?;
    match parse(&input) {
        Ok(ast) => {
            dbg!(ast);
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
