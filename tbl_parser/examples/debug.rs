use logos::Logos;
use tbl_parser::{resolve_directives, Parser, Source, Span, Token};

fn main() {
    let mut args = std::env::args();
    let path = args.nth(1).unwrap();

    let source = std::fs::read_to_string(&path).unwrap();

    let lexer = Token::lexer(&source);
    let tokens: Result<Vec<(Token<'_>, Span)>, ()> = lexer
        .spanned()
        .map(|(t, s)| match t {
            Ok(t) => Ok((t, s)),
            Err(e) => Err(e),
        })
        .collect();

    let mut parser = Parser::new(
        tokens.unwrap(),
        Source {
            name: &path,
            contents: &source,
        },
    );
    let (program, _) = parser.parse();
    let program = resolve_directives(program);
    dbg!(program);
}
