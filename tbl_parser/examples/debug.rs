use logos::Logos;
use tbl_parser::{Parser, Source, Span, Token};

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

    dbg!(&tokens);

    let mut parser = Parser::new(tokens.unwrap());
    let (program, errors) = parser.parse();

    for e in errors {
        println!("{e:?}");
    }

    dbg!(program);
}
