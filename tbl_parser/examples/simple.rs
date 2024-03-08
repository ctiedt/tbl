use logos::Logos;
use tbl_parser::{Parser, Source, Span, Token};

fn main() {
    let source = include_str!("../../examples/if.tbl");

    let lexer = Token::lexer(source);
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
            name: "int.tbl",
            contents: source,
        },
    );
    let program = parser.parse().unwrap();
    //println!("{:?}", program);
    dbg!(program);
    //let et = parser.parse_extern_task().unwrap().unwrap();
    //dbg!(et);
    //let t = parser.parse_task().unwrap().unwrap();
    //dbg!(t);
}