use logos::Logos;

#[derive(Logos, Clone, Copy, PartialEq, Eq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'a> {
    #[token("task")]
    #[token("global")]
    #[token("struct")]
    #[token("extern")]
    #[token("if")]
    #[token("else")]
    #[token("loop")]
    #[token("break")]
    #[token("schedule")]
    #[token("exit")]
    Keyword(&'a str),

    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident(&'a str),

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token("{")]
    CurlyOpen,

    #[token("}")]
    CurlyClose,

    #[token("<")]
    LessThan,

    #[token(">")]
    GreaterThan,

    #[token(".")]
    Period,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("=")]
    Equals,

    #[token(":")]
    Colon,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token("->")]
    Arrow,

    #[regex(r"\d+", |lex| lex.slice().parse::<u64>().unwrap())]
    Number(u64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String(&'a str),
}
