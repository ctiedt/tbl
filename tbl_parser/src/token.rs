use logos::Logos;
use std::fmt::Display;

#[derive(Logos, Clone, Copy, PartialEq, Eq, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum Token<'a> {
    #[token("task")]
    Task,

    #[token("global")]
    Global,

    #[token("struct")]
    Struct,

    #[token("enum")]
    Enum,

    #[token("extern")]
    Extern,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("loop")]
    Loop,

    #[token("match")]
    Match,

    #[token("break")]
    Break,

    #[token("schedule")]
    Schedule,

    #[token("every")]
    Every,

    #[token("exit")]
    Exit,

    #[token("return")]
    Return,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("as")]
    As,

    #[token("use")]
    Use,

    #[token("ms")]
    Ms,

    #[token("let")]
    Let,

    #[token("uninit")]
    Uninit,

    #[token("on")]
    On,

    #[token("do")]
    Do,

    #[regex(r"([iu])(8|16|32|64)")]
    IntType(&'a str),

    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident(&'a str),

    #[token("...")]
    Varargs,

    #[token("->")]
    Arrow,

    #[token("::")]
    DoubleColon,

    #[token("==")]
    DoubleEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("<=")]
    LessEqual,

    #[token("!=")]
    Unequal,

    #[token("@")]
    At,

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

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("&")]
    And,

    #[token("|")]
    Pipe,

    #[token("!")]
    Exclamation,

    #[token("#")]
    Hash,

    #[regex(r"\d+", |lex| lex.slice().parse::<u64>().unwrap())]
    Number(u64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String(&'a str),

    #[regex(r"'([^\\']|\\[^'])'")]
    Char(&'a str),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Task => "task".to_string(),
                Token::Global => "global".to_string(),
                Token::Struct => "struct".to_string(),
                Token::Enum => "enum".to_string(),
                Token::Extern => "extern".to_string(),
                Token::If => "if".to_string(),
                Token::Else => "else".to_string(),
                Token::Loop => "loop".to_string(),
                Token::Match => "match".to_string(),
                Token::Break => "break".to_string(),
                Token::Schedule => "schedule".to_string(),
                Token::Every => "every".to_string(),
                Token::Exit => "exit".to_string(),
                Token::Return => "return".to_string(),
                Token::True => "true".to_string(),
                Token::False => "false".to_string(),
                Token::As => "as".to_string(),
                Token::Use => "use".to_string(),
                Token::Ms => "ms".to_string(),
                Token::Let => "let".to_string(),
                Token::Uninit => "uninit".to_string(),
                Token::On => "on".to_string(),
                Token::Do => "do".to_string(),
                Token::IntType(t) => t.to_string(),
                Token::Ident(id) => id.to_string(),
                Token::Varargs => "...".to_string(),
                Token::Arrow => "->".to_string(),
                Token::DoubleColon => "::".to_string(),
                Token::DoubleEqual => "==".to_string(),
                Token::Unequal => "!=".to_string(),
                Token::GreaterEqual => ">=".to_string(),
                Token::LessEqual => "<=".to_string(),
                Token::At => "@".to_string(),
                Token::ParenOpen => "(".to_string(),
                Token::ParenClose => ")".to_string(),
                Token::BracketOpen => "[".to_string(),
                Token::BracketClose => "]".to_string(),
                Token::CurlyOpen => "{".to_string(),
                Token::CurlyClose => "}".to_string(),
                Token::LessThan => "<".to_string(),
                Token::GreaterThan => ">".to_string(),
                Token::Period => ".".to_string(),
                Token::Comma => ",".to_string(),
                Token::Semicolon => ";".to_string(),
                Token::Equals => "=".to_string(),
                Token::Colon => ":".to_string(),
                Token::Minus => "-".to_string(),
                Token::Plus => "+".to_string(),
                Token::Star => "*".to_string(),
                Token::Slash => "/".to_string(),
                Token::And => "&".to_string(),
                Token::Pipe => "|".to_string(),
                Token::Hash => "#".to_string(),
                Token::Exclamation => "!".to_string(),
                Token::Number(n) => format!("{n}"),
                Token::String(s) => s.to_string(),
                Token::Char(c) => c.to_string(),
            }
        )
    }
}
