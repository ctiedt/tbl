use crate::{Span, Token};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub(crate) fn new(span: Span, kind: ParseErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("malformed character")]
    BadCharacter,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("unknown keyword")]
    UnknownKeyword,
    #[error("no more tokens")]
    NoMoreTokens,
    #[error("malformed task parameters")]
    BadParams,
    #[error("malformed type name")]
    BadType,
    #[error("expected token `{0}`")]
    ExpectedToken(Token<'static>),
    #[error("expected `{0}`")]
    ExpectedPattern(String),
    #[error("expected expression")]
    ExpectedExpression,
    #[error("bad integer value")]
    ParseIntError(#[from] ParseIntError),
}

pub type ParseResult<T> = Result<Option<T>, ParseError>;

pub trait ParseResultExt {
    fn error(self, kind: ParseErrorKind) -> Self;

    fn expected_expr(self) -> Self;

    fn expect_token(self, t: Token<'static>) -> Self;
}

impl<T> ParseResultExt for ParseResult<T> {
    fn error(self, kind: ParseErrorKind) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseError::new(e.span, kind)),
        }
    }

    fn expected_expr(self) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseError::new(e.span, ParseErrorKind::ExpectedExpression)),
        }
    }

    fn expect_token(self, t: Token<'static>) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseError::new(e.span, ParseErrorKind::ExpectedToken(t))),
        }
    }
}
