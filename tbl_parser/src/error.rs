use crate::{Span, Token};
use std::{fmt::Display, num::ParseIntError};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub struct ParseError {
    pub span: Span,
    #[source]
    pub kind: ParseErrorKind,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error at {}..{}: {}",
            self.span.start, self.span.end, self.kind
        )
    }
}

impl ParseError {
    pub(crate) fn new(span: Span, kind: ParseErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Error, Clone)]
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
    #[error("expected statement")]
    ExpectedStmt,
    #[error("bad integer value")]
    ParseIntError(#[from] ParseIntError),
    #[error("expected identifier")]
    ExpectedIdent,
    #[error("expected type")]
    ExpectedType,
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
