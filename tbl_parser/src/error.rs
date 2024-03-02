use crate::Span;
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
    #[error("any")]
    Any,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("unknown keyword")]
    UnknownKeyword,
    #[error("no more tokens")]
    NoMoreTokens,
    #[error("malformed task parameters")]
    BadParams,
}

pub type ParseResult<T> = Result<Option<T>, ParseError>;

pub trait ParseResultExt {
    fn error(self, kind: ParseErrorKind) -> Self;
}

impl<T> ParseResultExt for ParseResult<T> {
    fn error(self, kind: ParseErrorKind) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseError::new(e.span, kind)),
        }
    }
}
