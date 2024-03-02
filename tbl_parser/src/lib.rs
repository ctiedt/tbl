pub mod error;
mod pattern;
mod token;
pub mod types;

use std::ops::Range;

use crate::types::Statement;
use ariadne::{sources, Label, Report};
use error::{ParseError, ParseErrorKind, ParseResult, ParseResultExt};
use pattern::Pattern;
pub use token::Token;
use types::{Declaration, Expression, ExternTaskParams, Program, Type};

pub type Span = Range<usize>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Location {
    line: usize,
    column: usize,
}

#[derive(Clone, Copy)]
pub struct Source<'a> {
    pub name: &'a str,
    pub contents: &'a str,
}

pub struct Parser<'a> {
    source: Source<'a>,
    tokens: Vec<(Token<'a>, Span)>,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<(Token<'a>, Span)>, source: Source<'a>) -> Self {
        Self {
            source,
            tokens,
            cursor: 0,
        }
    }

    fn current(&self) -> Token<'a> {
        self.tokens[self.cursor].0
    }

    fn current_span(&self) -> Span {
        self.tokens[self.cursor].1.clone()
    }

    fn done(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        self.cursor += 1;
        if self.cursor >= self.tokens.len() {
            Err(ParseError::new(
                self.current_span(),
                ParseErrorKind::NoMoreTokens,
            ))
        } else {
            Ok(())
        }
    }

    fn accept(&mut self, p: impl Pattern<Token<'a>>) -> ParseResult<Token<'a>> {
        if p.accept(self.current()) {
            self.cursor += 1;
            Ok(Some(self.tokens[self.cursor - 1].0))
        } else {
            Ok(None)
        }
    }

    fn require(&mut self, p: impl Pattern<Token<'a>>) -> ParseResult<Token<'a>> {
        match self.accept(p) {
            Ok(Some(v)) => Ok(Some(v)),
            Ok(None) => Err(ParseError::new(self.current_span(), ParseErrorKind::Any)),
            Err(e) => Err(e),
        }
    }

    fn accept_delimited(
        &mut self,
        start: impl Pattern<Token<'a>>,
        end: impl Pattern<Token<'a>> + Copy,
    ) -> ParseResult<Vec<(Token<'a>, Span)>> {
        let mut tokens = vec![];
        self.accept(start)?;
        while self.accept(end).unwrap().is_none() {
            tokens.push(self.tokens[self.cursor].clone());
            if self.advance().is_err() {
                return Err(ParseError::new(
                    self.current_span(),
                    ParseErrorKind::NoMoreTokens,
                ));
            }
        }

        Ok(Some(tokens))
    }

    fn accept_many(&mut self, p: &[impl Pattern<Token<'a>> + Copy]) -> ParseResult<Vec<Token<'a>>> {
        let mut tokens = vec![];
        for pat in p {
            match self.accept(*pat) {
                Ok(Some(t)) => tokens.push(t),
                Ok(None) => {}
                Err(e) => return Err(e),
            }
        }
        Ok(Some(tokens))
    }

    pub fn parse(&mut self) -> Result<Program, Box<Report>> {
        let mut declarations = vec![];
        let mut errors = vec![];
        while !self.done() {
            match self.parse_extern_task() {
                Ok(Some(t)) => declarations.push(t),
                Ok(None) => {}
                Err(e) => errors.push(e),
            }

            match self.parse_task() {
                Ok(Some(t)) => declarations.push(t),
                Ok(None) => {}
                Err(e) => errors.push(e),
            }
        }

        for error in errors {
            Report::build(
                ariadne::ReportKind::Error,
                self.source.name,
                error.span.start,
            )
            .with_message("Parsing error".to_string())
            .with_label(
                Label::new((self.source.name, error.span)).with_message(format!("{}", error.kind)),
            )
            .finish()
            .print((
                self.source.name,
                ariadne::Source::from(self.source.contents),
            ))
            .unwrap();
        }

        Ok(Program { declarations })
    }

    fn parse_separated<T>(
        &mut self,
        sep: Token<'a>,
        p: impl Fn(&[(Token<'a>, Span)]) -> ParseResult<T>,
    ) -> Vec<ParseResult<T>> {
        let mut res = vec![];
        let splits = self.tokens.split(|(t, _)| *t == sep);
        for split in splits {
            res.push(p(split));
        }

        res
    }

    pub fn parse_extern_task(&mut self) -> ParseResult<types::Declaration> {
        self.require(Token::Extern)?;
        self.require(Token::Task)
            .error(ParseErrorKind::UnknownKeyword)?;
        let name = self
            .accept(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();
        let param_tokens = self
            .accept_delimited(Token::ParenOpen, Token::ParenClose)?
            .unwrap();

        let mut param_parser = Parser::new(param_tokens, self.source);
        let params = param_parser
            .parse_external_params()?
            .ok_or(ParseError::new(
                self.current_span(),
                ParseErrorKind::BadParams,
            ))?;
        //param_parser.parse_separated(Token::Comma, |t| {
        //    let mut parser = Parser::new(Vec::from(t));
        //    parser.parse_external_params()
        //});

        Ok(Some(Declaration::ExternTask {
            name,
            params,
            returns: None,
        }))
    }

    pub fn parse_task(&mut self) -> ParseResult<types::Declaration> {
        //let mut params = vec![];
        self.require(Token::Task)?;
        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();
        let param_tokens = self
            .accept_delimited(Token::ParenOpen, Token::ParenClose)?
            .unwrap();
        let params = Parser::new(param_tokens, self.source)
            .parse_params()?
            .unwrap();
        if let Ok(locals) = self.accept_delimited(Token::LessThan, Token::GreaterThan) {
            todo!()
        }

        let stmt_tokens = self
            .accept_delimited(Token::CurlyOpen, Token::CurlyClose)?
            .unwrap();

        let mut stmt_parser = Parser::new(stmt_tokens, self.source);
        let stmts: Vec<Result<Statement, ParseError>> = stmt_parser
            .parse_separated(Token::Semicolon, |t| parse_stmt(t))
            .into_iter()
            .filter_map(|t| match t {
                Ok(Some(v)) => Some(Ok(v)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            })
            .collect();

        let body: Result<Vec<Statement>, ParseError> = stmts.into_iter().collect();

        Ok(Some(Declaration::Task {
            location: Location { line: 0, column: 0 },
            name,
            params,
            returns: None,
            locals: vec![],
            body: body.unwrap(),
        }))
    }

    fn parse_param(&mut self) -> ParseResult<(String, Type)> {
        let name = self
            .accept(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();
        self.accept(Token::Colon)?.unwrap();
        let ty = self.parse_ty()?;
        Ok(Some((name, ty.unwrap())))
    }

    fn parse_external_params(&mut self) -> ParseResult<ExternTaskParams> {
        match self.accept(Token::Varargs)? {
            Some(_) => Ok(Some(ExternTaskParams::Variadic)),
            None => Ok(Some(ExternTaskParams::WellKnown(
                self.parse_params()?.ok_or(ParseError::new(
                    self.current_span(),
                    ParseErrorKind::BadParams,
                ))?,
            ))),
        }
    }

    fn parse_params(&mut self) -> ParseResult<Vec<(String, Type)>> {
        Ok(Some(vec![]))
    }

    fn parse_ty(&mut self) -> ParseResult<types::Type> {
        if let Some(res) = self.accept(Token::Ident("bool"))? {}
        if let Some(res) = self.accept(Token::Ident("any"))? {}
        Ok(None)
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let current_span = self.current_span();
        if let Some(res) = self.accept([Token::Return].as_slice())? {
            match res {
                Token::Return => return Ok(Some(Statement::Return(None))),
                _ => {
                    return Err(ParseError::new(
                        current_span,
                        ParseErrorKind::UnknownKeyword,
                    ))
                }
            }
        }
        Ok(None)
    }
}

fn parse_stmt(tokens: &[(Token<'_>, Span)]) -> ParseResult<Statement> {
    dbg!(tokens);
    match tokens {
        [(Token::Return, _)] => Ok(Some(Statement::Return(None))),
        [] => Ok(None),
        other => Ok(Some(Statement::Expression(parse_expr(other)?.unwrap()))),
        //_ => Err(ParseError::Any),
    }
}

fn parse_expr(tokens: &[(Token<'_>, Span)]) -> ParseResult<Expression> {
    match tokens {
        [(Token::Number(n), _)] => Ok(Some(Expression::Literal(types::Literal::Int(*n as i64)))),
        _ => Ok(None),
    }
}

fn parse_arg(tokens: &[(Token<'_>, Span)]) -> ParseResult<(String, Type)> {
    todo!()
}
