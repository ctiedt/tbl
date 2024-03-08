pub mod error;
mod pattern;
mod token;
pub mod types;

use std::{ops::Range, path::Path};

use crate::types::Statement;
use ariadne::{Label, Report};
use error::{ParseError, ParseErrorKind, ParseResult, ParseResultExt};
use logos::Logos;
use pattern::Pattern;
pub use token::Token;
use types::{Declaration, Expression, ExternTaskParams, Program, Type};

pub type Span = Range<usize>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
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
            Ok(None) => Err(ParseError::new(
                self.current_span(),
                ParseErrorKind::ExpectedPattern(p.display()),
            )),
            Err(e) => Err(e),
        }
    }

    fn accept_delimited(
        &mut self,
        start: impl Pattern<Token<'a>>,
        end: impl Pattern<Token<'a>> + Copy,
    ) -> ParseResult<Vec<(Token<'a>, Span)>> {
        let mut tokens = vec![];
        if self.accept(start)?.is_none() {
            return Ok(None);
        }
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

    pub fn parse(&mut self) -> Result<Program, Box<Report>> {
        let mut declarations = vec![];
        let mut errors = vec![];
        let mut current_cursor = self.cursor;
        while !self.done() {
            let mut error = None;
            let mut any_ok = false;
            match self.parse_extern_task() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    any_ok = true;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_task() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    any_ok = true;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            if !any_ok {
                errors.push(error.unwrap());
            }

            if self.cursor == current_cursor {
                break;
            }
            current_cursor = self.cursor;
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

        self.require(Token::Semicolon)?;

        Ok(Some(Declaration::ExternTask {
            name,
            params,
            returns: None,
        }))
    }

    pub fn parse_task(&mut self) -> ParseResult<types::Declaration> {
        let mut params = vec![];
        let mut locals = vec![];
        let mut returns = None;
        self.require(Token::Task)?;
        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();

        self.require(Token::ParenOpen)?;
        let mut no_params = true;
        while let Some(param) = self.parse_param()? {
            no_params = false;
            params.push(param);
            if self.accept(Token::Comma)?.is_none() {
                self.require(Token::ParenClose)?;
            }
        }
        if no_params {
            self.require(Token::ParenClose)?;
        }

        if self.accept(Token::Arrow)?.is_some() {
            returns.replace(self.parse_ty()?.unwrap());
        }

        if self.accept(Token::LessThan)?.is_some() {
            while let Some(local) = self.parse_param()? {
                locals.push(local);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::GreaterThan)?;
                }
            }
        }

        let mut body = vec![];
        self.require(Token::CurlyOpen)?;

        while let Some(stmt) = self.parse_stmt()? {
            body.push(stmt);
        }

        self.require(Token::CurlyClose)?;

        Ok(Some(Declaration::Task {
            location: Location { line: 0, column: 0 },
            name,
            params,
            returns,
            locals,
            body,
        }))
    }

    fn parse_param(&mut self) -> ParseResult<(String, Type)> {
        if let Some(name) = self.accept(|t| matches!(t, Token::Ident(_)))? {
            let name = name.to_string();
            self.require(Token::Colon)?.unwrap();
            let ty = self.parse_ty()?;
            Ok(Some((name, ty.unwrap())))
        } else {
            Ok(None)
        }
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
        if self.accept(Token::Ident("bool"))?.is_some() {
            return Ok(Some(Type::Bool));
        }
        if self.accept(Token::Ident("any"))?.is_some() {
            return Ok(Some(Type::Any));
        }
        if let Some(res) = self.accept(|t| matches!(t, Token::Ident(_)))? {
            let ty_str = res.to_string();
            let (sign, bits) = ty_str.split_at(1);
            let signed = match sign {
                "u" => false,
                "i" => true,
                _ => {
                    return Err(ParseError::new(
                        self.current_span(),
                        ParseErrorKind::BadType,
                    ))
                }
            };
            let width = bits.parse().map_err(|e| {
                ParseError::new(self.current_span(), ParseErrorKind::ParseIntError(e))
            })?;
            return Ok(Some(Type::Integer { signed, width }));
        }
        Ok(None)
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        if self.accept(Token::If)?.is_some() {
            let test = self.parse_expr()?.unwrap();
            self.require(Token::CurlyOpen)?;
            let mut then = vec![];
            let mut else_ = vec![];
            while let Some(stmt) = self.parse_stmt()? {
                then.push(stmt);
            }
            self.require(Token::CurlyClose)?;
            if self.accept(Token::Else)?.is_some() {
                self.require(Token::CurlyOpen)?;
                while let Some(stmt) = self.parse_stmt()? {
                    else_.push(stmt);
                }
                self.require(Token::CurlyClose)?;
            }
            return Ok(Some(Statement::Conditional { test, then, else_ }));
        }
        if self.accept(Token::Return)?.is_some() {
            let ret_value = self.parse_expr().expected_expr()?;
            self.require(Token::Semicolon)
                .expect_token(Token::Semicolon)?;
            return Ok(Some(Statement::Return(ret_value)));
        }
        if let Some(expr) = self.parse_expr()? {
            if self
                .accept(Token::Semicolon)
                .expect_token(Token::Semicolon)?
                .is_some()
            {
                return Ok(Some(Statement::Expression(expr)));
            }
            if self
                .accept(Token::Equals)
                .error(ParseErrorKind::ExpectedToken(Token::Equals))?
                .is_some()
            {
                let rhs = self.parse_expr().expected_expr()?.unwrap();
                self.require(Token::Semicolon)
                    .expect_token(Token::Semicolon)?;
                return Ok(Some(Statement::Assign {
                    location: expr,
                    value: rhs,
                }));
            }
        }
        Ok(None)
    }

    fn parse_expr(&mut self) -> ParseResult<Expression> {
        if let Some(left) = self.parse_value()? {
            if self.accept(Token::Plus)?.is_some() {
                let right = self.parse_expr()?.ok_or(ParseError::new(
                    self.current_span(),
                    ParseErrorKind::ExpectedExpression,
                ))?;
                return Ok(Some(Expression::BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: types::BinaryOperator::Add,
                }));
            }
            if self.accept(Token::ParenOpen)?.is_some() {
                let mut args = vec![];
                let mut no_args = true;
                while let Some(arg) = self.parse_expr()? {
                    no_args = false;
                    args.push(arg);
                    if self.accept(Token::Comma)?.is_none() {
                        self.require(Token::ParenClose)?;
                    }
                }
                if no_args {
                    self.require(Token::ParenClose)?;
                }

                return Ok(Some(Expression::Call {
                    task: Box::new(left),
                    args,
                }));
            }

            return Ok(Some(left));
        }

        self.parse_value()
    }

    fn parse_value(&mut self) -> ParseResult<Expression> {
        if self.accept(Token::True)?.is_some() {
            return Ok(Some(Expression::Literal(types::Literal::Bool(true))));
        }
        if self.accept(Token::False)?.is_some() {
            return Ok(Some(Expression::Literal(types::Literal::Bool(false))));
        }
        if let Some(n) = self.accept(|t| matches!(t, Token::Number(_)))? {
            let Token::Number(val) = n else {
                unreachable!()
            };
            return Ok(Some(Expression::Literal(types::Literal::Int(val as i64))));
        }
        if let Some(t) = self.accept(|t| matches!(t, Token::String(_)))? {
            let Token::String(val) = t else {
                unreachable!()
            };

            return Ok(Some(Expression::Literal(types::Literal::String(
                snailquote::unescape(&val.to_string()).unwrap(),
            ))));
        }
        if let Some(n) = self.accept(|t| matches!(t, Token::Ident(_)))? {
            let Token::Ident(val) = n else { unreachable!() };
            return Ok(Some(Expression::Var(val.to_string())));
        }
        Ok(None)
    }
}

pub fn parse<'a, P: AsRef<Path>>(path: P) -> Program {
    let path_str = path.as_ref().display().to_string();
    let source = std::fs::read_to_string(path).unwrap();
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
            name: &path_str,
            contents: &source,
        },
    );
    parser.parse().unwrap()
}
