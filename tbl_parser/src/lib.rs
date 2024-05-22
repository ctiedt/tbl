pub mod error;
pub mod module;
mod pattern;
mod token;
pub mod types;

use std::{ops::Range, path::Path};

use crate::{pattern::IdentPattern, types::Statement};
use error::{ParseError, ParseErrorKind, ParseResult, ParseResultExt};
use logos::Logos;
use pattern::Pattern;
pub use token::Token;
use types::{
    BinaryOperator, DeclarationKind, Expression, ExpressionKind, ExternTaskParams, Literal,
    MatchPattern, PostfixOperator, Program, StatementKind, Type, UnaryOperator,
};

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

impl<'a> Source<'a> {
    pub fn row_col(&self, span: Span) -> (Location, Location) {
        let mut row = 0;
        let mut col = 0;
        for (idx, c) in self.contents.char_indices() {
            if idx == span.start {
                break;
            }
            col += 1;
            if c == '\n' {
                row += 1;
                col = 0;
            }
        }

        (
            Location {
                line: row,
                column: col,
            },
            Location {
                line: row,
                column: col + span.count(),
            },
        )
    }
}

pub struct Parser<'a> {
    tokens: Vec<(Token<'a>, Span)>,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<(Token<'a>, Span)>) -> Self {
        Self { tokens, cursor: 0 }
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

    fn accept_sequence(&mut self, p: &[impl Pattern<Token<'a>>]) -> ParseResult<Vec<Token<'a>>> {
        let mut tokens = vec![];
        for (idx, pat) in p.iter().enumerate() {
            match self.accept(*pat) {
                Ok(Some(t)) => {
                    tokens.push(t);
                }
                Ok(None) => {
                    self.cursor -= idx;
                    return Ok(None);
                }
                Err(e) => return Err(e),
            }
        }
        Ok(Some(tokens))
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError::new(self.current_span(), kind)
    }

    pub fn parse(&mut self) -> (Program, Vec<ParseError>) {
        let mut declarations = vec![];
        let mut errors = vec![];
        let mut current_cursor = self.cursor;
        while !self.done() {
            let mut error = None;
            match self.parse_extern_task() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_extern_global() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_use() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_task() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_directive() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_global() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_struct() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            match self.parse_enum() {
                Ok(Some(t)) => {
                    declarations.push(t);
                    continue;
                }
                Ok(None) => {}
                Err(e) => {
                    error.replace(e);
                }
            }

            if let Some(error) = error {
                errors.push(error);
            }

            if self.cursor == current_cursor {
                break;
            }
            current_cursor = self.cursor;
        }

        (Program { declarations }, errors)
    }

    fn parse_directive(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self.accept(Token::At)?.is_none() {
            return Ok(None);
        }
        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();

        self.require(Token::ParenOpen)?;

        let mut args = vec![];
        let mut no_params = true;
        while let Some(param) = self.parse_literal()? {
            no_params = false;
            args.push(param);
            if self.accept(Token::Comma)?.is_none() {
                self.require(Token::ParenClose)?;
            }
        }
        if no_params {
            self.require(Token::ParenClose)?;
        }

        let end = self.current_span().end;
        self.require(Token::Semicolon)?;

        Ok(Some(
            DeclarationKind::Directive { name, args }.with_span(start..end),
        ))
    }

    fn parse_use(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self.accept(Token::Use)?.is_none() {
            return Ok(None);
        }

        let module = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .ok_or(self.error(ParseErrorKind::ExpectedExpression))?
            .to_string();

        let end = self.current_span().end;
        self.require(Token::Semicolon)?;

        Ok(Some(DeclarationKind::Use { module }.with_span(start..end)))
    }

    fn parse_extern_global(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self
            .accept_sequence(&[Token::Extern, Token::Global])?
            .is_none()
        {
            return Ok(None);
        }

        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .ok_or(ParseError::new(
                self.current_span(),
                ParseErrorKind::UnexpectedToken,
            ))?
            .to_string();

        self.require(Token::Colon)?;

        let type_ = self
            .parse_ty()?
            .ok_or(self.error(ParseErrorKind::BadType))?;

        let end = self.current_span().end;
        self.require(Token::Semicolon)?;

        Ok(Some(
            DeclarationKind::ExternGlobal { name, type_ }.with_span(start..end),
        ))
    }

    fn parse_global(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self.accept(Token::Global)?.is_none() {
            return Ok(None);
        }
        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .ok_or(ParseError::new(
                self.current_span(),
                ParseErrorKind::UnexpectedToken,
            ))?
            .to_string();

        self.require(Token::Colon)?;

        let type_ = self
            .parse_ty()?
            .ok_or(self.error(ParseErrorKind::BadType))?;

        self.require(Token::Equals)?;

        let value = self
            .parse_expr()?
            .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;

        self.require(Token::Semicolon)?;
        let end = self.current_span().end;

        Ok(Some(
            DeclarationKind::Global { name, type_, value }.with_span(start..end),
        ))
    }

    fn parse_struct(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self.accept(Token::Struct)?.is_none() {
            return Ok(None);
        }

        let end = self.current_span().end;

        let (name, members) = self
            .parse_struct_variant()?
            .ok_or(self.error(ParseErrorKind::BadType))?;

        if !matches!(self.tokens[self.cursor - 1].0, Token::CurlyClose) {
            self.require(Token::Semicolon)?;
        }

        Ok(Some(
            DeclarationKind::Struct { name, members }.with_span(start..end),
        ))
    }

    fn parse_struct_variant(&mut self) -> ParseResult<(String, Vec<(String, Type)>)> {
        let name = if let Some(n) = self.accept(IdentPattern)? {
            n.to_string()
        } else {
            return Ok(None);
        };

        let mut members = vec![];
        if self.accept(Token::CurlyOpen)?.is_some() {
            let mut no_params = true;
            while let Some(param) = self.parse_param()? {
                no_params = false;
                members.push(param);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::CurlyClose)?;
                }
            }
            if no_params {
                self.require(Token::CurlyClose)?;
            }
        } else if self.accept(Token::ParenOpen)?.is_some() {
            let mut param_counter = 0;
            while let Some(param) = self.parse_ty()? {
                members.push((param_counter.to_string(), param));
                param_counter += 1;
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::ParenClose)?;
                }
            }
            if param_counter == 0 {
                self.require(Token::ParenClose)?;
            }
        }

        Ok(Some((name, members)))
    }

    fn parse_extern_task(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        let mut returns = None;

        if self
            .accept_sequence(&[Token::Extern, Token::Task])?
            .is_none()
        {
            return Ok(None);
        }
        let name = self
            .accept(|t| matches!(t, Token::Ident(_)))?
            .ok_or(self.error(ParseErrorKind::ExpectedIdent))?
            .to_string();

        let params = self
            .parse_external_params()?
            .ok_or(self.error(ParseErrorKind::BadParams))?;

        if self.accept(Token::Arrow)?.is_some() {
            returns.replace(self.parse_ty()?.unwrap());
        }

        let end = self.current_span().end;
        self.require(Token::Semicolon)?;

        Ok(Some(
            DeclarationKind::ExternTask {
                name,
                params,
                returns,
            }
            .with_span(start..end),
        ))
    }

    pub fn parse_task(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        let mut params = vec![];
        let mut locals = vec![];
        let mut returns = None;
        if self.accept(Token::Task)?.is_none() {
            return Ok(None);
        }
        let name = self.require(IdentPattern)?.unwrap().to_string();

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

        let end = self.current_span().end;
        self.require(Token::CurlyClose)?;

        Ok(Some(
            DeclarationKind::Task {
                name,
                params,
                returns,
                locals,
                body,
            }
            .with_span(start..end),
        ))
    }

    fn parse_param(&mut self) -> ParseResult<(String, Type)> {
        if let Some(name) = self.accept(IdentPattern)? {
            let name = name.to_string();
            self.require(Token::Colon)?;
            let ty = self
                .parse_ty()?
                .ok_or(self.error(ParseErrorKind::BadType))?;
            Ok(Some((name, ty)))
        } else {
            Ok(None)
        }
    }

    fn parse_external_params(&mut self) -> ParseResult<ExternTaskParams> {
        self.require(Token::ParenOpen)?;
        match self.accept(Token::Varargs)? {
            Some(_) => {
                self.require(Token::ParenClose)?;
                Ok(Some(ExternTaskParams::Variadic))
            }
            None => {
                let mut params = vec![];
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
                Ok(Some(ExternTaskParams::WellKnown(params)))
            }
        }
    }

    fn parse_ty(&mut self) -> ParseResult<types::Type> {
        if self.accept(Token::Ident("bool"))?.is_some() {
            return Ok(Some(Type::Bool));
        }
        if self.accept(Token::Ident("any"))?.is_some() {
            return Ok(Some(Type::Any));
        }
        if self.accept(Token::Task)?.is_some() {
            let mut params = vec![];
            let mut returns = None;

            self.require(Token::ParenOpen)?;
            let mut no_params = true;
            while let Some(param) = self.parse_ty()? {
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
                returns.replace(Box::new(self.parse_ty()?.unwrap()));
            }

            return Ok(Some(Type::TaskPtr { params, returns }));
        }
        if self.accept(Token::BracketOpen)?.is_some() {
            let item = Box::new(
                self.parse_ty()?
                    .ok_or(self.error(ParseErrorKind::BadType))?,
            );

            self.require(Token::Semicolon)?;

            let length = self
                .parse_int()?
                .ok_or(self.error(ParseErrorKind::BadType))?;

            self.require(Token::BracketClose)?;

            return Ok(Some(Type::Array { item, length }));
        }
        if let Some(res) = self.accept(|t| matches!(t, Token::IntType(_)))? {
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
        if let Some(res) = self.accept(IdentPattern)? {
            return Ok(Some(Type::Named(res.to_string())));
        }
        if self.accept(Token::And)?.is_some() {
            let inner = self
                .parse_ty()?
                .ok_or(self.error(ParseErrorKind::BadType))?;
            return Ok(Some(Type::Pointer(Box::new(inner))));
        }
        Ok(None)
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let start = self.current_span().start;
        if self.accept(Token::CurlyOpen)?.is_some() {
            let mut statements = vec![];
            while let Some(stmt) = self.parse_stmt()? {
                statements.push(stmt);
            }
            self.require(Token::CurlyClose)?;
            let end = self.current_span().end;
            return Ok(Some(
                StatementKind::Block { statements }.with_span(start..end),
            ));
        }
        if self.accept(Token::If)?.is_some() {
            let test = self
                .parse_expr()?
                .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
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
            let end = self.current_span().end;
            return Ok(Some(
                StatementKind::Conditional { test, then, else_ }.with_span(start..end),
            ));
        }
        if self.accept(Token::Loop)?.is_some() {
            self.require(Token::CurlyOpen)?;
            let mut body = vec![];
            while let Some(stmt) = self.parse_stmt()? {
                body.push(stmt);
            }
            self.require(Token::CurlyClose)?;
            let end = self.current_span().end;
            return Ok(Some(StatementKind::Loop { body }.with_span(start..end)));
        }
        if self.accept(Token::Match)?.is_some() {
            let value = self
                .parse_expr()?
                .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
            let mut branches = vec![];
            self.require(Token::CurlyOpen)?;
            loop {
                let Some(pat) = self.parse_match_pattern()? else {
                    break;
                };
                let Some(_) = self.require(Token::Arrow)? else {
                    break;
                };
                let Some(stmt) = self.parse_stmt()? else {
                    break;
                };
                branches.push((pat, stmt));
            }
            self.require(Token::CurlyClose)?;
            let end = self.current_span().end;
            return Ok(Some(
                StatementKind::Match { value, branches }.with_span(start..end),
            ));
        }
        if self.accept(Token::Return)?.is_some() {
            let ret_value = self.parse_expr()?;
            self.require(Token::Semicolon)?;
            let end = self.current_span().end;
            return Ok(Some(StatementKind::Return(ret_value).with_span(start..end)));
        }
        if self.accept(Token::Exit)?.is_some() {
            self.require(Token::Semicolon)?;
            let end = self.current_span().end;
            return Ok(Some(StatementKind::Exit.with_span(start..end)));
        }
        if self.accept(Token::Break)?.is_some() {
            self.require(Token::Semicolon)?;
            let end = self.current_span().end;
            return Ok(Some(StatementKind::Break.with_span(start..end)));
        }
        if self.accept(Token::Schedule)?.is_some() {
            let task = self
                .require(IdentPattern)?
                .ok_or(self.error(ParseErrorKind::ExpectedIdent))?
                .to_string();

            let mut args = vec![];
            self.require(Token::ParenOpen)?;
            let mut no_params = true;
            while let Some(param) = self.parse_expr()? {
                no_params = false;
                args.push(param);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::ParenClose)?;
                }
            }
            if no_params {
                self.require(Token::ParenClose)?;
            }

            self.require(Token::Semicolon)?;
            let end = self.current_span().end;
            return Ok(Some(
                StatementKind::Schedule { task, args }.with_span(start..end),
            ));
        }
        if let Some(expr) = self.parse_expr()? {
            if self.accept(Token::Semicolon)?.is_some() {
                let end = self.current_span().end;
                return Ok(Some(StatementKind::Expression(expr).with_span(start..end)));
            }
            if self
                .accept(Token::Equals)
                .error(ParseErrorKind::ExpectedToken(Token::Equals))?
                .is_some()
            {
                let rhs = self.parse_expr().expected_expr()?.unwrap();
                self.require(Token::Semicolon)?;
                let end = self.current_span().end;
                return Ok(Some(
                    StatementKind::Assign {
                        location: expr,
                        value: rhs,
                    }
                    .with_span(start..end),
                ));
            }
        }
        Ok(None)
    }

    fn parse_match_pattern(&mut self) -> ParseResult<MatchPattern> {
        if let Some(pat) = self.accept(IdentPattern)? {
            return Ok(Some(MatchPattern::Ident(pat.display())));
        }
        if self.accept(Token::Star)?.is_some() {
            return Ok(Some(MatchPattern::Any));
        }
        if let Some(expr) = self.parse_expr()? {
            return Ok(Some(MatchPattern::Expr(expr)));
        }
        Ok(None)
    }

    fn parse_expr(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(left) = self.parse_comparison()? {
            if let Some(op) = self.accept([Token::DoubleEqual, Token::Unequal].as_slice())? {
                let operator = match op {
                    Token::DoubleEqual => BinaryOperator::Equal,
                    Token::Unequal => BinaryOperator::Unequal,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_expr()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                let end = self.current_span().end;
                Ok(Some(
                    ExpressionKind::BinaryOperation {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator,
                    }
                    .with_span(start..end),
                ))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(left) = self.parse_term()? {
            if let Some(op) = self.accept(
                [
                    Token::GreaterThan,
                    Token::GreaterEqual,
                    Token::LessThan,
                    Token::LessEqual,
                    Token::And,
                    Token::Pipe,
                ]
                .as_slice(),
            )? {
                let operator = match op {
                    Token::GreaterThan => BinaryOperator::GreaterThan,
                    Token::GreaterEqual => BinaryOperator::GreaterOrEqual,
                    Token::LessThan => BinaryOperator::LessThan,
                    Token::LessEqual => BinaryOperator::LessOrEqual,
                    Token::And => BinaryOperator::And,
                    Token::Pipe => BinaryOperator::Or,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_comparison()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                let end = self.current_span().end;
                Ok(Some(
                    ExpressionKind::BinaryOperation {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator,
                    }
                    .with_span(start..end),
                ))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_term(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(left) = self.parse_factor()? {
            if let Some(op) = self.accept([Token::Plus, Token::Minus].as_slice())? {
                let operator = match op {
                    Token::Plus => BinaryOperator::Add,
                    Token::Minus => BinaryOperator::Subtract,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_term()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                let end = self.current_span().end;
                Ok(Some(
                    ExpressionKind::BinaryOperation {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator,
                    }
                    .with_span(start..end),
                ))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_factor(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(left) = self.parse_unary()? {
            if let Some(op) = self.accept([Token::Star, Token::Slash].as_slice())? {
                let operator = match op {
                    Token::Star => BinaryOperator::Multiply,
                    Token::Slash => BinaryOperator::Divide,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_factor()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                let end = self.current_span().end;
                Ok(Some(
                    ExpressionKind::BinaryOperation {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator,
                    }
                    .with_span(start..end),
                ))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_unary(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(op) =
            self.accept([Token::Exclamation, Token::Minus, Token::Star, Token::And].as_slice())?
        {
            let operator = match op {
                Token::Exclamation => UnaryOperator::Not,
                Token::Minus => UnaryOperator::Minus,
                Token::Star => UnaryOperator::Dereference,
                Token::And => UnaryOperator::Reference,
                _ => unreachable!(),
            };
            let value = Box::new(
                self.parse_postfix()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?,
            );
            let end = self.current_span().end;
            Ok(Some(
                ExpressionKind::UnaryOperation { value, operator }.with_span(start..end),
            ))
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        match self.parse_primary()? {
            Some(mut value) => {
                while let Some(pf) = self.parse_postfix_operator()? {
                    let end = self.current_span().end;
                    match pf {
                        PostfixOperator::StructAccess { member } => {
                            value = ExpressionKind::StructAccess {
                                value: Box::new(value),
                                member,
                            }
                            .with_span(start..end);
                        }
                        PostfixOperator::Index { at } => {
                            value = ExpressionKind::Index {
                                value: Box::new(value),
                                at: Box::new(at),
                            }
                            .with_span(start..end);
                        }
                        PostfixOperator::Cast { to } => {
                            value = ExpressionKind::Cast {
                                value: Box::new(value),
                                to,
                            }
                            .with_span(start..end);
                        }
                        PostfixOperator::Call { args } => {
                            value = ExpressionKind::Call {
                                task: Box::new(value),
                                args,
                            }
                            .with_span(start..end);
                        }
                    }
                }
                Ok(Some(value))
            }
            None => Ok(None),
        }
    }

    fn parse_postfix_operator(&mut self) -> ParseResult<PostfixOperator> {
        if self.accept(Token::Period)?.is_some() {
            let member = self
                .require(IdentPattern)?
                .ok_or(self.error(ParseErrorKind::ExpectedIdent))?
                .to_string();
            return Ok(Some(PostfixOperator::StructAccess { member }));
        }
        if self.accept(Token::BracketOpen)?.is_some() {
            let index = self
                .parse_expr()?
                .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
            self.require(Token::BracketClose)?;
            return Ok(Some(PostfixOperator::Index { at: index }));
        }
        if self.accept(Token::As)?.is_some() {
            let to = self
                .parse_ty()?
                .ok_or(self.error(ParseErrorKind::BadType))?;
            return Ok(Some(PostfixOperator::Cast { to }));
        }
        if self.accept(Token::ParenOpen)?.is_some() {
            let mut args = vec![];
            let mut no_args = true;
            while let Some(arg) = self.parse_expr()? {
                no_args = false;
                args.push(arg);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::ParenClose)?;
                    break;
                }
            }
            if no_args {
                self.require(Token::ParenClose)?;
            }

            return Ok(Some(PostfixOperator::Call { args }));
        }
        Ok(None)
    }

    fn parse_primary(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(value) = self.parse_value()? {
            return Ok(Some(value));
        }
        if self.accept(Token::Hash)?.is_some() {
            let value = self
                .parse_ty()?
                .ok_or(self.error(ParseErrorKind::BadType))?;
            let end = self.current_span().end;
            return Ok(Some(ExpressionKind::SizeOf { value }.with_span(start..end)));
        }
        if self.accept(Token::ParenOpen)?.is_some() {
            let expr = self
                .parse_expr()?
                .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
            self.require(Token::ParenClose)?;
            return Ok(Some(expr));
        }
        Ok(None)
    }

    fn parse_value(&mut self) -> ParseResult<Expression> {
        let start = self.current_span().start;
        if let Some(lit) = self.parse_literal()? {
            let end = self.current_span().end;
            return Ok(Some(ExpressionKind::Literal(lit).with_span(start..end)));
        }
        if let Some(n) = self.accept(IdentPattern)? {
            let end = self.current_span().end;
            let Token::Ident(val) = n else { unreachable!() };
            return Ok(Some(
                ExpressionKind::Var(val.to_string()).with_span(start..end),
            ));
        }

        Ok(None)
    }

    fn parse_member(&mut self) -> ParseResult<(String, Expression)> {
        let name = self
            .require(IdentPattern)?
            .ok_or(self.error(ParseErrorKind::ExpectedExpression))?
            .to_string();

        self.require(Token::Colon)?;

        let value = self
            .parse_expr()?
            .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;

        Ok(Some((name, value)))
    }

    fn parse_literal(&mut self) -> ParseResult<Literal> {
        if self.accept(Token::True)?.is_some() {
            return Ok(Some(Literal::Bool(true)));
        }
        if self.accept(Token::False)?.is_some() {
            return Ok(Some(Literal::Bool(false)));
        }
        if let Some(n) = self.parse_int()? {
            return Ok(Some(Literal::Int(n as i64)));
        }
        // if self.accept(Token::CurlyOpen)?.is_some() {
        //     let mut members = vec![];
        //     let mut no_members = true;
        //     while let Some(arg) = self.parse_member()? {
        //         no_members = false;
        //         members.push(arg);
        //         if self.accept(Token::Comma)?.is_none() {
        //             self.require(Token::CurlyClose)?;
        //             break;
        //         }
        //     }
        //     if no_members {
        //         self.require(Token::CurlyClose)?;
        //     }

        //     return Ok(Some(Literal::Struct(members)));
        // }
        if self.accept(Token::BracketOpen)?.is_some() {
            let mut items = vec![];
            let mut no_items = true;
            while let Some(arg) = self.parse_expr()? {
                no_items = false;
                items.push(arg);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::BracketClose)?;
                }
            }
            if no_items {
                self.require(Token::BracketClose)?;
            }

            return Ok(Some(Literal::Array(items)));
        }

        if let Some(struct_lit) = self.parse_struct_literal()? {
            return Ok(Some(Literal::Struct(struct_lit)));
        }

        if let Some(t) = self.accept(|t| matches!(t, Token::String(_)))? {
            let Token::String(val) = t else {
                unreachable!()
            };

            return Ok(Some(Literal::String(
                snailquote::unescape(&val.to_string()).unwrap(),
            )));
        }
        if let Some(Token::Char(c)) = self.accept(|t| matches!(t, Token::Char(_)))? {
            let c = snailquote::unescape(&format!("\"{}\"", &c[1..c.len()]))
                .map_err(|_| self.error(ParseErrorKind::BadCharacter))?;

            let c = c.chars().next().unwrap();
            return Ok(Some(Literal::Int(c as i64)));
        }
        if self.accept(Token::Colon)?.is_some() {
            let variant = self
                .require(IdentPattern)?
                .ok_or(self.error(ParseErrorKind::ExpectedIdent))?
                .to_string();
            let members = self.parse_struct_literal()?.unwrap_or_default();
            return Ok(Some(Literal::Enum { variant, members }));
        }
        Ok(None)
    }

    fn parse_struct_literal(&mut self) -> ParseResult<Vec<(String, Expression)>> {
        let mut members = vec![];
        if self.accept(Token::CurlyOpen)?.is_some() {
            let mut no_members = true;
            while let Some(arg) = self.parse_member()? {
                no_members = false;
                members.push(arg);
                if self.accept(Token::Comma)?.is_none() {
                    self.require(Token::CurlyClose)?;
                    break;
                }
            }
            if no_members {
                self.require(Token::CurlyClose)?;
            }
            return Ok(Some(members));
        }
        //// Tuple structs are difficult, because they look exactly like calls.
        //// I can't be bothered to implement them properly right now.
        // if self.accept(Token::ParenOpen)?.is_some() {
        //     let mut param_counter = 0;
        //     while let Some(param) = self.parse_expr()? {
        //         members.push((param_counter.to_string(), param));
        //         param_counter += 1;
        //         if self.accept(Token::Comma)?.is_none() {
        //             self.require(Token::ParenClose)?;
        //         }
        //     }
        //     if param_counter == 0 {
        //         self.require(Token::ParenClose)?;
        //     }
        //     return Ok(Some(members));
        // }
        Ok(None)
    }

    fn parse_int(&mut self) -> ParseResult<u64> {
        if let Some(n) = self.accept(|t| matches!(t, Token::Number(_)))? {
            let Token::Number(val) = n else {
                unreachable!()
            };
            return Ok(Some(val));
        }
        Ok(None)
    }

    fn parse_enum(&mut self) -> ParseResult<types::Declaration> {
        let start = self.current_span().start;
        if self.accept(Token::Enum)?.is_none() {
            return Ok(None);
        }

        let name = self
            .require(IdentPattern)?
            .ok_or(self.error(ParseErrorKind::ExpectedIdent))?
            .to_string();

        let mut variants = vec![];
        self.require(Token::CurlyOpen)?;

        while let Some(variant) = self.parse_struct_variant()? {
            variants.push(variant);
            self.require(Token::Comma)?;
        }

        self.require(Token::CurlyClose)?;
        let end = self.current_span().end;
        Ok(Some(
            DeclarationKind::Enum { name, variants }.with_span(start..end),
        ))
    }
}

pub fn parse_path<'a, P: AsRef<Path>>(path: P) -> (Program, Vec<ParseError>) {
    let path_str = path.as_ref().display().to_string();
    let contents = std::fs::read_to_string(path).unwrap();
    let source = Source {
        name: &path_str,
        contents: &contents,
    };
    parse(source)
}

pub fn parse(source: Source<'_>) -> (Program, Vec<ParseError>) {
    let lexer = Token::lexer(&source.contents);
    let tokens: Result<Vec<(Token<'_>, Span)>, ()> = lexer
        .spanned()
        .map(|(t, s)| match t {
            Ok(t) => Ok((t, s)),
            Err(e) => Err(e),
        })
        .collect();
    let mut parser = Parser::new(tokens.unwrap());
    let (program, errors) = parser.parse();
    (program, errors)
}
