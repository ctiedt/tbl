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
use types::{
    BinaryOperator, Declaration, Expression, ExternTaskParams, Literal, Program, Type,
    UnaryOperator,
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

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError::new(self.current_span(), kind)
    }

    pub fn parse(&mut self) -> Result<Program, Box<Report>> {
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

            if let Some(error) = error {
                errors.push(error);
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

    fn parse_directive(&mut self) -> ParseResult<types::Declaration> {
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

        self.require(Token::Semicolon)?;

        Ok(Some(Declaration::Directive { name, args }))
    }

    fn parse_global(&mut self) -> ParseResult<types::Declaration> {
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

        Ok(Some(Declaration::Global { name, type_, value }))
    }

    fn parse_struct(&mut self) -> ParseResult<types::Declaration> {
        if self.accept(Token::Struct)?.is_none() {
            return Ok(None);
        }

        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
            .ok_or(self.error(ParseErrorKind::ExpectedExpression))?
            .to_string();

        let mut members = vec![];
        self.require(Token::CurlyOpen)?;
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

        Ok(Some(Declaration::Struct { name, members }))
    }

    fn parse_extern_task(&mut self) -> ParseResult<types::Declaration> {
        let mut returns = None;
        if self.accept(Token::Extern)?.is_none() {
            return Ok(None);
        }
        self.require(Token::Task)
            .error(ParseErrorKind::UnknownKeyword)?;
        let name = self
            .accept(|t| matches!(t, Token::Ident(_)))?
            .unwrap()
            .to_string();

        let params = self
            .parse_external_params()?
            .ok_or(self.error(ParseErrorKind::BadParams))?;

        if self.accept(Token::Arrow)?.is_some() {
            returns.replace(self.parse_ty()?.unwrap());
        }

        self.require(Token::Semicolon)?;

        Ok(Some(Declaration::ExternTask {
            name,
            params,
            returns,
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
        if let Some(res) = self.accept(|t| matches!(t, Token::Ident(_)))? {
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
        if self.accept(Token::Loop)?.is_some() {
            self.require(Token::CurlyOpen)?;
            let mut body = vec![];
            while let Some(stmt) = self.parse_stmt()? {
                body.push(stmt);
            }
            self.require(Token::CurlyClose)?;
            return Ok(Some(Statement::Loop { body }));
        }
        if self.accept(Token::Return)?.is_some() {
            let ret_value = self.parse_expr()?;
            self.require(Token::Semicolon)?;
            return Ok(Some(Statement::Return(ret_value)));
        }
        if self.accept(Token::Exit)?.is_some() {
            self.require(Token::Semicolon)?;
            return Ok(Some(Statement::Exit));
        }
        if self.accept(Token::Schedule)?.is_some() {
            let task = self
                .require(|t| matches!(t, Token::Ident(_)))?
                .ok_or(self.error(ParseErrorKind::Any))?
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
            return Ok(Some(Statement::Schedule { task, args }));
        }
        if let Some(expr) = self.parse_expr()? {
            if self.accept(Token::Semicolon)?.is_some() {
                return Ok(Some(Statement::Expression(expr)));
            }
            if self
                .accept(Token::Equals)
                .error(ParseErrorKind::ExpectedToken(Token::Equals))?
                .is_some()
            {
                let rhs = self.parse_expr().expected_expr()?.unwrap();
                self.require(Token::Semicolon)?;
                return Ok(Some(Statement::Assign {
                    location: expr,
                    value: rhs,
                }));
            }
        }
        Ok(None)
    }

    fn parse_expr(&mut self) -> ParseResult<Expression> {
        if let Some(left) = self.parse_comparison()? {
            if let Some(op) = self.accept([Token::DoubleEqual, Token::Unequal].as_slice())? {
                let operator = match op {
                    Token::DoubleEqual => BinaryOperator::Equal,
                    Token::Unequal => BinaryOperator::Unequal,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_comparison()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                Ok(Some(Expression::BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
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
                    .parse_term()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                Ok(Some(Expression::BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_term(&mut self) -> ParseResult<Expression> {
        if let Some(left) = self.parse_factor()? {
            if let Some(op) = self.accept([Token::Plus, Token::Minus].as_slice())? {
                let operator = match op {
                    Token::Plus => BinaryOperator::Add,
                    Token::Minus => BinaryOperator::Subtract,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_factor()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                Ok(Some(Expression::BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_factor(&mut self) -> ParseResult<Expression> {
        if let Some(left) = self.parse_unary()? {
            if let Some(op) = self.accept([Token::Star, Token::Slash].as_slice())? {
                let operator = match op {
                    Token::Star => BinaryOperator::Multiply,
                    Token::Slash => BinaryOperator::Divide,
                    _ => unreachable!(),
                };
                let right = self
                    .parse_unary()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                Ok(Some(Expression::BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }))
            } else {
                Ok(Some(left))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_unary(&mut self) -> ParseResult<Expression> {
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
            Ok(Some(Expression::UnaryOperation { value, operator }))
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> ParseResult<Expression> {
        if let Some(value) = self.parse_primary()? {
            if self.accept(Token::Period)?.is_some() {
                let member = self
                    .require(|t| matches!(t, Token::Ident(_)))?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?
                    .to_string();
                return Ok(Some(Expression::StructAccess {
                    value: Box::new(value),
                    member,
                }));
            }
            if self.accept(Token::BracketOpen)?.is_some() {
                let index = self
                    .parse_expr()?
                    .ok_or(self.error(ParseErrorKind::ExpectedExpression))?;
                self.require(Token::BracketClose)?;
                return Ok(Some(Expression::Index {
                    value: Box::new(value),
                    at: Box::new(index),
                }));
            }
            if self.accept(Token::As)?.is_some() {
                let to = self
                    .parse_ty()?
                    .ok_or(self.error(ParseErrorKind::BadType))?;
                return Ok(Some(Expression::Cast {
                    value: Box::new(value),
                    to,
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
                        break;
                    }
                }
                if no_args {
                    self.require(Token::ParenClose)?;
                }

                return Ok(Some(Expression::Call {
                    task: Box::new(value),
                    args,
                }));
            }
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expression> {
        if let Some(value) = self.parse_value()? {
            return Ok(Some(value));
        }
        if self.accept(Token::Hash)?.is_some() {
            let value = self
                .parse_ty()?
                .ok_or(self.error(ParseErrorKind::BadType))?;
            return Ok(Some(Expression::SizeOf { value }));
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
        if let Some(lit) = self.parse_literal()? {
            return Ok(Some(Expression::Literal(lit)));
        }
        if let Some(n) = self.accept(|t| matches!(t, Token::Ident(_)))? {
            let Token::Ident(val) = n else { unreachable!() };
            return Ok(Some(Expression::Var(val.to_string())));
        }

        Ok(None)
    }

    fn parse_member(&mut self) -> ParseResult<(String, Expression)> {
        let name = self
            .require(|t| matches!(t, Token::Ident(_)))?
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
        if self.accept(Token::CurlyOpen)?.is_some() {
            let mut members = vec![];
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

            return Ok(Some(Literal::Struct(members)));
        }
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
        if let Some(t) = self.accept(|t| matches!(t, Token::String(_)))? {
            let Token::String(val) = t else {
                unreachable!()
            };

            return Ok(Some(Literal::String(
                snailquote::unescape(&val.to_string()).unwrap(),
            )));
        }
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

pub fn resolve_directives(program: Program) -> Program {
    let mut new_program = vec![];
    for decl in program.declarations {
        if let Declaration::Directive { name, args } = decl {
            match name.as_str() {
                "using" => {
                    let Literal::String(ref path) = args[0] else {
                        unreachable!()
                    };
                    let inner = resolve_directives(parse(path));
                    for decl in inner.declarations {
                        new_program.push(decl);
                    }
                }
                _ => {
                    unreachable!()
                }
            }
        } else {
            new_program.push(decl)
        }
    }
    Program {
        declarations: new_program,
    }
}
