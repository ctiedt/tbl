mod token;
pub mod types;

use std::iter::TakeWhile;

use token::Token;

enum ParseError<'a> {
    Any,
    UnexpectedToken(Token<'a>),
}

type ParseResult<'a, T> = Option<Result<T, ParseError<'a>>>;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn current(&self) -> Token<'a> {
        self.tokens[self.cursor]
    }

    fn accept(&mut self, f: impl Fn(Token<'a>) -> bool) -> ParseResult<Token<'a>> {
        if f(self.current()) {
            self.cursor += 1;
            Some(Ok(self.tokens[self.cursor - 1]))
        } else {
            None
        }
    }

    fn accept_err(&mut self, f: impl Fn(Token<'a>) -> bool) -> ParseResult<Token<'a>> {
        let current = self.current();
        match self.accept(f) {
            Some(res) => Some(res),
            None => Some(Err(ParseError::UnexpectedToken(current))),
        }
    }

    fn accept_exact(&mut self, t: Token<'a>) -> ParseResult<Token<'a>> {
        let current = self.current();
        if current == t {
            self.cursor += 1;
            Some(Ok(current))
        } else {
            None
        }
    }

    fn parse_task(&mut self) -> ParseResult<types::Declaration> {
        let mut params = vec![];
        self.accept(|t| t == Token::Keyword("task"))?;
        let name = self.accept(|t| matches!(t, Token::Ident(_)))?;
        self.accept_err(|t| matches!(t, Token::ParenOpen))?;
        loop {
            if matches!(self.accept_exact(Token::ParenClose), Some(Ok(_))) {
                break;
            }
            let tok = self.accept_err(|t| matches!(t, Token::Ident(_)))?;
            self.accept_exact(Token::Colon);
            let ty = self.accept_err(|t| matches!(t, Token::Ident(_)))?;
        }
        todo!()
    }

    fn parse_ty(&mut self) -> ParseResult<types::Type> {}
}
