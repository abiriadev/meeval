use std::{
	iter::Enumerate,
	ops::{Index, RangeFrom},
	slice::Iter,
};

use nom::{
	branch::alt,
	character::complete::{char, i32},
	combinator::value,
	multi::many0,
	Compare, CompareResult, Finish, IResult, InputIter, InputLength, InputTake,
	Needed, Parser, Slice,
};

use crate::parser::ws;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
	Literal(i32),
	Plus,
	Minus,
	Asterisk,
	Slash,
	Caret,
	LParen,
	RParen,
}

impl InputLength for Token {
	fn input_len(&self) -> usize { 1 }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenStream<'a>(pub &'a [Token]);

impl AsRef<[Token]> for TokenStream<'_> {
	fn as_ref(&self) -> &[Token] { self.0 }
}

impl<'a> From<&'a [Token]> for TokenStream<'a> {
	fn from(value: &'a [Token]) -> Self { Self(value) }
}

impl InputLength for TokenStream<'_> {
	fn input_len(&self) -> usize { self.0.len() }
}

impl Slice<RangeFrom<usize>> for TokenStream<'_> {
	fn slice(&self, range: RangeFrom<usize>) -> Self {
		self.0.index(range).into()
	}
}

impl<'a> InputIter for TokenStream<'a> {
	type Item = &'a Token;
	type Iter = Enumerate<Self::IterElem>;
	type IterElem = Iter<'a, Token>;

	fn iter_indices(&self) -> Self::Iter { self.0.iter().enumerate() }

	fn iter_elements(&self) -> Self::IterElem { self.0.iter() }

	fn position<P>(&self, predicate: P) -> Option<usize>
	where P: Fn(Self::Item) -> bool {
		self.iter_elements().position(predicate)
	}

	fn slice_index(&self, count: usize) -> Result<usize, Needed> {
		if self.0.len() >= count {
			Ok(count)
		} else {
			Err(Needed::new(count - self.0.len()))
		}
	}
}

impl InputTake for TokenStream<'_> {
	fn take(&self, count: usize) -> Self { self.0[0..count].into() }

	fn take_split(&self, count: usize) -> (Self, Self) {
		let (l, r) = self.0.split_at(count);
		(r.into(), l.into())
	}
}

impl Compare<Token> for TokenStream<'_> {
	fn compare(&self, t: Token) -> CompareResult {
		self.0
			.get(0)
			.map(|f| {
				if *f == t {
					CompareResult::Ok
				} else {
					CompareResult::Error
				}
			})
			.unwrap_or(CompareResult::Incomplete)
	}

	fn compare_no_case(&self, t: Token) -> CompareResult { self.compare(t) }
}

fn lex_literal(i: &str) -> IResult<&str, Token> {
	i32.map(Token::Literal).parse(i)
}

fn lex_plus(i: &str) -> IResult<&str, Token> {
	value(Token::Plus, char('+')).parse(i)
}

fn lex_minus(i: &str) -> IResult<&str, Token> {
	value(Token::Minus, char('-')).parse(i)
}

fn lex_asterisk(i: &str) -> IResult<&str, Token> {
	value(Token::Asterisk, char('*')).parse(i)
}

fn lex_slash(i: &str) -> IResult<&str, Token> {
	value(Token::Slash, char('/')).parse(i)
}

fn lex_caret(i: &str) -> IResult<&str, Token> {
	value(Token::Caret, char('^')).parse(i)
}

fn lex_lparen(i: &str) -> IResult<&str, Token> {
	value(Token::LParen, char('(')).parse(i)
}

fn lex_rparen(i: &str) -> IResult<&str, Token> {
	value(Token::RParen, char(')')).parse(i)
}

fn lex_token(i: &str) -> IResult<&str, Token> {
	alt((
		lex_literal,
		lex_plus,
		lex_minus,
		lex_asterisk,
		lex_slash,
		lex_caret,
		lex_lparen,
		lex_rparen,
	))(i)
}

pub fn lex_expr(i: &str) -> IResult<&str, Vec<Token>> {
	many0(ws(lex_token))(i)
}

#[test]
fn parse_i32() {
	let (r, ast) = lex_literal("1323").finish().unwrap();

	assert_eq!(ast, Token::Literal(1323));
	assert_eq!(r, "")
}

#[test]
fn lex_test() {
	assert_eq!(
		lex_expr("1 + 2"),
		Ok(("", vec![
			Token::Literal(1),
			Token::Plus,
			Token::Literal(2),
		]))
	);
}
