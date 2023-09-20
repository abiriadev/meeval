use std::mem::discriminant;

use nom::{
	branch::alt,
	bytes::complete::{tag, take},
	character::complete::multispace0,
	combinator::{value, verify},
	error::{ErrorKind, ParseError},
	multi::many1,
	sequence::{delimited, pair, separated_pair},
	AsChar, IResult, InputLength, InputTakeAtPosition, Parser, Slice,
};

use crate::lexer::{Token, TokenStream};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
	Literal(i32),
	Exp(Box<Expr>, Box<Expr>),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PlusMinus {
	Plus,
	Minus,
}

impl PlusMinus {
	fn parse<'a, E>(i: TokenStream<'a>) -> IResult<TokenStream<'a>, Self, E>
	where E: ParseError<TokenStream<'a>> {
		alt((
			value(Self::Plus, parse_token(Token::Plus)),
			value(Self::Minus, parse_token(Token::Minus)),
		))(i)
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AsteriskSlash {
	Asterisk,
	Slash,
}

impl AsteriskSlash {
	fn parse<'a, E>(i: TokenStream<'a>) -> IResult<TokenStream<'a>, Self, E>
	where E: ParseError<TokenStream<'a>> {
		alt((
			value(
				Self::Asterisk,
				parse_token(Token::Asterisk),
			),
			value(Self::Slash, parse_token(Token::Slash)),
		))(i)
	}
}

fn left_associative<I, O, E, P, O2, P2, F>(
	op: P2,
	p: P,
	f: F,
) -> impl Parser<I, O, E>
where
	I: Clone + InputLength,
	E: ParseError<I>,
	P: Parser<I, O, E> + Copy,
	P2: Parser<I, O2, E>,
	F: Fn(O, (O2, O)) -> O + Copy,
{
	pair(p, many1(pair(op, p))).map(move |(a, r)| r.into_iter().fold(a, f))
}

pub fn ws<I, O, E, P>(p: P) -> impl Parser<I, O, E>
where
	P: Parser<I, O, E>,
	I: InputTakeAtPosition,
	<I as InputTakeAtPosition>::Item: AsChar + Clone,
	E: ParseError<I>, {
	delimited(multispace0, p, multispace0)
}

fn parse_token<'a, E>(tok: Token) -> impl Parser<TokenStream<'a>, Token, E>
where E: ParseError<TokenStream<'a>> {
	move |i: TokenStream<'a>| {
		if i.input_len() > 0 && discriminant(&i.0[0]) == discriminant(&tok) {
			Ok((i.slice(1..), tok.clone()))
		} else {
			Err(nom::Err::Error(E::from_error_kind(
				i,
				ErrorKind::Tag,
			)))
		}
	}
}

fn parse_literal(i: TokenStream) -> IResult<TokenStream, Expr> {
	verify(take(1usize), |t: &TokenStream| {
		matches!(t.0.get(0).unwrap(), Token::Literal(..))
	})
	.map(
		|ts: TokenStream| match ts.0.get(0).unwrap() {
			Token::Literal(l) => Expr::Literal(*l),
			_ => unreachable!(),
		},
	)
	.parse(i)
}

fn parse_expr_atom(i: TokenStream) -> IResult<TokenStream, Expr> {
	alt((
		// parse_number_i32,
		parse_literal,
		delimited(
			tag(Token::LParen),
			parse_expr,
			tag(Token::RParen),
		),
	))(i)
}

fn parse_expr_binop_exp(i: TokenStream) -> IResult<TokenStream, Expr> {
	alt((
		separated_pair(
			parse_expr_atom,
			tag(Token::Caret),
			parse_expr_binop_exp,
		)
		.map(|(left, right)| Expr::Exp(Box::new(left), Box::new(right))),
		parse_expr_atom,
	))(i)
}

fn parse_expr_binop_mul(i: TokenStream) -> IResult<TokenStream, Expr> {
	alt((
		left_associative(
			AsteriskSlash::parse,
			parse_expr_binop_exp,
			|left, (op, right)| match op {
				AsteriskSlash::Asterisk =>
					Expr::Mul(Box::new(left), Box::new(right)),
				AsteriskSlash::Slash =>
					Expr::Div(Box::new(left), Box::new(right)),
			},
		),
		parse_expr_binop_exp,
	))(i)
}

fn parse_expr_binop_add(i: TokenStream) -> IResult<TokenStream, Expr> {
	alt((
		left_associative(
			PlusMinus::parse,
			parse_expr_binop_mul,
			|left, (op, right)| match op {
				PlusMinus::Plus => Expr::Add(Box::new(left), Box::new(right)),
				PlusMinus::Minus => Expr::Sub(Box::new(left), Box::new(right)),
			},
		),
		parse_expr_binop_mul,
	))(i)
}

pub fn parse_expr(i: TokenStream) -> IResult<TokenStream, Expr> {
	parse_expr_binop_add(i)
}

#[cfg(test)]
mod test {
	use nom::Finish;

	use crate::{
		lexer::Token,
		parser::{parse_expr, AsteriskSlash, Expr, PlusMinus},
	};

	#[test]
	fn parse_add() {
		let (r, ast) = parse_expr(
			[Token::Literal(1), Token::Plus, Token::Literal(2)]
				.as_slice()
				.into(),
		)
		.finish()
		.unwrap();

		assert_eq!(
			ast,
			Expr::Add(
				Box::new(Expr::Literal(1)),
				Box::new(Expr::Literal(2))
			)
		);
		assert_eq!(r, [].as_slice().into())
	}

	#[test]
	fn parse_plusminus() {
		assert_eq!(
			PlusMinus::parse([Token::Literal(123)].as_slice().into()),
			Err(nom::Err::Error(()))
		);

		assert_eq!(
			PlusMinus::parse::<()>([Token::Plus].as_slice().into()),
			Ok(([].as_slice().into(), PlusMinus::Plus))
		);

		assert_eq!(
			PlusMinus::parse::<()>([Token::Minus].as_slice().into()),
			Ok(([].as_slice().into(), PlusMinus::Minus))
		);
	}

	#[test]
	fn parse_asteriskslash() {
		assert_eq!(
			AsteriskSlash::parse([Token::Literal(123)].as_slice().into()),
			Err(nom::Err::Error(()))
		);

		assert_eq!(
			AsteriskSlash::parse::<()>([Token::Asterisk].as_slice().into()),
			Ok((
				[].as_slice().into(),
				AsteriskSlash::Asterisk
			))
		);

		assert_eq!(
			AsteriskSlash::parse::<()>([Token::Slash].as_slice().into()),
			Ok((
				[].as_slice().into(),
				AsteriskSlash::Slash
			))
		);
	}
}
