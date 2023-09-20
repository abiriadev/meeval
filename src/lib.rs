use std::{
	iter::Enumerate,
	mem::discriminant,
	num::NonZeroUsize,
	ops::{Index, RangeFrom},
	slice::Iter,
};

use nom::{
	branch::alt,
	bytes::complete::{tag, take},
	character::complete::{char, i32, multispace0},
	combinator::{all_consuming, value, verify},
	error::{Error as NomError, ErrorKind, ParseError},
	multi::{many0, many1},
	sequence::{delimited, pair, separated_pair},
	AsChar, Compare, CompareResult, Finish, IResult, InputIter, InputLength,
	InputTake, InputTakeAtPosition, Needed, Parser, Slice,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
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

#[derive(Clone, Debug)]
struct TokenStream<'a>(&'a [Token]);

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

#[derive(Debug, PartialEq, Eq)]
enum Expr {
	Literal(i32),
	Exp(Box<Expr>, Box<Expr>),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
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

fn lex_expr(i: &str) -> IResult<&str, Vec<Token>> { many0(ws(lex_token))(i) }

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

fn ws<I, O, E, P>(p: P) -> impl Parser<I, O, E>
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
		if i.input_len() > 0 {
			if discriminant(&i.0[0]) == discriminant(&tok) {
				Ok((i.slice(1..), tok.clone()))
			} else {
				Err(nom::Err::Error(E::from_error_kind(
					i,
					ErrorKind::Tag,
				)))
			}
		} else {
			Err(nom::Err::Incomplete(Needed::Size(
				unsafe { NonZeroUsize::new_unchecked(1) },
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
			alt((
				tag(Token::Asterisk).map(|t: TokenStream| t.0.get(0).unwrap()),
				tag(Token::Slash).map(|t: TokenStream| t.0.get(0).unwrap()),
			)),
			parse_expr_binop_exp,
			|left, (op, right)| match op {
				Token::Asterisk => Expr::Mul(Box::new(left), Box::new(right)),
				Token::Slash => Expr::Div(Box::new(left), Box::new(right)),
				_ => unreachable!(),
			},
		),
		parse_expr_binop_exp,
	))(i)
}

fn parse_expr_binop_add(i: TokenStream) -> IResult<TokenStream, Expr> {
	alt((
		left_associative(
			alt((
				tag(Token::Plus).map(|t: TokenStream| t.0.get(0).unwrap()),
				tag(Token::Minus).map(|t: TokenStream| t.0.get(0).unwrap()),
			)),
			parse_expr_binop_mul,
			|left, (op, right)| match op {
				Token::Plus => Expr::Add(Box::new(left), Box::new(right)),
				Token::Minus => Expr::Sub(Box::new(left), Box::new(right)),
				_ => unreachable!(),
			},
		),
		parse_expr_binop_mul,
	))(i)
}

fn parse_expr(i: TokenStream) -> IResult<TokenStream, Expr> {
	parse_expr_binop_add(i)
}

fn eval_ast(ast: Expr) -> i32 {
	match ast {
		Expr::Literal(i) => i,
		// TODO: support native floating points someday
		Expr::Exp(a, b) => (eval_ast(*a) as f64).powi(eval_ast(*b)) as i32,
		Expr::Add(a, b) => eval_ast(*a) + eval_ast(*b),
		Expr::Sub(a, b) => eval_ast(*a) - eval_ast(*b),
		Expr::Mul(a, b) => eval_ast(*a) * eval_ast(*b),
		Expr::Div(a, b) => eval_ast(*a) / eval_ast(*b),
	}
}

pub fn eval(expression: &str) -> Result<i32, NomError<&str>> {
	let ts = lex_expr(expression).unwrap().1;

	let x = Ok(eval_ast(
		all_consuming(parse_expr)(TokenStream(&ts))
			.finish()
			.unwrap()
			.1,
	));
	x
}

#[test]
fn parse_i32() {
	let (r, ast) = lex_literal("1323").finish().unwrap();

	assert_eq!(ast, Token::Literal(1323));
	assert_eq!(r, "")
}

// #[test]
// fn parse_add() {
// 	let (r, ast) = parse_expr("1 + 2").finish().unwrap();
//
// 	assert_eq!(
// 		ast,
// 		Expr::Add(
// 			Box::new(Expr::Literal(1)),
// 			Box::new(Expr::Literal(2))
// 		)
// 	);
// 	assert_eq!(r, "")
// }

#[test]
fn parse_plusminus() {
	assert_eq!(
		PlusMinus::parse("123"),
		Err(nom::Err::Error(()))
	);

	assert_eq!(
		PlusMinus::parse::<()>("+123"),
		Ok(("123", PlusMinus::Plus))
	);

	assert_eq!(
		PlusMinus::parse::<()>("-123"),
		Ok(("123", PlusMinus::Minus))
	);
}

#[test]
fn parse_asteriskslash() {
	assert_eq!(
		AsteriskSlash::parse("123"),
		Err(nom::Err::Error(()))
	);

	assert_eq!(
		AsteriskSlash::parse::<()>("*123"),
		Ok(("123", AsteriskSlash::Asterisk))
	);

	assert_eq!(
		AsteriskSlash::parse::<()>("/123"),
		Ok(("123", AsteriskSlash::Slash))
	);
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
