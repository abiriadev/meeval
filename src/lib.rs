use nom::{
	branch::alt,
	character::complete::{char, i32, multispace0, one_of},
	combinator::{all_consuming, value},
	error::{Error as NomError, ParseError},
	multi::many1,
	sequence::{delimited, pair},
	AsChar, Finish, IResult, InputLength, InputTakeAtPosition, Parser,
};

#[derive(Debug, PartialEq, Eq)]
enum Expr {
	Literal(i32),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PlusMinus {
	Plus = '+' as isize,
	Minus = '-' as isize,
}

impl<'a, E> Parser<&'a str, Self, E> for PlusMinus
where E: ParseError<&'a str>
{
	fn parse(&mut self, input: &'a str) -> IResult<&'a str, Self, E> {
		alt((
			value(
				Self::Plus,
				char(Self::Plus as u8 as char),
			),
			value(
				Self::Minus,
				char(Self::Minus as u8 as char),
			),
		))(input)
	}
}

impl PlusMinus {
	fn parse<'a, E>(input: &'a str) -> IResult<&'a str, Self, E>
	where E: ParseError<&'a str> {
		Self::Plus.parse(input)
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TimesSlash {
	Times = '*' as isize,
	Slash = '/' as isize,
}

impl<'a, E> Parser<&'a str, Self, E> for TimesSlash
where E: ParseError<&'a str>
{
	fn parse(&mut self, input: &'a str) -> IResult<&'a str, Self, E> {
		alt((
			value(
				Self::Times,
				char(Self::Times as u8 as char),
			),
			value(
				Self::Slash,
				char(Self::Slash as u8 as char),
			),
		))(input)
	}
}

impl TimesSlash {
	fn parse<'a, E>(input: &'a str) -> IResult<&'a str, Self, E>
	where E: ParseError<&'a str> {
		Self::Times.parse(input)
	}
}

fn left_associative<I, O, E, P, O2, P2>(
	op: P2,
	p: P,
) -> impl Parser<I, (O, Vec<(O2, O)>), E>
where
	I: Clone + InputLength,
	E: ParseError<I>,
	P: Parser<I, O, E> + Copy,
	P2: Parser<I, O2, E>,
{
	pair(p, many1(pair(op, p)))
}

fn ws<I, O, E, P>(p: P) -> impl Parser<I, O, E>
where
	P: Parser<I, O, E>,
	I: InputTakeAtPosition,
	<I as InputTakeAtPosition>::Item: AsChar + Clone,
	E: ParseError<I>, {
	delimited(multispace0, p, multispace0)
}

fn parse_number_i32(i: &str) -> IResult<&str, Expr> {
	i32.map(|int| Expr::Literal(int))
		.parse(i)
}

fn parse_expr_atom(i: &str) -> IResult<&str, Expr> {
	alt((
		parse_number_i32,
		delimited(char('('), parse_expr, char(')')),
	))(i)
}

fn parse_expr_binop_mul(i: &str) -> IResult<&str, Expr> {
	alt((
		left_associative(ws(one_of("*/")), parse_expr_atom).map(
			|(first, rest)| {
				rest.into_iter()
					.fold(first, |left, (op, right)| match op {
						'*' => Expr::Mul(Box::new(left), Box::new(right)),
						'/' => Expr::Div(Box::new(left), Box::new(right)),
						_ => unreachable!(),
					})
			},
		),
		parse_expr_atom,
	))(i)
}

fn parse_expr_binop_add(i: &str) -> IResult<&str, Expr> {
	alt((
		left_associative(ws(one_of("+-")), parse_expr_binop_mul).map(
			|(first, rest)| {
				rest.into_iter()
					.fold(first, |left, (op, right)| match op {
						'+' => Expr::Add(Box::new(left), Box::new(right)),
						'-' => Expr::Sub(Box::new(left), Box::new(right)),
						_ => unreachable!(),
					})
			},
		),
		parse_expr_binop_mul,
	))(i)
}

fn parse_expr(i: &str) -> IResult<&str, Expr> { parse_expr_binop_add(i) }

fn eval_ast(ast: Expr) -> i32 {
	match ast {
		Expr::Literal(i) => i,
		Expr::Add(a, b) => eval_ast(*a) + eval_ast(*b),
		Expr::Sub(a, b) => eval_ast(*a) - eval_ast(*b),
		Expr::Mul(a, b) => eval_ast(*a) * eval_ast(*b),
		Expr::Div(a, b) => eval_ast(*a) / eval_ast(*b),
	}
}

pub fn eval(expression: &str) -> Result<i32, NomError<&str>> {
	Ok(eval_ast(
		all_consuming(ws(parse_expr))(expression)
			.finish()?
			.1,
	))
}

#[test]
fn parse_i32() {
	let (r, ast) = parse_number_i32("1323")
		.finish()
		.unwrap();

	assert_eq!(ast, Expr::Literal(1323));
	assert_eq!(r, "")
}

#[test]
fn parse_add() {
	let (r, ast) = parse_expr("1 + 2").finish().unwrap();

	assert_eq!(
		ast,
		Expr::Add(
			Box::new(Expr::Literal(1)),
			Box::new(Expr::Literal(2))
		)
	);
	assert_eq!(r, "")
}

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
