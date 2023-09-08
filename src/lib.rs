use nom::{
	branch::alt,
	character::complete::{char, i32, multispace0},
	combinator::all_consuming,
	error::{Error as NomError, ParseError},
	sequence::{delimited, separated_pair},
	AsChar, Finish, IResult, InputTakeAtPosition, Parser,
};

#[derive(Debug, PartialEq, Eq)]
enum Expr {
	Literal(i32),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
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
	alt((parse_number_i32,))(i)
}

fn parse_expr_binop_mul(i: &str) -> IResult<&str, Expr> {
	alt((
		separated_pair(
			parse_expr_atom,
			ws(char('*')),
			parse_expr_atom,
		)
		.map(|(ex1, ex2)| Expr::Mul(Box::new(ex1), Box::new(ex2))),
		separated_pair(
			parse_expr_atom,
			ws(char('/')),
			parse_expr_atom,
		)
		.map(|(ex1, ex2)| Expr::Div(Box::new(ex1), Box::new(ex2))),
		parse_expr_atom,
	))(i)
}

fn parse_expr_binop_add(i: &str) -> IResult<&str, Expr> {
	alt((
		separated_pair(
			parse_expr_binop_mul,
			ws(char('+')),
			parse_expr_binop_mul,
		)
		.map(|(ex1, ex2)| Expr::Add(Box::new(ex1), Box::new(ex2))),
		separated_pair(
			parse_expr_binop_mul,
			ws(char('-')),
			parse_expr_binop_mul,
		)
		.map(|(ex1, ex2)| Expr::Sub(Box::new(ex1), Box::new(ex2))),
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
