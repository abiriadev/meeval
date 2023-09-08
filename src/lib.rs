use nom::{
	branch::alt,
	character::complete::{char, i32, multispace0},
	combinator::all_consuming,
	error::{Error as NomError, ParseError},
	sequence::{delimited, separated_pair},
	AsChar, Finish, IResult, InputTakeAtPosition, Parser,
};

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

fn parse_expr(i: &str) -> IResult<&str, Expr> {
	alt((
		parse_number_i32,
		separated_pair(parse_expr, ws(char('+')), parse_expr)
			.map(|(ex1, ex2)| Expr::Add(Box::new(ex1), Box::new(ex2))),
		separated_pair(parse_expr, ws(char('-')), parse_expr)
			.map(|(ex1, ex2)| Expr::Sub(Box::new(ex1), Box::new(ex2))),
		separated_pair(parse_expr, ws(char('*')), parse_expr)
			.map(|(ex1, ex2)| Expr::Mul(Box::new(ex1), Box::new(ex2))),
		separated_pair(parse_expr, ws(char('/')), parse_expr)
			.map(|(ex1, ex2)| Expr::Div(Box::new(ex1), Box::new(ex2))),
	))(i)
}

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
