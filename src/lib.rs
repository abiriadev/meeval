use lexer::{lex_expr, TokenStream};
use nom::{combinator::all_consuming, error::Error as NomError, Finish};
use parser::{parse_expr, Expr};

mod lexer;
mod parser;

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
