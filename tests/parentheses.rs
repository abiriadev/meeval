use meeval::eval;

#[test]
fn add_before_mul() {
	assert_eq!(eval("5 * (10 + 2)"), Ok(60));
}

#[test]
fn mul_two_parens() {
	assert_eq!(eval("(6 - 3) * (23 - 42)"), Ok(-57));
}

#[test]
fn nested() {
	assert_eq!(
		eval("((34) + ((89) + ((11) + ((43) + (89)))))"),
		Ok(266)
	);
}

#[test]
#[ignore]
fn nested_unary() {
	// TODO: taks too much time to parse. improve needed
	assert_eq!(eval("((((((((((1))))))))))"), Ok(1));
}
