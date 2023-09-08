use meeval::eval;

#[test]
fn add_and_mul() {
	assert_eq!(eval("5 + 2 * 10"), Ok(25));
}

#[test]
fn sub_and_div() {
	assert_eq!(eval("1 - 10 / 2"), Ok(-4));
}

#[test]
fn mul_and_add() {
	assert_eq!(eval("20 * 5 + 2"), Ok(102));
}

#[test]
fn div_and_add() {
	assert_eq!(eval("5 / 2 + 10"), Ok(12));
}
