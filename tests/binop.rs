use meeval::eval;

#[test]
fn add() {
	assert_eq!(eval("1 + 2"), Ok(3));
}

#[test]
fn sub() {
	assert_eq!(eval("1 - 2"), Ok(-1));
}

#[test]
fn mul() {
	assert_eq!(eval("1 * 2"), Ok(2));
}

#[test]
fn div() {
	assert_eq!(eval("1 / 2"), Ok(0));
}
