use meeval::eval;

#[test]
fn add_multiple_times() {
	assert_eq!(eval("5 + 2 + 10 + 7"), Ok(24));
}

#[test]
fn sub_multiple_times() {
	assert_eq!(eval("50 - 40 - 5 - 8"), Ok(-3));
}

#[test]
fn mul_multiple_times() {
	assert_eq!(eval("2 * 3 * 4 * 5"), Ok(120));
}

#[test]
fn div_multiple_times() {
	assert_eq!(eval("24 / 2 / 3 / 2"), Ok(2));
}
