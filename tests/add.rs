use meeval::eval;

#[test]
fn add() {
	assert_eq!(eval("1 + 2"), Ok(3));
}
