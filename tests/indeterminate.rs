use meeval::eval;

#[test]
#[should_panic] // TODO: should show descriptive error message someday later
fn divide_by_zero() { let _ = eval("123 / 0"); }

#[test]
// TODO: This answer is incorrect; it should raise an exception.
// However, `pow` in Rust's standard library behaves this way, thus we allow it for now.
fn zero_to_the_power_of_zero() { assert_eq!(eval("0 ^ 0"), Ok(1)) }
