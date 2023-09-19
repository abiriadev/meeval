use meeval::eval;

#[test]
#[should_panic] // TODO: should show descriptive error message someday later
fn divide_by_zero() { let _ = eval("123 / 0"); }
