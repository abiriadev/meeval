use std::io::stdin;

use meeval::eval;

fn main() {
	stdin()
		.lines()
		.filter_map(Result::ok)
		.for_each(|line| match eval(line.trim()) {
			Ok(r) => println!("{r}"),
			Err(e) => println!("err: {e}"),
		});
}
