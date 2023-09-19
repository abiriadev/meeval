use meeval::eval;

fn main() {
	let res = eval("(10 + 3) * 2^5");

	let Ok(res) = res else {
		println!("error!");
		return;
	};

	println!("{res}");
}
