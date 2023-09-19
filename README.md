# MEEval: Mathematical Expression EVALuator

## Example

<!-- AUTO-GENERATED-CONTENT:START (CODE:src=./examples/simple.rs) -->
<!-- The below code snippet is automatically added from ./examples/simple.rs -->
```rs
use meeval::eval;

fn main() {
	let res = eval("(10 + 3) * 2^5");

	let Ok(res) = res else {
		println!("error!");
		return;
	};

	println!("{res}");
}
```
<!-- AUTO-GENERATED-CONTENT:END -->

## Features

- [x] Elementary operations (`+`, `-`, `*`, `/`)
    - [x] Parantheses
- [ ] Extended operations
    - [x] Exponentials
    - [ ] Roots
    - [ ] Factorials
- [ ] First-class floating point support
    - [ ] Indeterminate form (e.g. `0^0`, `1/0`) aka `NaN`
    - [ ] Arbitrary precision fractions
    - [ ] Bigint
- [ ] Variables
    - [ ] Predefined constants
    - [ ] Coefficient syntax (.e.g `2ab`)
- [ ] REPL
    - [ ] Detailed error message
    - [ ] Syntax highlighting
    - [ ] Cache for recent evaluations
- [ ] Web support
