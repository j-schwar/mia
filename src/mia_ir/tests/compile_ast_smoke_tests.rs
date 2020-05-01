//! Smoke tests which just ensure that some source code compiles (or fails to
//! compile depending on the test). They don't ensure that the generated IR
//! code is correct.

extern crate mia_ir;

use mia_ir::ir;
use mia_syntax as syntax;

/// Parses `source` and invokes the `compile_ast` function on the result.
///
/// ## Panics
///
/// Panics with message "failed to parse" if unable to parse `source` or with
/// the error message returned from the `compile_ast` function if compilation
/// failed.
fn invoke_compiler(source: &str) {
	let (_, ast) = syntax::parser::parse_complete(source).expect("failed to parse");
	match ir::compile_ast(ast) {
		Ok(_) => (),
		Err(e) => panic!("{}", e),
	}
}

#[test]
fn test_sum_function() {
	let source = "fun sum(x: i32, y: i32) -> i32 = x + y;";
	invoke_compiler(source);
}

#[test]
#[should_panic(expected = "use of undefined variable: 'z'")]
fn test_sum_function_use_of_undefined_variable() {
	let source = "fun sum(x: i32, y: i32) -> i32 = x + z;";
	invoke_compiler(source);
}

#[test]
#[should_panic(expected = "use of undefined type: 'i128'")]
fn test_sum_function_use_of_undefined_type() {
	let source = "fun sum(x, y) -> i128 = x + y;";
	invoke_compiler(source);
}

#[test]
#[should_panic(expected = "redefinition of variable: 'x'")]
fn test_variable_redefinition() {
	let source = "fun foo(x, y) {
		let a = x;
		let b = y;
		let x = x + y;
	}";
	invoke_compiler(source);
}

#[test]
fn test_integer_literal() {
	let source = "fun add_1(x: i32) -> i32 = x + 1;";
	invoke_compiler(source);
}
