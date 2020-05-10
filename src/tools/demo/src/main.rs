use ir::builder::*;
use ir::codegen::*;
use ir::grammar::*;
use ir::types::*;

/// Traditional (inefficient) fibonacci function implemented in IR code.
///
/// Demonstrates the boolean match (if/else) term.
fn fib() -> Definition {
	let body = Builder::new()
		.bind("1", Native::Int, Literal::Int(1))
		.bind("2", Native::Int, Literal::Int(2))
		.binary_op("cond", Native::Bool, Operator::Le, "n", "1")
		.if_else(
			"cond",
			Builder::new().ret("n").build(),
			Builder::new()
				.binary_op("sub_1", Native::Int, Operator::Sub, "n", "1")
				.call("rec_1", Native::Int, "fib", vec!["sub_1"])
				.binary_op("sub_2", Native::Int, Operator::Sub, "n", "2")
				.call("rec_2", Native::Int, "fib", vec!["sub_2"])
				.binary_op("result", Native::Int, Operator::Add, "rec_1", "rec_2")
				.ret("result")
				.build(),
		)
		.build();

	Definition::new("fib", vec![("n", Native::Int.into())], Native::Int, body)
}

/// Simple sum function implemented in IR code.
fn sum() -> Definition {
	let body = Builder::new()
		.binary_op("z", Native::Int, Operator::Add, "x", "y")
		.ret("z")
		.build();

	Definition::new(
		"sum",
		vec![("x", Native::Int.into()), ("y", Native::Int.into())],
		Native::Int,
		body,
	)
}

/// The `main` function implemented in IR code.
fn ir_main() -> Definition {
	let body = Builder::new()
		.bind("a", Native::Int, Literal::Int(1))
		.bind("b", Native::Int, Literal::Int(2))
		.call("result", Native::Int, "sum", vec!["a", "b"])
		.ret("result")
		.build();

	Definition::new("main", Vec::<(Variable, Type)>::new(), Native::Int, body)
}

fn main() {
	let program = Program::new(vec![ir_main(), sum(), fib()]);
	println!("==  IR  ==\n\n{}\n", program);

	let context = Context::create();
	let module = context.create_module("demo");
	let cg = CodeGen::new(&context, module);
	cg.compile_program(&program);
	println!("== LLVM ==\n\n{}", cg.module.print_to_string().to_string());

	match cg.module.verify() {
		Ok(()) => println!("== compiled program is valid"),
		Err(e) => println!("== llvm error:\n== {}", e.to_string()),
	}

	// cg.module.write_bitcode_to_path(std::path::Path::new("./demo.bc"));
}
