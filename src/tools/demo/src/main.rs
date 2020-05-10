use ir::builder::*;
use ir::codegen::*;
use ir::grammar::*;
use ir::types::*;

fn main() {
	let term = Builder::new()
		.binary_op("c", Native::Bool, Operator::Eq, "a", "b")
		.ret("c")
		.build();
	let foo = Definition::new(
		"foo",
		vec![("a", Native::Int.into()), ("b", Native::Int.into())],
		Native::Bool,
		term,
	);

	let term = Builder::new()
		.binary_op("z", Native::Int, Operator::Add, "x", "y")
		.ret("z")
		.build();
	let sum = Definition::new(
		"sum",
		vec![("x", Native::Int.into()), ("y", Native::Int.into())],
		Native::Int,
		term,
	);

	let term = Builder::new()
		.bind("a", Native::Int, Literal::Int(1))
		.bind("b", Native::Int, Literal::Int(2))
		.call("result", Native::Int, "sum", vec!["a", "b"])
		.ret("result")
		.build();
	let main = Definition::new("main", Vec::<(Variable, Type)>::new(), Native::Int, term);

	let program = Program::new(vec![main, sum, foo]);
	println!("==  IR  ==\n\n{}\n", program);

	let context = Context::create();
	let module = context.create_module("demo");
	let cg = CodeGen::new(&context, module);
	cg.compile_program(&program);
	println!("== LLVM ==\n\n{}", cg.module.print_to_string().to_string());

	// cg.module.write_bitcode_to_path(std::path::Path::new("./demo.bc"));
}
