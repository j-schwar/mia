use ir::codegen::*;
use ir::grammar::*;
use ir::types::*;

fn main() {
	let term = Term::new_return("z");
	let term = Term::new_bind(
		"z",
		Native::Int,
		Expression::new_binary(Operator::Add, "x", "y"),
		term,
	);

	let int = Type::Native(Native::Int);
	let sum = Definition::new(
		"sum",
		vec![("x", int.clone()), ("y", int.clone())],
		Native::Int,
		term,
	);

	let term = Term::new_return("result");
	let term = Term::new_bind(
		"result",
		Native::Int,
		Expression::new_call("sum", vec!["a", "b"]),
		term,
	);
	let term = Term::new_bind("b", Native::Int, Literal::Int(2), term);
	let term = Term::new_bind("a", Native::Int, Literal::Int(1), term);
	let main = Definition::new("main", Vec::<(Variable, Type)>::new(), Native::Int, term);

	let program = Program::new(vec![main, sum]);
	println!("==  IR  ==\n\n{}\n", program);

	let context = Context::create();
	let module = context.create_module("demo");
	let cg = CodeGen::new(&context, module);
	cg.compile_program(&program);
	println!("== LLVM ==\n\n{}", cg.module.print_to_string().to_string());

	// cg.module.write_bitcode_to_path(std::path::Path::new("./demo.bc"));
}
