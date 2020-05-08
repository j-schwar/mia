use ir::grammar::*;
use ir::types::*;

fn main() {
	let int = Type::Native(Native::Int);

	let term = Term::Return(Variable::new("z"));
	let term = Term::Bind {
		variable: Variable::new("z"),
		type_annotation: int.clone(),
		expression: Expression::Operation {
			operator: Operator::Add,
			operands: vec![Variable::new("x"), Variable::new("y")],
		},
		continuation: Box::new(term),
	};
	let def = Definition::new(
		"sum",
		vec![("x", int.clone()), ("y", int.clone())],
		int.clone(),
		term,
	);
	let program = Program::new(vec![def]);

	println!("{}", program);

	let int = Type::new_native(Native::Int);
	let pair = Type::new_product(vec![("left", int.clone()), ("right", int.clone())]);
	let list = Type::new_sum(vec![
		("Cons", Type::new_typename("cons")),
		("Nil", Type::Unit),
	]);

	println!();
	println!("{}", int);
	println!("{}", pair);
	println!("{}", list);
}
