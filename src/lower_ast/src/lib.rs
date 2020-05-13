//! This crate implements the transformation from AST to IR code.
//!
//! The algorithm for generating IR code from an AST is total meaning that it
//! does not produce any errors. However, this does mean that the resultant
//! IR program is valid. It is up to the various IR passes to ensure type and
//! semantic correctness.

use ir::Type::Infer;
use std::marker::PhantomData;

/// Converts the syntactic notion of a `Program` (the AST) into a semantic one
/// (some IR code).
pub fn lower_ast(ast_program: &syntax::Program) -> ir::Program {
	ir::Program::new(
		ast_program
			.functions
			.iter()
			.map(FunctionCompiler::lower)
			.collect(),
	)
}

/// Lowers a single AST function into an IR one.
struct FunctionCompiler {
	var_generator: UniqueNameGenerator<ir::Variable>,
}

impl FunctionCompiler {
	/// Lowers an AST function into an IR one.
	pub fn lower(ast_function: &syntax::Function) -> ir::Definition {
		let name = ast_function.ident.text.clone();
		let parameters = ast_function
			.parameters
			.iter()
			.map(|(var, ty)| (ir::Variable::new(var.text.clone()), transform_typename(ty)))
			.collect::<Vec<(ir::Variable, ir::Type)>>();
		let return_type = ast_function
			.return_type
			.as_ref()
			.map(transform_typename)
			.unwrap_or(ir::Type::Unit);
		let term = FunctionCompiler::new().compile_function_body(&ast_function.body);
		return ir::Definition::new(name, parameters, return_type, term);
	}

	/// Constructs a new compiler instance.
	fn new() -> Self {
		FunctionCompiler {
			var_generator: UniqueNameGenerator::new(),
		}
	}

	/// Compiles a function body into an IR term.
	fn compile_function_body(&mut self, body: &syntax::FunctionBody) -> ir::Term {
		use syntax::FunctionBody::*;

		let mut builder = ir::Builder::new();
		match body {
			Block { statements, .. } => {
				self.compile_statements(&mut builder, statements.as_slice());
				builder.build()
			}

			Expr(expr) => {
				let expr = self.compile_expression(&mut builder, expr, None);
				builder.build_ret(expr);
				builder.build()
			}
		}
	}

	/// Compiles a sequence of statements into a sequence of terms.
	fn compile_statements(&mut self, builder: &mut ir::Builder, statements: &[syntax::Statement]) {
		use syntax::Statement::*;

		for i in 0..statements.len() {
			let statement = &statements[i];
			match statement {
				Expr(expr) => {
					let var = self.var_generator.generate();
					self.compile_expression(builder, expr, Some((var, ir::Type::Unit)));
				}

				Define {
					ident,
					typename,
					expr,
					..
				} => {
					let var = ir::Variable::new(ident.text.clone());
					let ty = transform_typename(typename);
					self.compile_expression(builder, expr, Some((var, ty)));
				}

				Assign { .. } => unimplemented!("mutable variables not yet supported in ir code"),

				If {
					condition, block, ..
				} => {
					let condition = self.compile_expression(builder, condition, None);
					let mut then_builder = ir::Builder::new();
					self.compile_statements(&mut then_builder, block.as_slice());

					// The rest of the statements in this block are treated as the else
					// block of the IR `if-else` term.
					let mut else_builder = ir::Builder::new();
					self.compile_statements(&mut else_builder, &statements[i + 1..]);

					builder.build_if_else(condition, then_builder.build(), else_builder.build());

					// Since we've already compiled the rest of the statements here in the
					// else block of the IR `if-else` term, we are done here.
					return;
				}

				IfElse {
					condition,
					then_block,
					else_block,
					..
				} => {
					let condition = self.compile_expression(builder, condition, None);
					let mut then_builder = ir::Builder::new();
					let mut else_builder = ir::Builder::new();
					self.compile_statements(&mut then_builder, then_block.as_slice());
					self.compile_statements(&mut else_builder, else_block.as_slice());
					builder.build_if_else(condition, then_builder.build(), else_builder.build());

					if i != statements.len() - 1 {
						panic!("continuation statements after if-else are not yet supported in ir code");
					}
				}

				Ret { expr, .. } => {
					let expr = self.compile_expression(builder, expr, None);
					builder.build_ret(expr);

					// It is illegal to add more statements after a return statement.
					// In order to stop the compiler from crashing we return from this
					// method here. We should add a pass to the AST which removes any
					// code after a return statement and logs a warning.
					return;
				}
			}
		}
	}

	/// Compiles an AST expression into a sequence or IR terms returning the IR
	/// variable which holds the result of `expr`.
	fn compile_expression(
		&mut self,
		builder: &mut ir::Builder,
		expr: &syntax::Expression,
		assignee: Option<(ir::Variable, ir::Type)>,
	) -> ir::Variable {
		use syntax::Expression::*;
		match expr {
			Ident(ident) => {
				let var = ir::Variable::new(ident.text.clone());

				// If we are assigning this variable to something else, construct the
				// appropriate bind term. Otherwise, simply return the variable.
				if assignee.is_none() {
					var
				} else {
					let (assignee, ty) = self.get_assignee(assignee);
					builder.build_bind(assignee.clone(), ty, var);
					assignee
				}
			}

			Lit(literal) => self.compile_literal(builder, literal),

			BinOp { op, lhs, rhs } => {
				// It's important to note that the left expression is compiled before
				// the right one. This means that the left expression will be evaluated
				// first and the right one second.
				let lhs = self.compile_expression(builder, lhs, None);
				let rhs = self.compile_expression(builder, rhs, None);
				let op = transform_binary_op(op.op);
				let expr = ir::Expression::new_binary(op, lhs, rhs);
				let (assignee, ty) = self.get_assignee(assignee);
				builder.build_bind(assignee.clone(), ty, expr);
				assignee
			}

			UnOp { op, operand } => {
				let operand = self.compile_expression(builder, operand, None);
				let op = transform_unary_op(op.op);
				let expr = ir::Expression::new_unary(op, operand);
				let (assignee, ty) = self.get_assignee(assignee);
				builder.build_bind(assignee.clone(), ty, expr);
				assignee
			}

			Call {
				func_name, args, ..
			} => {
				let args = args
					.into_iter()
					.map(|a| self.compile_expression(builder, a, None))
					.collect();
				let (assignee, ty) = self.get_assignee(assignee);
				let expr = ir::Expression::new_call(func_name.text.clone(), args);
				builder.build_bind(assignee.clone(), ty, expr);
				assignee
			}
		}
	}

	/// Compiles an AST literal into an IR term returning the variable that was
	/// bound to.
	fn compile_literal(
		&mut self,
		builder: &mut ir::Builder,
		literal: &syntax::Literal,
	) -> ir::Variable {
		use ir::types::Native;
		use syntax::LiteralKind::*;

		// Determine the type of the literal.
		let (ir_type, ir_literal) = match literal.kind {
			Int(x) => (Native::Int, ir::Literal::Int(x)),
			Bool(x) => (Native::Bool, ir::Literal::Bool(x)),
		};

		let inter = self.var_generator.generate();
		builder.build_bind(inter.clone(), ir_type, ir_literal);
		return inter;
	}

	/// Unwraps `assignee` if it exists or returns a new intermediate variable if
	/// it doesn't.
	fn get_assignee(
		&mut self,
		assignee: Option<(ir::Variable, ir::Type)>,
	) -> (ir::Variable, ir::Type) {
		assignee.unwrap_or_else(|| (self.var_generator.generate(), Infer))
	}
}

/// Transforms a syntax typename identifier into an IR type.
fn transform_typename(ident: &syntax::Identifier) -> ir::Type {
	match ident.text.as_str() {
		"int" => ir::Native::Int.into(),
		"bool" => ir::Native::Bool.into(),
		text => ir::Type::Typename(text.to_string()),
	}
}

/// Transforms a syntax binary operator into an IR operator.
fn transform_binary_op(op: syntax::BinaryOp) -> ir::Operator {
	use syntax::BinaryOp::*;

	match op {
		Add => ir::Operator::Add,
		Sub => ir::Operator::Sub,
		Mul => ir::Operator::Mul,
		Div => ir::Operator::Div,
		Rem => ir::Operator::Mod,
		BitAnd => ir::Operator::BitAnd,
		BitOr => ir::Operator::BitOr,
		BitXor => ir::Operator::BitXor,
		Eq => ir::Operator::Eq,
		Ne => ir::Operator::Ne,
		Gt => ir::Operator::Gt,
		Ge => ir::Operator::Ge,
		Lt => ir::Operator::Lt,
		Le => ir::Operator::Le,
	}
}

/// Transforms a syntax unary operator into an IR operator.
fn transform_unary_op(op: syntax::UnaryOp) -> ir::Operator {
	use syntax::UnaryOp::*;

	match op {
		Neg => ir::Operator::Neg,
		BitNot => ir::Operator::BitNot,
		Not => ir::Operator::Not,
	}
}

/// Generates objects of type `T` with unique names.
///
/// Generated names are in the form "$x" where 'x' is some number.
pub struct UniqueNameGenerator<T>
where
	T: From<String>,
{
	next: usize,
	_marker: PhantomData<T>,
}

impl<T> UniqueNameGenerator<T>
where
	T: From<String>,
{
	pub fn new() -> Self {
		UniqueNameGenerator {
			next: 0,
			_marker: PhantomData,
		}
	}

	/// Generates a unique name.
	pub fn generate_name(&mut self) -> String {
		let name = format!("${}", self.next);
		self.next += 1;
		return name;
	}

	/// Generates an object with a unique name.
	pub fn generate(&mut self) -> T {
		T::from(self.generate_name())
	}
}
