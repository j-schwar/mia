//! Compilers for converting IR code into LLVM.
//!
//! By the time we reach this stage, the input IR code should have been
//! validated to ensure its correctness and the compilation to LLVM should be a
//! trivial transformation with no possibility of error. As such, the majority
//! of the functions/methods defined in this module panic if they come across
//! something unexpected. Such cases should have been corrected or flagged as
//! errors by IR passes before reaching this stage.

pub use inkwell::context::Context;
pub use inkwell::module::Module;

use crate::grammar::{Definition, Expression, Literal, Operator, Program, Term, Value, Variable};
use crate::types::{Native, Type};
use inkwell::builder::Builder;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue};
use inkwell::IntPredicate;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

pub struct CodeGen<'ctx> {
	pub context: &'ctx Context,
	pub module: Module<'ctx>,
	builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
	/// Constructs a new code generator.
	pub fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
		CodeGen {
			context,
			module,
			builder: context.create_builder(),
		}
	}

	pub fn compile_program(&self, program: &Program) {
		let functions = program
			.definitions
			.iter()
			.map(|d| self.compile_function_declaration(d))
			.collect::<Vec<_>>();

		for (function, definition) in functions.iter().zip(program.definitions.iter()) {
			self.compile_function_body(&definition, function.clone())
		}
	}

	pub fn compile_function(&self, definition: &Definition) {
		let value = self.compile_function_declaration(&definition);
		self.compile_function_body(&definition, value);
	}

	fn compile_function_declaration(&self, definition: &Definition) -> FunctionValue<'ctx> {
		// Determine the function type.
		let param_types: Vec<_> = definition
			.parameters
			.iter()
			.map(|(_, t)| self.type_to_llvm_type(t))
			.collect();
		let fn_type = if definition.return_type.is_unit() {
			self.context
				.void_type()
				.fn_type(param_types.as_slice(), false)
		} else {
			let return_type = self.type_to_llvm_type(&definition.return_type);
			return_type.fn_type(param_types.as_slice(), false)
		};

		// Construct the function definition.
		self.module.add_function(&definition.name, fn_type, None)
	}

	fn compile_function_body(&self, definition: &Definition, function: FunctionValue<'ctx>) {
		// Construct the initial basic block.
		let entry_block = self.context.append_basic_block(function, "entry");
		self.builder.position_at_end(entry_block);

		let mut function_compiler = FunctionCompiler::new(self);

		// Add parameters to the compiler so that the body may access them.
		let param_iter = definition
			.parameters
			.iter()
			.map(|(v, _)| v)
			.zip(function.get_param_iter());
		for (variable, value) in param_iter {
			function_compiler.add_variable(variable.clone(), value.into());
		}

		// Compile the function body.
		function_compiler.compile_term(&definition.term);
	}

	/// Converts an IR type into an LLVM one.
	pub fn type_to_llvm_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
		match t {
			Type::Native(n) => match n {
				Native::Int => self.context.i64_type().into(),
				Native::Bool => self.context.bool_type().into(),
			},
			Type::Typename(_) => unimplemented!("type names are not yet supported"),
			Type::Sum(_) => unimplemented!("sum types are not yet supported"),
			Type::Product(_) => unimplemented!("product types are not yet supported"),
			Type::Unit => unimplemented!("the unit type is not net supported"),
		}
	}
}

/// Compiler for function definitions.
///
/// Its whole purpose is to compile a single function into LLVM.
struct FunctionCompiler<'a, 'ctx> {
	codegen: &'a CodeGen<'ctx>,
	var_map: HashMap<Variable, AnyValueEnum<'ctx>>,
}

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
	pub fn new(codegen: &'a CodeGen<'ctx>) -> Self {
		FunctionCompiler {
			codegen,
			var_map: HashMap::new(),
		}
	}

	pub fn add_variable(&mut self, variable: Variable, value: AnyValueEnum<'ctx>) {
		self.var_map.insert(variable, value);
	}

	pub fn compile_term(&mut self, term: &Term) {
		use Term::*;
		match term {
			Bind {
				variable,
				expression,
				continuation,
				..
			} => {
				let value = self.compile_expression(expression, &variable.name);
				self.var_map.insert(variable.clone(), value);
				self.compile_term(continuation);
			}

			Match { .. } => unimplemented!("match terms are not yet supported"),

			Return(v) => {
				let value = self.lookup_variable(v);
				self.codegen.builder.build_return(Some(
					&BasicValueEnum::try_from(value)
						.expect("failed to convert AnyValue to BasicValue"),
				));
			}

			Undefined => panic!("undefined term reached: internal compiler error"),
		}
	}

	fn binary_operands(
		&self,
		operands: &Vec<Variable>,
	) -> (AnyValueEnum<'ctx>, AnyValueEnum<'ctx>) {
		let lhs = self.lookup_variable(&operands[0]);
		let rhs = self.lookup_variable(&operands[1]);
		return (lhs, rhs);
	}

	fn int_compare(
		&self,
		predicate: IntPredicate,
		operands: &Vec<Variable>,
		result_name: &str,
	) -> AnyValueEnum<'ctx> {
		let (lhs, rhs) = self.binary_operands(operands);
		self.codegen
			.builder
			.build_int_compare(
				predicate,
				lhs.into_int_value(),
				rhs.into_int_value(),
				result_name,
			)
			.into()
	}

	fn compile_expression(&self, expr: &Expression, result_name: &str) -> AnyValueEnum<'ctx> {
		use Expression::*;
		match expr {
			Value(v) => self.compile_value(v),

			Operation { operator, operands } => {
				use Operator::*;
				match operator {
					/* Binary Arithmetic Operators */
					Add => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_int_add(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					Sub => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					Mul => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					Div => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_int_signed_div(
								lhs.into_int_value(),
								rhs.into_int_value(),
								result_name,
							)
							.into()
					}

					Mod => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_int_signed_rem(
								lhs.into_int_value(),
								rhs.into_int_value(),
								result_name,
							)
							.into()
					}

					/* Unary Arithmetic Operators */
					Neg => {
						let operand = self.lookup_variable(&operands[0]).into_int_value();
						self.codegen
							.builder
							.build_int_neg(operand, result_name)
							.into()
					}

					/* Binary Bitwise */
					BitAnd => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_and(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					BitOr => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_or(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					BitXor => {
						let (lhs, rhs) = self.binary_operands(operands);
						self.codegen
							.builder
							.build_xor(lhs.into_int_value(), rhs.into_int_value(), result_name)
							.into()
					}

					/* Unary Bitwise */
					BitNot => {
						let operand = self.lookup_variable(&operands[0]).into_int_value();
						self.codegen.builder.build_not(operand, result_name).into()
					}

					/* Binary Logical Operators */
					Eq => self.int_compare(IntPredicate::EQ, operands, result_name),

					Ne => self.int_compare(IntPredicate::NE, operands, result_name),

					Gt => self.int_compare(IntPredicate::SGT, operands, result_name),

					Ge => self.int_compare(IntPredicate::SGE, operands, result_name),

					Lt => self.int_compare(IntPredicate::SLT, operands, result_name),

					Le => self.int_compare(IntPredicate::SLE, operands, result_name),

					/* Unary Logical Operators */
					Not => {
						let operand = self.lookup_variable(&operands[0]).into_int_value();
						let zero = self.codegen.context.bool_type().const_zero();
						self.codegen
							.builder
							.build_int_compare(IntPredicate::EQ, operand, zero, result_name)
							.into()
					}
				}
			}

			Call { name, arguments } => {
				let function = self
					.codegen
					.module
					.get_function(name)
					.expect(&format!("unable to find function named '{}'", name));
				let arguments = arguments
					.iter()
					.map(|v| {
						self.lookup_variable(v)
							.try_into()
							.expect("unable to convert to basic value")
					})
					.collect::<Vec<_>>();
				self.codegen
					.builder
					.build_call(function, arguments.as_slice(), result_name)
					.as_any_value_enum()
			}
		}
	}

	fn compile_value(&self, value: &Value) -> AnyValueEnum<'ctx> {
		use Value::*;
		match value {
			Literal(l) => self.compile_literal(l),
			Variable(v) => self.lookup_variable(v),
			SumVariant { .. } => unimplemented!("sum variants are not yet supported"),
			Record { .. } => unimplemented!("records are not yet supported"),
			Unit => unimplemented!("the unit type is not yet supported"),
		}
	}

	fn lookup_variable(&self, variable: &Variable) -> AnyValueEnum<'ctx> {
		self.var_map
			.get(variable)
			.expect(&format!(
				"unable to lookup value for variable '{}', has it been defined?",
				variable
			))
			.clone()
	}

	fn compile_literal(&self, literal: &Literal) -> AnyValueEnum<'ctx> {
		use Literal::*;
		match literal {
			Int(i) => self.codegen.context.i64_type().const_int(*i as u64, false),
			Bool(b) => {
				if *b {
					self.codegen.context.bool_type().const_int(1, false)
				} else {
					self.codegen.context.bool_type().const_zero()
				}
			}
		}
		.into()
	}
}
