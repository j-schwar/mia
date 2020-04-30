//! IR language definition.

mod builder;
mod func;
mod instruction;
mod module;
mod scope;
pub mod traits;
mod type_value;
mod value;

use crate::error::*;
use mia_syntax::ast;
use traits::*;

pub use builder::*;
pub use func::*;
pub use instruction::*;
pub use module::*;
pub use scope::*;
pub use type_value::*;
pub use value::*;

const BUILTIN_TYPE_NAMES: [&'static str; 9] =
	["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "bool"];

/// Compiles an AST `TranslationUnit` into an IR `Context`.
pub fn compile_ast(tu: ast::TranslationUnit) -> Result<Module> {
	let mut module = Module::new();

	// Allocate built-in types.
	for type_name in &BUILTIN_TYPE_NAMES {
		module.alloc(Type::new(*type_name));
	}

	let mut compiler = AstCompiler::new(&mut module);
	compiler.compile_translation_unit(tu)?;
	Ok(module)
}

struct AstCompiler<'m> {
	builder: Builder<'m>,
}

impl<'m> AstCompiler<'m> {
	/// Constructs a new compiler instance.
	pub fn new(module: &'m mut Module) -> Self {
		AstCompiler {
			builder: Builder::new(module),
		}
	}

	pub fn compile_translation_unit(&mut self, tu: ast::TranslationUnit) -> Result<()> {
		for func in tu.functions {
			self.compile_function_definition(func)?;
		}
		Ok(())
	}

	/// Compiles an AST function definition into an IR function.
	///
	/// ## Parameters
	///
	/// * `func`: The function definition to compile
	///
	fn compile_function_definition(&mut self, func: ast::FunctionDefinition) -> Result<()> {
		// Redefinition of functions is not allowed so search for any existing
		// functions with this specific name and return an error if one is
		// found.
		let previous_definition: Option<(FunctionId, _)> =
			self.builder.module.find_with_name(&func.ident.name);
		if previous_definition.is_some() {
			return Err(NamingError::RedefinitionOfFunction(func.ident.name).into());
		}

		// Introduce a new scope for this function adding its parameters to the
		// scope as values.
		let scope = self.builder.open_scope();
		let parameters = func
			.parameters
			.into_iter()
			.map(|parameter| {
				let type_id = if let Some(type_value) = parameter.type_value {
					Some(self.resolve_type_value(type_value)?)
				} else {
					None
				};

				// Having two parameters named the same is illegal so before the creation
				// of a parameter's value in the scope first ensure that there are no
				// values with the same name already defined.
				if self
					.builder
					.search_value_by_name(&parameter.ident.name)
					.is_some()
				{
					return Err(NamingError::RedefinitionOfVariable(parameter.ident.name).into());
				}

				let value = Value::new(parameter.ident.name, type_id, false);
				let id = self.builder.alloc_in_scope(value);
				Ok(id)
			})
			.collect::<Result<Vec<ValueId>>>()?;

		// Resolve the function's return type if it is present.
		let return_type = if let Some(return_type) = func.return_type {
			Some(self.resolve_type_value(return_type)?)
		} else {
			None
		};

		// Compile the function body.
		match func.body {
			ast::FunctionBody::Expr(e) => {
				self.compile_expression(None, e)?;
			}
			ast::FunctionBody::Block(b) => {
				self.compile_code_block(b)?;
			}
		}

		// Build the function definition.
		self.builder
			.build_function(func.ident.name, parameters, return_type, scope);

		Ok(())
	}

	/// Compiles all the statements in an AST code block into IR code.
	///
	/// Before compilation, a new scope is opened. The statements in `block` are
	/// compiled into this new scope. After compilation is finished, the scope is
	/// closed leaving the builder's active scope the same as before this method
	/// was called.
	///
	/// ## Parameters
	///
	/// * `block`: The code block to compile
	///
	/// ## Returns
	///
	/// Returns the id of the new scope created by this method.
	fn compile_code_block(&mut self, block: ast::CodeBlock) -> Result<ScopeId> {
		let scope_id = self.builder.open_scope();
		for statement in block.statements {
			self.compile_statement(statement)?;
		}
		self.builder.close_scope();
		Ok(scope_id)
	}

	/// Compiles an AST statement into an IR statement.
	///
	/// ## Parameters
	///
	/// * `stmt`: The statement to compile
	///
	/// ## Panics
	///
	/// Panics if the compiler's builder does not have an active scope or if its
	/// active scope is malformed in some way.
	fn compile_statement(&mut self, stmt: ast::Statement) -> Result<()> {
		use ast::Statement::*;

		match stmt {
			Expr(e) => {
				self.compile_expression(None, e)?;
			}

			Define {
				ident,
				is_mutable,
				type_value,
				value,
			} => {
				// Don't allow redefinition of variables so search to see if a variable
				// with this specific name already exists before creating a new one.
				if self.builder.search_value_by_name(&ident.name).is_some() {
					return Err(NamingError::RedefinitionOfVariable(ident.name).into());
				}

				// If there is a type declaration try and resolve it.
				let type_value = if let Some(type_value) = type_value {
					Some(self.resolve_type_value(type_value)?)
				} else {
					None
				};

				// Allocate a new variable.
				let var = self
					.builder
					.alloc_in_scope(Value::new(ident.name, type_value, is_mutable));

				// Compile the expression and assign the result to the new value.
				self.compile_expression(Some(var), value)?;
			}

			Assign { ident, value } => {
				// Lookup the id for the variable being assigned to.
				let var = self
					.builder
					.search_value_by_name(&ident.name)
					.ok_or(NamingError::UseOfUndefinedVariable(ident.name))?;

				// Compile the expression and assign the result to the variable. Latter
				// passes will ensure that the variable is mutable and has a matching
				// type to the expression.
				self.compile_expression(Some(var), value)?;
			}
		}

		Ok(())
	}

	/// Compiles an AST expression into an IR statement.
	///
	/// ## Parameters
	///
	/// * `assignee`: Forwarded to the `build` call which constructs the IR code
	/// for this expression. If supplied, the result of the expression will be
	/// assigned to the variable represented by this id. If not, then a temporary
	/// variable will be created to hold the result of the expression.
	///
	/// * `expr`: The expression to compile
	///
	/// ## Returns
	///
	/// Returns the id of the variable which holds the result of the expression.
	/// If `assignee` was supplied, then that id will be returned. If not, then
	/// the id of the temporary variable which holds the result is returned.
	///
	/// An error is returned if the expression references an undefined variable
	/// or function.
	///
	/// ## Panics
	///
	/// Panics if the compiler's builder does not have an active scope or if its
	/// active scope is malformed in some way.
	fn compile_expression(
		&mut self,
		assignee: Option<ValueId>,
		expr: ast::Expression,
	) -> Result<ValueId> {
		use ast::Expression::*;

		match expr {
			Variable(i) => {
				let var = self
					.builder
					.search_value_by_name(&i.name)
					.ok_or(NamingError::UseOfUndefinedVariable(i.name.clone()))?;

				// If there is a desired assignee, create an alias for the variable.
				let result = if let Some(assignee) = assignee {
					self.builder.build_alias(assignee, var)
				} else {
					var
				};

				Ok(result)
			}

			Literal(_) => unimplemented!("support for IR literals is needed"),

			Call { name, args } => {
				let (function_id, _) = self
					.builder
					.module
					.find_with_name(&name.name)
					.ok_or(NamingError::UseOfUndefinedFunction(name.name.clone()))?;

				let args = args
					.into_iter()
					.map(|e| self.compile_expression(None, e))
					.collect::<Result<Vec<ValueId>>>()?;

				Ok(self.builder.build_call(assignee, function_id, args))
			}

			Prefix { op, expr } => {
				let expr = self.compile_expression(None, *expr)?;
				Ok(self.builder.build_unary_op(assignee, op.into(), expr))
			}

			Infix { op, lhs, rhs } => {
				let lhs = self.compile_expression(None, *lhs)?;
				let rhs = self.compile_expression(None, *rhs)?;
				Ok(self.builder.build_binary_op(assignee, op.into(), lhs, rhs))
			}

			IfElse {
				condition,
				true_block,
				false_block,
			} => {
				let condition = self.compile_expression(None, *condition)?;
				let true_scope = self.compile_code_block(*true_block)?;
				let false_scope = self.compile_code_block(*false_block)?;
				Ok(self
					.builder
					.build_if_else(assignee, condition, true_scope, false_scope))
			}

			Group(e) => self.compile_expression(assignee, *e),
		}
	}

	/// Looks for an IR type which matches `type_value` in the current context.
	fn resolve_type_value(&mut self, type_value: ast::TypeValue) -> Result<TypeId> {
		let name = type_value.name.name;
		self.builder
			.module
			.find_with_name(&name)
			.map(|(id, _)| id)
			.ok_or(NamingError::UseOfUndefinedType(name).into())
	}
}
