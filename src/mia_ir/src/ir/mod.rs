//! IR language definition.

mod func;
mod scope;
mod stmt;
mod type_value;
mod value;

use crate::error::*;
use id_arena::Arena;
use mia_syntax::ast;

pub use func::*;
pub use scope::*;
use std::borrow::Borrow;
pub use stmt::*;
pub use type_value::*;
pub use value::*;

const BUILTIN_TYPE_NAMES: [&'static str; 8] = [
	"i8",
	"i16",
	"i32",
	"i64",
	"u8",
	"u16",
	"u32",
	"u64",
];

/// IR context holds top-level information about a translation unit including
/// a list of defined functions and the allocated for scopes.
pub struct Context {
	scope_arena: Arena<Scope>,
	function_arena: Arena<Function>,
	type_arena: Arena<Type>,
}

impl Context {
	/// Constructs a new context.
	pub fn new() -> Self {
		let mut context = Context {
			scope_arena: Arena::new(),
			function_arena: Arena::new(),
			type_arena: Arena::new(),
		};

		// Populate context's type-space with builtin types.
		for type_name in &BUILTIN_TYPE_NAMES {
			context.type_arena.alloc(Type::new(type_name.to_string()));
		}

		return context;
	}

	/// Constructs a new builder for this context.
	pub fn new_builder(&mut self) -> Builder {
		Builder::new(self)
	}

	/// Allocates a scope in this context.
	pub fn alloc_scope(&mut self, s: Scope) -> ScopeId {
		self.scope_arena.alloc(s)
	}

	/// Retrieves the scope with a given id.
	pub fn get_scope(&self, id: ScopeId) -> Option<&Scope> {
		self.scope_arena.get(id)
	}

	/// Retrieves the scope with a given id.
	pub fn get_scope_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
		self.scope_arena.get_mut(id)
	}

	/// Allocates a function in this context.
	pub fn alloc_function(&mut self, f: Function) -> FunctionId {
		self.function_arena.alloc(f)
	}

	/// Retries the function with a given id.
	pub fn get_function(&self, id: FunctionId) -> Option<&Function> {
		self.function_arena.get(id)
	}

	/// Returns the id and reference to the first function which satisfies a
	/// given predicate.
	pub fn find_function<P>(&self, predicate: P) -> Option<(FunctionId, &Function)>
	where
		P: Fn(&Function) -> bool,
	{
		self.function_arena.iter().find(|(_, f)| predicate(f))
	}

	/// Returns the id and reference to the first function with a given name.
	pub fn find_function_with_name<S>(&self, name: S) -> Option<(FunctionId, &Function)>
	where
		S: Borrow<String>,
	{
		self.find_function(|f| f.name.eq(name.borrow()))
	}

	/// Allocates a new type in this context.
	pub fn alloc_type(&mut self, t: Type) -> TypeId {
		self.type_arena.alloc(t)
	}

	/// Retrieves the type with a given `id` in this context.
	pub fn get_type(&self, id: TypeId) -> Option<&Type> {
		self.type_arena.get(id)
	}

	/// Retrieves a mutable reference to the type with a given `id`.
	pub fn get_type_mut(&mut self, id: TypeId) -> Option<&mut Type> {
		self.type_arena.get_mut(id)
	}

	/// Searches for and returns the first type in this context which satisfies a
	/// given predicate.
	pub fn find_type<P>(&self, predicate: P) -> Option<(TypeId, &Type)>
	where
		P: Fn(&Type) -> bool,
	{
		self.type_arena.iter().find(|(_, item)| predicate(item))
	}
}

/// IR builder is a utility object which aids in the construction of IR code.
pub struct Builder<'ctx> {
	context: &'ctx mut Context,
	active_scope: Option<ScopeId>,
}

impl<'ctx> Builder<'ctx> {
	/// Constructs a new builder for `context`.
	pub fn new(context: &'ctx mut Context) -> Self {
		Builder {
			context,
			active_scope: None,
		}
	}

	/// True if this builder has an active scope.
	pub fn has_active_scope(&self) -> bool {
		self.active_scope.is_some()
	}

	/// Returns this builder's active scope panicking if there is none.
	///
	/// ## Panics
	///
	/// Panics if this builder does not have an active scope. Use
	/// `has_active_scope` to check if one exists.
	pub fn active_scope_id(&self) -> ScopeId {
		self.active_scope.expect("no active scope")
	}

	/// Sets the active scope of this builder.
	pub fn set_active_scope(&mut self, id: ScopeId) {
		self.active_scope = Some(id);
	}

	/// Returns a reference to this builder's active scope stored in its context.
	///
	/// ## Panics
	///
	/// Panics if any of the following are true:
	///
	/// * The builder does not have an active scope
	///
	/// * The builder's active scope is not allocated in it's context
	///
	pub fn active_scope(&mut self) -> &mut Scope {
		let id = self.active_scope_id();
		self.context
			.get_scope_mut(id)
			.expect("scope allocated in different context")
	}

	/// Creates a new child scope of the active one.
	pub fn new_scope(&mut self) -> ScopeId {
		let scope = Scope::new(self.active_scope);
		self.context.alloc_scope(scope)
	}

	/// Creates a new child scope of the active one then replaces the active scope
	/// with the newly created one.
	///
	/// To create a new child scope of the active one without changing the active
	/// scope, use `new_scope` instead.
	pub fn open_scope(&mut self) -> ScopeId {
		let id = self.new_scope();
		self.active_scope = Some(id);
		return id;
	}

	/// Replaces the active scope with its parent.
	///
	/// ## Panics
	///
	/// This method panics if any of the following are true:
	///
	/// * The active scope is `None`. Use `has_active_scope` to check that this
	/// builder has an active scope.
	///
	/// * The active scope was not allocated in this builder's context.
	///
	pub fn close_scope(&mut self) {
		let id = self.active_scope.expect("no active scope");
		self.active_scope = self
			.context
			.get_scope(id)
			.expect("scope allocated in different context")
			.parent;
	}

	/// Creates a new function returning its id.
	pub fn build_function<S>(
		&mut self,
		name: S,
		parameters: Vec<ValueId>,
		return_type: Option<TypeId>,
		scope: ScopeId,
	) -> FunctionId
	where
		S: Into<String>,
	{
		let function = Function::new(name, parameters, return_type, scope);
		self.context.alloc_function(function)
	}

	fn assignee_or_temp(&mut self, assignee: Option<ValueId>) -> ValueId {
		if let Some(a) = assignee {
			a
		} else {
			self.active_scope().alloc_temp_value()
		}
	}

	/// Aliases `value` with `assignee`.
	///
	/// Always returns `assignee`.
	pub fn build_alias(&mut self, assignee: ValueId, value: ValueId) -> ValueId {
		let alias = Statement::new_alias(assignee, value);
		self.active_scope().statements.push(alias);
		return assignee;
	}

	/// Constructs a `Call` statement.
	///
	/// The constructed statement is appended to the sequence of statements
	/// in the active scope.
	///
	/// ## Parameters
	///
	/// * `assignee`: An optional value to assign the result of the call to
	///
	/// * `function`: The function to call
	///
	/// * `arguments`: A list of arguments to pass to the function
	///
	/// ## Return
	///
	/// Returns a value identifier representing the result of the call. If
	/// `assignee` is not `None` then it will be returned. Otherwise the id of a
	/// new temporary value will be returned.
	///
	/// ## Panics
	///
	/// Panics if this builder has no active scope or it's active scope is not
	/// allocated in the builder's context.
	pub fn build_call(
		&mut self,
		assignee: Option<ValueId>,
		function: FunctionId,
		arguments: Vec<ValueId>,
	) -> ValueId {
		let assignee = self.assignee_or_temp(assignee);
		let call = Statement::new_call(assignee, function, arguments);
		self.active_scope().statements.push(call);
		return assignee;
	}

	/// Constructs a `UnaryOperation` statement.
	///
	/// The constructed statement is appended to the sequence of statements
	/// in the active scope.
	///
	/// ## Parameters
	///
	/// * `assignee`: An optional value to assign the result of the operation to
	///
	/// * `op`: The unary operation to construct
	///
	/// * `v`: The argument for the unary operation
	///
	/// ## Return
	///
	/// Returns a value identifier representing the result of the operation. If
	/// `assignee` is not `None` then it will be returned. Otherwise the id of a
	/// new temporary value will be returned.
	///
	/// ## Panics
	///
	/// Panics if this builder has no active scope or it's active scope is not
	/// allocated in the builder's context.
	pub fn build_unary_op(
		&mut self,
		assignee: Option<ValueId>,
		op: UnaryOperation,
		v: ValueId,
	) -> ValueId {
		let assignee = self.assignee_or_temp(assignee);
		let unary_op = Statement::new_unary_op(assignee, op, v);
		self.active_scope().statements.push(unary_op);
		return assignee;
	}

	/// Constructs a `BinaryOperation` statement.
	///
	/// The constructed statement is appended to the sequence of statements
	/// in the active scope.
	///
	/// ## Parameters
	///
	/// * `assignee`: An optional value to assign the result of the operation to
	///
	/// * `op`: The binary operation to construct
	///
	/// * `lhs`: The left argument to the operation
	///
	/// * `rhs`: The right argument to the operation
	///
	/// ## Return
	///
	/// Returns a value identifier representing the result of the operation. If
	/// `assignee` is not `None` then it will be returned. Otherwise the id of a
	/// new temporary value will be returned.
	///
	/// ## Panics
	///
	/// Panics if this builder has no active scope or it's active scope is not
	/// allocated in the builder's context.
	pub fn build_binary_op(
		&mut self,
		assignee: Option<ValueId>,
		op: BinaryOperation,
		lhs: ValueId,
		rhs: ValueId,
	) -> ValueId {
		let assignee = self.assignee_or_temp(assignee);
		let binary_op = Statement::new_binary_op(assignee, op, lhs, rhs);
		self.active_scope().statements.push(binary_op);
		return assignee;
	}

	/// Constructs aa `IfElse` statement.
	///
	/// The constructed statement is appended to the sequence of statements
	/// in the active scope.
	///
	/// ## Parameters
	///
	/// * `assignee`: An optional value to assign the result of the operation to
	///
	/// * `condition`: The condition for the if/else statement
	///
	/// * `true_scope`: The scope for the true block of the if expression
	///
	/// * `false_scope`: The scope for the false block of the if expression
	///
	/// ## Return
	///
	/// Returns a value identifier representing the result of the statement. If
	/// `assignee` is not `None`, then it will be returned. Otherwise the id of a
	/// new temporary value will be returned.
	///
	/// ## Panics
	///
	/// Panics if this builder has no active scope or it's active scope is not
	/// allocated in the builder's context.
	pub fn build_if_else(
		&mut self,
		assignee: Option<ValueId>,
		condition: ValueId,
		true_scope: ScopeId,
		false_scope: ScopeId,
	) -> ValueId {
		let assignee = self.assignee_or_temp(assignee);
		let if_else = Statement::new_if_else(assignee, condition, true_scope, false_scope);
		self.active_scope().statements.push(if_else);
		return assignee;
	}

	/// Returns the id and reference of of the first function that satisfies
	/// `predicate`.
	pub fn find_function<P>(&self, predicate: P) -> Option<(FunctionId, &Function)>
	where
		P: Fn(&Function) -> bool,
	{
		self.context.find_function(predicate)
	}

	/// Returns the id and reference of the first function with a given name.
	pub fn find_function_with_name<S>(&self, name: S) -> Option<(FunctionId, &Function)>
	where
		S: Borrow<String>,
	{
		self.context.find_function_with_name(name)
	}

	/// Returns the first type which satisfies `predicate`.
	pub fn find_type<P>(&self, predicate: P) -> Option<(TypeId, &Type)>
	where
		P: Fn(&Type) -> bool,
	{
		self.context.find_type(predicate)
	}

	/// Recursively searches the active scope and all above scopes returning the
	/// first value which satisfies `predicate`.
	///
	/// ## Panics
	///
	/// This method panics if any of the following conditions are true:
	///
	/// * There is no active scope
	///
	/// * The active scope, or any of its ancestors are not allocated in this
	/// builder's context
	///
	pub fn lookup_value<P>(&self, predicate: P) -> Option<(ValueId, &Value)>
	where
		P: Fn(&Value) -> bool + Clone,
	{
		let scope_id = self.active_scope_id();
		let scope = self
			.context
			.get_scope(scope_id)
			.expect("scope allocated in different context");
		scope.lookup_value(self.context, predicate)
	}

	/// Recursively searches the active scope and all above scopes returning the
	/// first value with a given name.
	///
	/// ## Panics
	///
	/// This method panics if any of the following conditions are true:
	///
	/// * There is no active scope
	///
	/// * The active scope, or any of its ancestors are not allocated in this
	/// builder's context
	///
	pub fn lookup_value_with_name<S>(&self, name: S) -> Option<(ValueId, &Value)>
	where
		S: Borrow<String>,
	{
		let scope_id = self.active_scope_id();
		let scope = self
			.context
			.get_scope(scope_id)
			.expect("scope allocated in different context");
		scope.lookup_value_with_name(self.context, name)
	}
}

/// Compiles an AST `TranslationUnit` into an IR `Context`.
pub fn compile_ast(tu: ast::TranslationUnit) -> Result<Context> {
	let mut context = Context::new();
	let mut compiler = AstCompiler::new(&mut context);
	compiler.compile_translation_unit(tu)?;
	Ok(context)
}

struct AstCompiler<'ctx> {
	builder: Builder<'ctx>,
}

impl<'ctx> AstCompiler<'ctx> {
	pub fn new(context: &'ctx mut Context) -> Self {
		AstCompiler {
			builder: Builder::new(context),
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
		let has_previous_definition = self
			.builder
			.find_function_with_name(&func.ident.name)
			.is_some();
		if has_previous_definition {
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
					.active_scope()
					.find_value_with_name(&parameter.ident.name)
					.is_some()
				{
					return Err(NamingError::RedefinitionOfVariable(parameter.ident.name).into());
				}

				Ok(self.builder.active_scope().alloc_value(Value::new(
					parameter.ident.name,
					type_id,
					false,
				)))
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
				let has_previous_definition =
					self.builder.lookup_value_with_name(&ident.name).is_some();
				if has_previous_definition {
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
					.active_scope()
					.alloc_value(Value::new(ident.name, type_value, is_mutable));

				// Compile the expression and assign the result to the new value.
				self.compile_expression(Some(var), value)?;
			}

			Assign { ident, value } => {
				// Lookup the id for the variable being assigned to.
				let (var, _) = self
					.builder
					.lookup_value_with_name(&ident.name)
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
					.lookup_value_with_name(&i.name)
					.map(|(id, _)| id)
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
					.find_function_with_name(&name.name)
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
		use ast::TypeIndirection::*;

		match type_value.indirection {
			Pointer => unimplemented!("pointer types are not yet supported"),
			Reference => unimplemented!("reference types are not yet supported"),
			None => (),
		}
		let name = type_value.name.name;
		self.builder
			.find_type(|t| match t {
				Type::Simple { name: n, .. } => n == &name,
				_ => false,
			})
			.map(|(id, _)| id)
			.ok_or(NamingError::UseOfUndefinedType(name).into())
	}
}
