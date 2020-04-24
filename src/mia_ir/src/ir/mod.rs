//! IR language definition.

mod func;
mod scope;
mod stmt;
mod type_value;
mod value;

use id_arena::Arena;

pub use func::*;
pub use scope::*;
pub use stmt::*;
pub use type_value::*;
pub use value::*;

/// IR context holds top-level information about a translation unit including
/// a list of defined functions and the allocated for scopes.
pub struct Context {
	scope_arena: Arena<Scope>,
	function_arena: Arena<Function>,
}

impl Context {
	/// Constructs a new context.
	pub fn new() -> Self {
		Context {
			scope_arena: Arena::new(),
			function_arena: Arena::new(),
		}
	}

	/// Constructs a new builder for this context.
	pub fn new_builder(&mut self) -> Builder {
		Builder::new(self)
	}

	/// Allocates a scope in this context.
	pub fn alloc_scope(&mut self, s: Scope) -> ScopeId {
		self.scope_arena.alloc(s)
	}

	/// Allocates a function in this context.
	pub fn alloc_function(&mut self, f: Function) -> FunctionId {
		self.function_arena.alloc(f)
	}

	/// Retrieves the scope with a given id.
	pub fn get_scope(&self, id: ScopeId) -> Option<&Scope> {
		self.scope_arena.get(id)
	}

	/// Retrieves the scope with a given id.
	pub fn get_scope_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
		self.scope_arena.get_mut(id)
	}

	/// Retries the function with a given id.
	pub fn get_function(&self, id: FunctionId) -> Option<&Function> {
		self.function_arena.get(id)
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

	/// Creates a new function returning its id and setting the builder's active
	/// scope to be the new function's scope.
	pub fn new_function<S>(
		&mut self,
		name: S,
		parameters: Vec<ValueId>,
		return_type: Option<TypeId>,
	) -> FunctionId
	where
		S: Into<String>,
	{
		let scope = self.open_scope();
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
	/// ## Return
	///
	/// Returns a value identifier representing the result of the statement along
	/// with the scope for the true and false block (in that order).
	///
	/// ## Panics
	///
	/// Panics if this builder has no active scope or it's active scope is not
	/// allocated in the builder's context.
	pub fn build_if_else(
		&mut self,
		assignee: Option<ValueId>,
		condition: ValueId,
	) -> (ValueId, ScopeId, ScopeId) {
		let assignee = self.assignee_or_temp(assignee);
		let true_scope = self.new_scope();
		let false_scope = self.new_scope();
		let if_else = Statement::new_if_else(assignee, condition, true_scope, false_scope);
		self.active_scope().statements.push(if_else);
		return (assignee, true_scope, false_scope);
	}
}
