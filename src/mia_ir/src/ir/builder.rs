use super::module::Module;
use super::scope::{Scope, ScopeId};
use super::traits::IdArena;
use crate::ir::{
	BinaryOperator, Function, FunctionId, Instruction, LiteralKind, Type, TypeId, UnaryOperator,
	Value, ValueId,
};

pub struct Builder<'m> {
	pub module: &'m mut Module,
	active_scope: Option<ScopeId>,
}

impl<'m> Builder<'m> {
	/// Constructs a new builder instance.
	pub fn new(module: &'m mut Module) -> Self {
		Builder {
			module,
			active_scope: None,
		}
	}

	/// Returns a reference to the active scope.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope or the active scope is not allocated
	/// in the builder's module.
	pub fn active_scope(&self) -> &Scope {
		self.module
			.get_unwrap(self.active_scope.expect("no active scope"))
	}

	/// Returns a mutable reference to the active scope.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope or the active scope is not allocated
	/// in the builder's module.
	pub fn active_scope_mut(&mut self) -> &mut Scope {
		self.module
			.get_mut_unwrap(self.active_scope.expect("no active scope"))
	}

	/// Sets the active scope for the builder.
	pub fn set_active_scope(&mut self, id: Option<ScopeId>) {
		self.active_scope = id;
	}

	/// Constructs a new scope returning a unique identifier for it.
	pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
		self.module.alloc(Scope::new(parent))
	}

	/// Constructs a new scope and sets it as the active scope. Returns the id
	/// of the newly created scope.
	///
	/// If the builder already has an active scope, that scope will become the
	/// parent of the new scope.
	pub fn open_scope(&mut self) -> ScopeId {
		let id = self.new_scope(self.active_scope);
		self.set_active_scope(Some(id));
		return id;
	}

	/// Replaces the builder's active scope with it's parent.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope or if the active scope is not allocated
	/// in the builder's module.
	pub fn close_scope(&mut self) {
		let parent = self.active_scope().parent;
		self.set_active_scope(parent);
	}

	/// Searches the active scope for the most recent value which satisfies
	/// `predicate`.
	///
	/// ## Panics
	///
	/// Panics if there is not active scope or the active scope, or any of its
	/// ancestors, is not allocated in the builder's module.
	pub fn search_value<P>(&'m self, predicate: P) -> Option<ValueId>
	where
		P: Fn(&'m Value) -> bool + Clone,
	{
		self.active_scope().search(self.module, predicate)
	}

	/// Searches the active scope for the most recent value with a given name.
	///
	/// ## Panics
	///
	/// Panics if there is not active scope or the active scope, or any of its
	/// ancestors, is not allocated in the builder's module.
	pub fn search_value_by_name(&self, name: &str) -> Option<ValueId> {
		self.active_scope().search_by_name(self.module, name)
	}

	/// Registers a value in the active scope by pushing it onto the scope's list
	/// of values.
	///
	/// This action makes `value` searchable via the builder's `search_value` and
	/// `search_value_by_name` methods.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn register_value(&mut self, value: ValueId) {
		self.active_scope_mut().push_value(value)
	}

	/// Allocates a value in this builder's module registering its id in the
	/// active scope.
	///
	/// Returns the id of the newly allocated value.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope or the the active scope is not
	/// allocated in the builder's module.
	pub fn alloc_in_scope(&mut self, value: Value) -> ValueId {
		let id = self.module.alloc(value);
		self.register_value(id);
		return id;
	}

	/// Allocates a new implicit type returning it's identifier.
	pub fn new_implicit_type(&mut self) -> TypeId {
		self.module.alloc_with_id(|id| Type::Implicit { id })
	}

	/// If `value` is not `None`, it is returned, otherwise a new temporary value
	/// is created, with an implicit type, and returned.
	fn value_or_temp(&mut self, value: Option<ValueId>) -> ValueId {
		value.unwrap_or_else(|| {
			let type_id = self.new_implicit_type();
			self.module
				.alloc_with_id(|id| Value::new_temporary(id, type_id))
		})
	}

	/// Builds a `Call` instruction appending it to the active scope's list of
	/// instructions.
	///
	/// Returns the id of the assignee.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_call(
		&mut self,
		assignee: Option<ValueId>,
		function: FunctionId,
		arguments: Vec<ValueId>,
	) -> ValueId {
		let assignee = self.value_or_temp(assignee);
		let instruction = Instruction::Call {
			assignee,
			function,
			arguments,
		};
		self.active_scope_mut().push_instruction(instruction);
		return assignee;
	}

	/// Builds a `UnaryOp` instruction appending it to the active scope's list of
	/// instructions.
	///
	/// Returns the id of the assignee.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_unary_op(
		&mut self,
		assignee: Option<ValueId>,
		op: UnaryOperator,
		operand: ValueId,
	) -> ValueId {
		let assignee = self.value_or_temp(assignee);
		let instruction = Instruction::UnaryOp {
			assignee,
			op,
			operand,
		};
		self.active_scope_mut().push_instruction(instruction);
		return assignee;
	}

	/// Builds a `BinaryOp` instruction appending it to the active scope's list
	/// of instructions.
	///
	/// Returns the id of the assignee.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_binary_op(
		&mut self,
		assignee: Option<ValueId>,
		op: BinaryOperator,
		lhs: ValueId,
		rhs: ValueId,
	) -> ValueId {
		let assignee = self.value_or_temp(assignee);
		let instruction = Instruction::BinaryOp {
			assignee,
			op,
			lhs,
			rhs,
		};
		self.active_scope_mut().push_instruction(instruction);
		return assignee;
	}

	/// Builds a `IfElse` instruction appending it to the active scope's list of
	/// instructions.
	///
	/// Returns the id of the assignee.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_if_else(
		&mut self,
		assignee: Option<ValueId>,
		condition: ValueId,
		true_scope: ScopeId,
		false_scope: ScopeId,
	) -> ValueId {
		let assignee = self.value_or_temp(assignee);
		let instruction = Instruction::IfElse {
			assignee,
			condition,
			true_scope,
			false_scope,
		};
		self.active_scope_mut().push_instruction(instruction);
		return assignee;
	}

	/// Builds an `Alias` instruction appending it to the active scope's list of
	/// instructions.
	///
	/// Returns the id of the assignee.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_alias(&mut self, assignee: ValueId, value: ValueId) -> ValueId {
		let instruction = Instruction::Alias { assignee, value };
		self.active_scope_mut().push_instruction(instruction);
		return assignee;
	}

	/// Builds a `Return` instruction appending it to the active scope's list of
	/// instructions.
	///
	/// ## Panics
	///
	/// Panics if there is no active scope.
	pub fn build_return(&mut self, value: ValueId) {
		let instruction = Instruction::Return { value };
		self.active_scope_mut().push_instruction(instruction);
	}

	/// Builds a `Function` definition appending it to the builder's module.
	pub fn build_function(
		&mut self,
		name: String,
		parameters: Vec<ValueId>,
		return_type: TypeId,
		scope: ScopeId,
	) -> FunctionId {
		let function = Function::new(name, parameters, return_type, scope);
		self.module.alloc(function)
	}

	/// Builds a new integer literal.
	pub fn build_integer_literal(&mut self, value: u64) -> ValueId {
		// TODO: Consider memoizing this type identifier
		let type_id = self.module.alloc(Type::IntegerLiteralType);
		let value = Value::Literal {
			type_value: type_id,
			kind: LiteralKind::Int { value },
		};
		return self.module.alloc(value);
	}
}
