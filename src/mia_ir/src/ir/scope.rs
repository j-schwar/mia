use super::stmt::Statement;
use super::type_value::*;
use super::value::*;
use id_arena::{Arena, Id};

/// Holds IR code for a scope.
#[derive(Debug, Eq, PartialEq)]
pub struct Scope {
	/// The parent scope of `self`.
	///
	/// Statements created in a scope may reference values and types from its
	/// parent, its parent's parent, etc.
	pub parent: Option<ScopeId>,

	/// The sequence of statements describing the logic of this scope.
	pub statements: Vec<Statement>,

	type_arena: Arena<Type>,
	value_arena: Arena<Value>,
}

impl Scope {
	/// Constructs a new, empty scope with an optional parent.
	pub fn new(parent: Option<ScopeId>) -> Self {
		Scope {
			parent,
			statements: Vec::new(),
			type_arena: Arena::new(),
			value_arena: Arena::new(),
		}
	}

	/// Retrieves the type with a given `id` in this scope.
	pub fn get_type(&self, id: TypeId) -> Option<&Type> {
		self.type_arena.get(id)
	}

	/// Retrieves a mutable reference to the type with a given `id`.
	pub fn get_type_mut(&mut self, id: TypeId) -> Option<&mut Type> {
		self.type_arena.get_mut(id)
	}

	/// Searches for and returns the first type in this scope which satisfies a
	/// given predicate.
	pub fn find_type<P>(&self, predicate: P) -> Option<(TypeId, &Type)>
	where
		P: Fn(&Type) -> bool,
	{
		self.type_arena.iter().find(|(_, item)| predicate(item))
	}

	/// Allocates a new type in this scope.
	pub fn alloc_type(&mut self, t: Type) -> TypeId {
		self.type_arena.alloc(t)
	}

	/// Retrieves the value with a given `id` in this scope.
	pub fn get_value(&self, id: ValueId) -> Option<&Value> {
		self.value_arena.get(id)
	}

	/// Retrieves a mutable reference to the value with a given `id`.
	pub fn get_value_mut(&mut self, id: ValueId) -> Option<&mut Value> {
		self.value_arena.get_mut(id)
	}

	/// Searches for and returns the first value in this scope which satisfies a
	/// given predicate.
	pub fn find_value<P>(&self, predicate: P) -> Option<(ValueId, &Value)>
	where
		P: Fn(&Value) -> bool,
	{
		self.value_arena.iter().find(|(_, item)| predicate(item))
	}

	/// Allocates a new value in this scope.
	pub fn alloc_value(&mut self, v: Value) -> ValueId {
		self.value_arena.alloc(v)
	}

	/// Allocates a new temporary value in this scope.
	pub fn alloc_temp_value(&mut self) -> ValueId {
		self.value_arena
			.alloc_with_id(|id| Value::new_temporary(id, None))
	}
}

pub type ScopeId = Id<Scope>;
