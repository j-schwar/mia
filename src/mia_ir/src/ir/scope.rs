use super::stmt::Statement;
use super::type_value::*;
use super::value::*;
use crate::ir::Context;
use id_arena::{Arena, Id};
use std::borrow::Borrow;

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

	value_arena: Arena<Value>,
}

impl Scope {
	/// Constructs a new, empty scope with an optional parent.
	pub fn new(parent: Option<ScopeId>) -> Self {
		Scope {
			parent,
			statements: Vec::new(),
			value_arena: Arena::new(),
		}
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

	/// Recursively searches through this and above scopes for the first value
	/// which satisfies `predicate`.
	///
	/// ## Panics
	///
	/// Panics if any of the scopes that dominate this one are not allocated in
	/// `context`.
	pub fn lookup_value<'ctx, P>(
		&'ctx self,
		context: &'ctx Context,
		predicate: P,
	) -> Option<(ValueId, &'ctx Value)>
	where
		P: Fn(&Value) -> bool + Clone,
	{
		let local = self.find_value(predicate.clone());
		if local.is_some() || self.parent.is_none() {
			local
		} else {
			let parent = context
				.get_scope(self.parent.unwrap())
				.expect("scope is allocated in different context");
			parent.lookup_value(context, predicate)
		}
	}

	/// Searches for and returns the first value in this scope with a given name.
	pub fn find_value_with_name<S>(&self, name: S) -> Option<(ValueId, &Value)>
	where
		S: Borrow<String>,
	{
		self.find_value(|v| match v {
			Value::Named { name: n, .. } => n == name.borrow(),
			_ => false,
		})
	}

	/// Recursively searches through this and above scopes for the first value
	/// with a given name.
	///
	/// ## Panics
	///
	/// Panics if any of the scopes that dominate this one are not allocated in
	/// `context`.
	pub fn lookup_value_with_name<'ctx, S>(
		&'ctx self,
		context: &'ctx Context,
		name: S,
	) -> Option<(ValueId, &'ctx Value)>
	where
		S: Borrow<String>,
	{
		self.lookup_value(context, |v| match v {
			Value::Named { name: n, .. } => n == name.borrow(),
			_ => false,
		})
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

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_value_lookup() {
		let mut context = Context::new();
		let s1 = context.alloc_scope(Scope::new(None));
		let s2 = context.alloc_scope(Scope::new(Some(s1)));

		let v1 = context.get_scope_mut(s1).unwrap().alloc_value(Value::new(
			"a".to_string(),
			None,
			false,
		));
		let (v2, _) = context
			.get_scope(s2)
			.unwrap()
			.lookup_value_with_name(&context, &String::from("a"))
			.expect("failed to lookup value");

		assert_eq!(v1, v2);
	}
}
