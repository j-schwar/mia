use super::instruction::Instruction;
use super::module::Module;
use super::traits::{IdArena, MaybeNamed};
use super::value::{Value, ValueId};
use id_arena::Id;

/// IR scope; simply it is a sequence of instructions along with a collection
/// of variables which may be referenced in child scopes.
#[derive(Debug, Eq, PartialEq)]
pub struct Scope {
	/// This scope's parent scope if it exists.
	pub parent: Option<ScopeId>,

	/// The sequence of instructions defined in this scope.
	pub instructions: Vec<Instruction>,

	/// The collection of values that have been defined in this scope.
	///
	/// Lower scopes (i.e., scopes which are a decedent of this one) may freely
	/// use theses values in their instructions as they are considered to sill
	/// be "in-scope".
	///
	/// We use a vector here rather than a set as the order in which these values
	/// are defined in is important. When searching through a scope to find a
	/// variable with a specific name, we want to find the most recent one. To
	/// do this, we iterate through this list in reverse order.
	pub values: Vec<ValueId>,
}

impl Scope {
	/// Constructs a new `Scope` instances.
	pub fn new(parent: Option<ScopeId>) -> Self {
		Scope {
			parent,
			instructions: Vec::new(),
			values: Vec::new(),
		}
	}

	/// Searches this scope for the most recent value which satisfies `predicate`.
	/// If unable to find such a value in this scope, parent scopes are
	/// recursively searched until such a value is found or there are no more
	/// scopes left to search.
	///
	/// ## Panics
	///
	/// Panics if any of the following conditions are true:
	///
	/// * Any of the values in `self.values` are not allocated in `module`. This
	/// goes for all of this scope's ancestors as well.
	///
	/// * Any of this scope's ancestors are not allocated in `module`.
	///
	pub fn search<'m, P>(&self, module: &'m Module, predicate: P) -> Option<ValueId>
	where
		P: Fn(&'m Value) -> bool + Clone,
	{
		for id in self.values.iter().rev() {
			let value = module.get_unwrap(*id);
			if predicate(value) {
				return Some(*id);
			}
		}

		if let Some(parent) = self.parent {
			module.get_unwrap(parent).search(module, predicate.clone())
		} else {
			None
		}
	}

	/// Recursively searches this and above scopes for the most recent value with
	/// a given name.
	///
	/// ## Panics
	///
	/// Panics if any of the following conditions are true:
	///
	/// * Any of the values in `self.values` are not allocated in `module`. This
	/// goes for all of this scope's ancestors as well.
	///
	/// * Any of this scope's ancestors are not allocated in `module`.
	///
	pub fn search_by_name(&self, module: &Module, name: &str) -> Option<ValueId> {
		self.search(module, |v| v.name() == Some(name))
	}

	/// Pushes a new value identifier into this scope.
	pub fn push_value(&mut self, id: ValueId) {
		self.values.push(id);
	}

	/// Pushes a new instruction into this scope.
	pub fn push_instruction(&mut self, instruction: Instruction) {
		self.instructions.push(instruction);
	}
}

/// A scope identifier.
pub type ScopeId = Id<Scope>;
