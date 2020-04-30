use super::func::Function;
use super::scope::Scope;
use super::traits::IdArena;
use super::type_value::Type;
use super::value::Value;
use id_arena::Arena;

/// An IR module contains all of the functions/scopes/types/variables which
/// make up a single translation unit.
pub struct Module {
	function_arena: Arena<Function>,
	scope_arena: Arena<Scope>,
	type_arena: Arena<Type>,
	value_arena: Arena<Value>,
}

impl Module {
	pub fn new() -> Self {
		Module {
			function_arena: Arena::new(),
			scope_arena: Arena::new(),
			type_arena: Arena::new(),
			value_arena: Arena::new(),
		}
	}
}

impl IdArena<Function> for Module {
	fn arena(&self) -> &Arena<Function> {
		&self.function_arena
	}

	fn arena_mut(&mut self) -> &mut Arena<Function> {
		&mut self.function_arena
	}
}

impl IdArena<Scope> for Module {
	fn arena(&self) -> &Arena<Scope> {
		&self.scope_arena
	}

	fn arena_mut(&mut self) -> &mut Arena<Scope> {
		&mut self.scope_arena
	}
}

impl IdArena<Type> for Module {
	fn arena(&self) -> &Arena<Type> {
		&self.type_arena
	}

	fn arena_mut(&mut self) -> &mut Arena<Type> {
		&mut self.type_arena
	}
}

impl IdArena<Value> for Module {
	#[inline]
	fn arena(&self) -> &Arena<Value> {
		&self.value_arena
	}

	#[inline]
	fn arena_mut(&mut self) -> &mut Arena<Value> {
		&mut self.value_arena
	}
}
