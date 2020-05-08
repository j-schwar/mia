//! Helper utilities for constructing IR code.

use crate::grammar::*;
use crate::types::*;

/// A helper object which aids in the construction of IR code.
pub struct Builder {
	// type_environment: TypeEnv,
	program: Program,
	active_definition: Option<Definition>,
}

impl Builder {
	/// Constructs a new builder.
	pub fn new() -> Self {
		Builder {
			// type_environment: TypeEnv::new(),
			program: Program::new(vec![]),
			active_definition: None,
		}
	}

	/// Constructs a new function definition with an undefined body and sets it
	/// as the builder's active definition.
	///
	/// # Parameters
	///
	/// * `name`: The name of the function being defined.
	///
	/// * `parameters`: An iterator over variable-type pairs which defines the
	/// parameters that the function accepts.
	///
	/// * `return_type`: The return type of the function.
	///
	pub fn new_definition<S, I, V>(&mut self, name: S, parameters: I, return_type: Type)
	where
		S: Into<String>,
		I: IntoIterator<Item = (V, Type)>,
		V: Into<Variable>,
	{
		let def = Definition::new(name, parameters, return_type, Term::Undefined);
		self.active_definition = Some(def);
	}

	/// Finalizes the builder's active definition appending it to the program
	/// being built.
	///
	/// Note that the active definition is not considered apart of the program
	/// until this method is called.
	///
	/// # Panics
	///
	/// Panics if any of the following conditions are true:
	///
	/// * There is no active definition.
	///
	/// * The active definition contains an undefined term.
	///
	pub fn finalize_definition(&mut self) {
		for term in self
			.active_definition
			.as_ref()
			.expect("no active definition")
			.terms()
		{
			if term.is_undefined() {
				panic!(
					"finalizing definition with undefined term:\n{}",
					self.active_definition.as_ref().unwrap()
				);
			}
		}

		let def = self.active_definition.take().unwrap();
		self.program.definitions.push(def);
	}
}
