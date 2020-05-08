use std::marker::PhantomData;

pub mod builder;
pub mod codegen;
pub mod display;
pub mod grammar;
pub mod types;

/// Generates objects of type `T` with unique names.
///
/// Generated names are in the form "$x" where 'x' is some number.
pub struct UniqueNameGenerator<T>
where
	T: From<String>,
{
	next: usize,
	_marker: PhantomData<T>,
}

impl<T> UniqueNameGenerator<T>
where
	T: From<String>,
{
	pub fn new() -> Self {
		UniqueNameGenerator {
			next: 0,
			_marker: PhantomData,
		}
	}

	/// Generates a unique name.
	pub fn generate_name(&mut self) -> String {
		let name = format!("${}", self.next);
		self.next += 1;
		return name;
	}

	/// Generates an object with a unique name.
	pub fn generate(&mut self) -> T {
		T::from(self.generate_name())
	}
}
