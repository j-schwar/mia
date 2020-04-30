use super::traits::MaybeNamed;
use id_arena::Id;

/// Contains data about a type.
#[derive(Debug, Eq, PartialEq)]
pub struct Type {
	name: String,
}

impl Type {
	/// Constructs a new type instance.
	pub fn new<S>(name: S) -> Self
	where
		S: Into<String>,
	{
		Type { name: name.into() }
	}
}

impl MaybeNamed for Type {
	fn name(&self) -> Option<&str> {
		Some(&self.name)
	}
}

pub type TypeId = Id<Type>;
