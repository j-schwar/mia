use super::traits::MaybeNamed;
use id_arena::Id;

/// Contains data about a type.
#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	/// A built in type native to the language.
	Builtin { name: &'static str },

	/// An unknown type to be filled in later.
	Implicit { id: TypeId },

	/// The type of integer literals.
	///
	/// We require a custom type here as we want to be able to implicitly convert
	/// integer literals into any of the native integer types.
	IntegerLiteralType,
}

impl MaybeNamed for Type {
	fn name(&self) -> Option<&str> {
		match self {
			Type::Builtin { name, .. } => Some(*name),
			_ => None,
		}
	}
}

pub type TypeId = Id<Type>;
