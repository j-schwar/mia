use super::traits::MaybeNamed;
use id_arena::Id;

/// Contains data about a type.
#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	Builtin { name: &'static str },

	Implicit { id: TypeId },
}

impl MaybeNamed for Type {
	fn name(&self) -> Option<&str> {
		match self {
			Type::Builtin { name, .. } => Some(*name),
			Type::Implicit { .. } => None,
		}
	}
}

pub type TypeId = Id<Type>;
