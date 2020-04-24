use id_arena::Id;

/// Contains data about a type.
#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	Simple {
		name: String,
	},

	Pointer {
		type_value: TypeId,
		is_mutable: bool,
	},

	Reference {
		type_value: TypeId,
		is_mutable: bool,
	},
}

impl Type {
	/// Constructs a new `Simple` named type.
	pub fn new(name: String) -> Self {
		Type::Simple { name }
	}

	/// Constructs a new pointer type.
	pub fn new_pointer(type_value: TypeId, is_mutable: bool) -> Self {
		Type::Pointer {
			type_value,
			is_mutable,
		}
	}

	/// Constructs a new reference type.
	pub fn new_reference(type_value: TypeId, is_mutable: bool) -> Self {
		Type::Reference {
			type_value,
			is_mutable,
		}
	}

	/// True if `self` is a pointer type.
	pub fn is_pointer(&self) -> bool {
		match self {
			Type::Pointer { .. } => true,
			_ => false,
		}
	}

	/// True if `self` is a reference type.
	pub fn is_reference(&self) -> bool {
		match self {
			Type::Reference { .. } => true,
			_ => false,
		}
	}
}

/// An identifier passed around instead of a data reference.
///
/// See [`Type`] for information about the actual data object.
///
/// [`Type`]: ../struct.Type.html
pub type TypeId = Id<Type>;
