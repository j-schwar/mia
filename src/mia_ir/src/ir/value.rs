use super::type_value::TypeId;
use id_arena::Id;

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
	Named {
		name: String,
		type_value: Option<TypeId>,
		is_mutable: bool,
	},

	Temporary {
		id: ValueId,
		type_value: Option<TypeId>,
	},
}

impl Value {
	/// Constructs a new named value.
	pub fn new(name: String, type_value: Option<TypeId>, is_mutable: bool) -> Self {
		Value::Named {
			name,
			type_value,
			is_mutable,
		}
	}

	/// Constructs a new temporary value.
	pub fn new_temporary(id: ValueId, type_value: Option<TypeId>) -> Self {
		Value::Temporary { id, type_value }
	}

	/// The type of `self`.
	pub fn get_type(&self) -> Option<TypeId> {
		match self {
			Value::Named { type_value, .. } => *type_value,
			Value::Temporary { type_value, .. } => *type_value,
		}
	}
}

pub type ValueId = Id<Value>;
