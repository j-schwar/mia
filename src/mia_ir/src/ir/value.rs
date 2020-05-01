use super::traits::MaybeNamed;
use super::type_value::TypeId;
use id_arena::Id;

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
	/// A value representing some user-defined named variable.
	Named {
		name: String,
		type_value: TypeId,
		is_mutable: bool,
	},

	/// An unnamed, temporary value generated by the compiler.
	///
	/// Such values are generated to hold the intermediate results of expressions.
	Temporary { id: ValueId, type_value: TypeId },

	/// A literal value which holds a constant value.
	Literal {
		kind: LiteralKind,
		type_value: TypeId,
	},
}

impl Value {
	/// Constructs a new named value.
	pub fn new(name: String, type_value: TypeId, is_mutable: bool) -> Self {
		Value::Named {
			name,
			type_value,
			is_mutable,
		}
	}

	/// Constructs a new temporary value.
	pub fn new_temporary(id: ValueId, type_value: TypeId) -> Self {
		Value::Temporary { id, type_value }
	}

	/// The type of `self`.
	pub fn get_type(&self) -> TypeId {
		match self {
			Value::Named { type_value, .. } => *type_value,
			Value::Temporary { type_value, .. } => *type_value,
			Value::Literal { type_value, .. } => *type_value,
		}
	}

	/// Returns a string representation of `self`.
	pub fn as_string(&self) -> String {
		match self {
			Value::Named { name, .. } => name.clone(),
			Value::Temporary { id, .. } => format!("${}", id.index()),
			Value::Literal { kind, .. } => match kind {
				LiteralKind::Int { value } => format!("{}", value),
			},
		}
	}
}

impl MaybeNamed for Value {
	fn name(&self) -> Option<&str> {
		match self {
			Value::Named { name, .. } => Some(name.as_str()),
			_ => None,
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum LiteralKind {
	Int { value: u64 },
}

pub type ValueId = Id<Value>;
