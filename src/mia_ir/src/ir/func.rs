use id_arena::Id;

use super::scope::ScopeId;
use super::traits::MaybeNamed;
use super::type_value::TypeId;
use super::value::ValueId;

/// Holds type/value information about a function definition along with its
/// scope.
pub struct Function {
	/// The name of the function.
	pub name: String,

	/// List of the function's parameters.
	///
	/// Type information can be extracted for the value nodes.
	pub parameters: Vec<ValueId>,

	/// The return type of the function if present.
	pub return_type: TypeId,

	/// The id of the scope associated with this function.
	pub scope: ScopeId,
}

impl Function {
	pub fn new<S>(name: S, parameters: Vec<ValueId>, return_type: TypeId, scope: ScopeId) -> Self
	where
		S: Into<String>,
	{
		Function {
			name: name.into(),
			parameters,
			return_type,
			scope,
		}
	}
}

impl MaybeNamed for Function {
	fn name(&self) -> Option<&str> {
		Some(&self.name)
	}
}

pub type FunctionId = Id<Function>;
