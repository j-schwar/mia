//! IR typing system.
//!
//! Like with the grammar, the typing system is based on μL (micro-language)
//! by Raphael L. Proust.

use itertools::Itertools;
use std::collections::HashMap as Map;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// An IR type.
///
/// All values and variables in the IR language are associated with some type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
	/// A native type.
	Native(Native),

	/// A typename, to be dereferenced into an actual type.
	Typename(String),

	/// A a union-style type discriminated by discriminant names.
	Sum(Vec<(String, Type)>),

	/// A record-style type with named fields.
	Product(Vec<(String, Type)>),

	/// The unit type.
	Unit,
}

impl Type {
	/// Constructs a new native type.
	pub fn new_native(native: Native) -> Self {
		Type::Native(native)
	}

	/// The native integer type.
	pub fn int_type() -> Self {
		Type::Native(Native::Int)
	}

	/// The native boolean type.
	pub fn bool_type() -> Self {
		Type::Native(Native::Bool)
	}

	/// Constructs a new typename.
	pub fn new_typename<S>(name: S) -> Self
	where
		S: Into<String>,
	{
		Type::Typename(name.into())
	}

	/// Constructs a new sum type.
	pub fn new_sum<I, S>(discriminants: I) -> Self
	where
		I: IntoIterator<Item = (S, Type)>,
		S: Into<String>,
	{
		Type::Sum(
			discriminants
				.into_iter()
				.map(|(k, v)| (k.into(), v))
				.collect(),
		)
	}

	/// Constructs a new product type.
	pub fn new_product<I, S>(fields: I) -> Self
	where
		I: IntoIterator<Item = (S, Type)>,
		S: Into<String>,
	{
		Type::Product(fields.into_iter().map(|(k, v)| (k.into(), v)).collect())
	}

	/// True if `self` is a `Native` variant.
	pub fn is_native(&self) -> bool {
		match self {
			Type::Native(_) => true,
			_ => false,
		}
	}

	/// True if `self` is a `Typename` variant.
	pub fn is_typename(&self) -> bool {
		match self {
			Type::Typename(_) => true,
			_ => false,
		}
	}

	/// True if `self` is a `Sum` variant.
	pub fn is_sum(&self) -> bool {
		match self {
			Type::Sum(_) => true,
			_ => false,
		}
	}

	/// True if `self` is a `Product` variant.
	pub fn is_product(&self) -> bool {
		match self {
			Type::Product(_) => true,
			_ => false,
		}
	}

	/// True if `self` is the unit type.
	pub fn is_unit(&self) -> bool {
		match self {
			Type::Unit => true,
			_ => false,
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Type::*;
		match self {
			Native(n) => n.fmt(f),

			Typename(name) => name.fmt(f),

			Sum(discriminants) => discriminants
				.iter()
				.map(|(name, t)| {
					if t.is_sum() {
						format!("{} ({})", name, t)
					} else {
						format!("{} {}", name, t)
					}
				})
				.join(" + ")
				.fmt(f),

			Product(fields) => {
				let fields = fields
					.iter()
					.map(|(name, t)| format!("{}: {}", name, t))
					.join(", ");
				write!(f, "{{ {} }}", fields)
			}

			Unit => write!(f, "{{}}"),
		}
	}
}

impl From<Native> for Type {
	fn from(native: Native) -> Self {
		Type::Native(native)
	}
}

/// Types which are native to the language.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Native {
	Int,
	Bool,
}

impl Display for Native {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Native::*;
		match self {
			Int => "int",
			Bool => "bool",
		}
		.fmt(f)
	}
}

/// The type environment is a mapping of type names (i.e., `Type::Typename`
/// variants) to actual types.
pub type TypeEnv = Map<String, Type>;
