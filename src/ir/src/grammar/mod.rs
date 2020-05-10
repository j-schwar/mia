//! Outlines the grammar for the intermediate language.
//!
//! Based on μL (micro-language) described in "ASAP As Static As Possible
//! Memory Management" by Raphael L. Proust at the University of Cambridge.

use crate::display::{ContextualDisplay, IndentContext};
use crate::types::Native;
use crate::types::Type;
use itertools::Itertools;
use std::collections::HashMap as Map;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub mod iter;
use iter::Terms;

/// A literal constant value.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
	/// An integer literal.
	///
	/// Note that we don't distinguish between integer sizes. This may change in
	/// future iterations.
	Int(i64),

	/// A boolean literal.
	Bool(bool),
}

impl Literal {
	/// Returns the type of this literal.
	pub fn get_type(&self) -> Type {
		use Literal::*;
		match self {
			Int(_) => Type::Native(Native::Int),
			Bool(_) => Type::Native(Native::Bool),
		}
	}
}

impl Display for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Literal::*;
		match self {
			Int(i) => write!(f, "{}", i),
			Bool(b) => write!(f, "{}", b),
		}
	}
}

/// A named variable.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Variable {
	pub name: String,
}

impl Variable {
	/// Constructs a new variable.
	pub fn new<S>(name: S) -> Self
	where
		S: Into<String>,
	{
		Variable { name: name.into() }
	}
}

impl Display for Variable {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.name)
	}
}

impl<S> From<S> for Variable
where
	S: Into<String>,
{
	/// Converts a string into a variable.
	fn from(name: S) -> Self {
		Variable::new(name)
	}
}

/// A value/pattern used in an expression/match context.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	/// A constant literal value.
	Literal(Literal),

	/// A variable value.
	Variable(Variable),

	/// A variant of a sum type.
	SumVariant {
		discriminant_name: String,
		variable: Variable,
	},

	/// A record value.
	Record { fields: Map<String, Variable> },

	/// The unit value.
	///
	/// Analogous to an empty record value.
	Unit,
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Value::*;
		match self {
			Literal(l) => l.fmt(f),

			Variable(v) => v.fmt(f),

			SumVariant {
				discriminant_name,
				variable,
			} => write!(f, "{} {}", discriminant_name, variable),

			Record { fields } => {
				let fields = fields
					.iter()
					.map(|(k, v)| format!("{} = {}", k, v))
					.join("; ");
				write!(f, "{{ {} }}", fields)
			}

			Unit => write!(f, "{{}}"),
		}
	}
}

impl From<Literal> for Value {
	fn from(l: Literal) -> Self {
		Value::Literal(l)
	}
}

impl<V> From<V> for Value
where
	V: Into<Variable>,
{
	fn from(v: V) -> Self {
		Value::Variable(v.into())
	}
}

/// Values may also be used as patterns in match contexts.
pub type Pattern = Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	/// A single value expression.
	Value(Value),

	/// A builtin operation (e.g., arithmetic add, logical and, etc.).
	Operation {
		operator: Operator,
		operands: Vec<Variable>,
	},

	/// A function call.
	Call {
		name: String,
		arguments: Vec<Variable>,
	},
}

impl Expression {
	/// Constructs a new unary operation.
	pub fn new_unary<V>(operator: Operator, operand: V) -> Self
	where
		V: Into<Variable>,
	{
		Expression::Operation {
			operator,
			operands: vec![operand.into()],
		}
	}

	/// Constructs a new binary operation.
	pub fn new_binary<V1, V2>(operator: Operator, lhs: V1, rhs: V2) -> Self
	where
		V1: Into<Variable>,
		V2: Into<Variable>,
	{
		Expression::Operation {
			operator,
			operands: vec![lhs.into(), rhs.into()],
		}
	}

	/// Constructs a new call expression.
	pub fn new_call<S, V>(name: S, arguments: Vec<V>) -> Self
	where
		S: Into<String>,
		V: Into<Variable>,
	{
		Expression::Call {
			name: name.into(),
			arguments: arguments.into_iter().map(|v| v.into()).collect(),
		}
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Expression::*;
		match self {
			Value(v) => v.fmt(f),

			Operation { operator, operands } => {
				// Note: panic if not enough operands are present in the expression
				match operator {
					Operator::Neg | Operator::Not => write!(f, "{}{}", operator, operands[0]),
					_ => write!(f, "{} {} {}", operands[0], operator, operands[1]),
				}
			}

			Call { name, arguments } => {
				let arguments = arguments.iter().map(|a| format!("{}", a)).join(", ");
				write!(f, "{}({})", name, arguments)
			}
		}
	}
}

impl<V> From<V> for Expression
where
	V: Into<Value>,
{
	fn from(v: V) -> Self {
		Expression::Value(v.into())
	}
}

/// Builtin operators.
///
/// Not that we don't distinguish between unary and binary operators for
/// simplicity's sake.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operator {
	/* Arithmetic Binary */
	Add,
	Sub,
	Mul,
	Div,
	Mod,

	/* Arithmetic Unary */
	Neg,

	/* Logical Binary */
	And,
	Or,
	Eq,
	Ne,
	Gt,
	Ge,
	Lt,
	Le,

	/* Logical Unary */
	Not,
}

impl Display for Operator {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use Operator::*;
		match self {
			Add => "+",
			Sub | Neg => "-",
			Mul => "*",
			Div => "/",
			Mod => "%",
			And => "&&",
			Or => "||",
			Eq => "==",
			Ne => "!=",
			Gt => ">",
			Ge => ">=",
			Lt => "<",
			Le => "<=",
			Not => "!",
		}
		.fmt(f)
	}
}

/// A program statement.
///
/// This IR language uses a continuation passing style of program structure.
#[derive(Clone, Debug, PartialEq)]
pub enum Term {
	/// The `Bind` term introduces a new variable into the program binding it to
	/// the result of some expression.
	Bind {
		/// The variable to bind the result of some expression to.
		variable: Variable,

		/// The type of the variable being bound.
		type_annotation: Type,

		/// The expression to bind.
		expression: Expression,

		/// The continuation of the program.
		continuation: Box<Term>,
	},

	/// The one control flow construct in the IR language.
	///
	/// Branches on the first pattern which matches `target`.
	Match {
		/// The target variable to match.
		target: Variable,

		/// The various possible branches to take.
		branches: Vec<(Pattern, Term)>,
	},

	/// Returns from a function with a specified value.
	Return(Variable),

	/// A placeholder term utilized by the IR builder to enable forward
	/// construction of IR code.
	Undefined,
}

impl Term {
	/// Constructs a new bind term.
	pub fn new_bind<V, T, E>(
		variable: V,
		type_annotation: T,
		expression: E,
		continuation: Term,
	) -> Self
	where
		V: Into<Variable>,
		T: Into<Type>,
		E: Into<Expression>,
	{
		Term::Bind {
			variable: variable.into(),
			type_annotation: type_annotation.into(),
			expression: expression.into(),
			continuation: Box::new(continuation),
		}
	}

	/// Constructs a new match term.
	pub fn new_match<V>(target: V, branches: Vec<(Pattern, Term)>) -> Self
	where
		V: Into<Variable>,
	{
		Term::Match {
			target: target.into(),
			branches,
		}
	}

	/// Constructs a new return term.
	pub fn new_return<V>(variable: V) -> Self
	where
		V: Into<Variable>,
	{
		Term::Return(variable.into())
	}

	/// True if `self` is a `Bind` variant.
	pub fn is_bind(&self) -> bool {
		match self {
			Term::Bind { .. } => true,
			_ => false,
		}
	}

	/// True if `self` is a `Match` variant.
	pub fn is_match(&self) -> bool {
		match self {
			Term::Match { .. } => true,
			_ => false,
		}
	}

	/// True if `self` is a `Return` variant.
	pub fn is_return(&self) -> bool {
		match self {
			Term::Return(_) => true,
			_ => false,
		}
	}

	/// True if `self` is an `Undefined` variant.
	pub fn is_undefined(&self) -> bool {
		match self {
			Term::Undefined => true,
			_ => false,
		}
	}

	/// Returns an iterator over the term sequence rooted at this term.
	pub fn iter(&self) -> Terms {
		Terms::new(self)
	}
}

impl ContextualDisplay<IndentContext> for Term {
	fn fmt(&self, mut context: IndentContext, f: &mut Formatter<'_>) -> FmtResult {
		use Term::*;
		match self {
			Bind {
				variable,
				type_annotation,
				expression,
				continuation,
			} => {
				write!(
					f,
					"{}let {}: {} = {} in\n",
					context, variable, type_annotation, expression
				)?;
				continuation.fmt(context, f)
			}

			Match { target, branches } => {
				write!(f, "{}match {} {{\n", context, target)?;
				context.increase_indent();
				for (pattern, term) in branches {
					write!(f, "{}{} =>\n", context, pattern)?;
					context.increase_indent();
					term.fmt(context.clone(), f)?;
					context.decrease_indent();
					write!(f, ";\n")?;
				}
				context.decrease_indent();
				write!(f, "{}}}", context)
			}

			Return(v) => write!(f, "{}ret {}", context, v),

			Undefined => write!(f, "undefined"),
		}
	}
}

/// A top-level function definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
	/// The name of the function being defined.
	pub name: String,

	/// A list of parameters that the function takes.
	pub parameters: Vec<(Variable, Type)>,

	/// The return type of the function.
	pub return_type: Type,

	/// The body of the function.
	pub term: Term,
}

impl Definition {
	/// Constructs a new function definition.
	pub fn new<S, P, V, T>(name: S, parameters: P, return_type: T, term: Term) -> Self
	where
		S: Into<String>,
		P: IntoIterator<Item = (V, Type)>,
		V: Into<Variable>,
		T: Into<Type>,
	{
		Definition {
			name: name.into(),
			parameters: parameters.into_iter().map(|(v, t)| (v.into(), t)).collect(),
			return_type: return_type.into(),
			term,
		}
	}

	/// Returns an iterator over the terms in this definition.
	///
	/// `Match` terms are explored in a depth-first manor.
	pub fn terms(&self) -> Terms {
		Terms::new(&self.term)
	}
}

impl ContextualDisplay<IndentContext> for Definition {
	fn fmt(&self, mut context: IndentContext, f: &mut Formatter<'_>) -> FmtResult {
		let parameters = self
			.parameters
			.iter()
			.map(|(v, t)| format!("{}: {}", v, t))
			.join(", ");
		write!(
			f,
			"fun {}({}) -> {} =\n",
			self.name, parameters, self.return_type
		)?;
		context.increase_indent();
		self.term.fmt(context, f)
	}
}

impl Display for Definition {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		let context = IndentContext::two_spaces();
		ContextualDisplay::fmt(self, context, f)
	}
}

/// A program is simply a list of definitions.
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
	pub definitions: Vec<Definition>,
}

impl Program {
	/// Constructs a new program from a sequence of definitions.
	pub fn new(definitions: Vec<Definition>) -> Self {
		Program { definitions }
	}
}

impl Display for Program {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		for (i, definition) in self.definitions.iter().enumerate() {
			write!(f, "{}", definition)?;
			if i != self.definitions.len() - 1 {
				write!(f, "\n\n")?;
			}
		}
		Ok(())
	}
}
