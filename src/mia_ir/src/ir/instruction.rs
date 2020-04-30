use super::func::FunctionId;
use super::scope::ScopeId;
use super::value::ValueId;
use mia_syntax::ast::{InfixOperator, PrefixOperator};

/// An IR Instruction.
///
/// Unlike AST statements, which main contain arbitrarily recursive expressions,
/// IR instructions are linear and contain no recursive elements. This
/// simplistic approach allows for easier type-checking and compilation to LLVM.
#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
	/// A function call instruction.
	Call {
		/// The variable to hold the result of the function call.
		assignee: ValueId,

		/// The id of the function to call.
		function: FunctionId,

		/// The arguments to pass to the function.
		arguments: Vec<ValueId>,
	},

	/// A unary operation instruction.
	UnaryOp {
		/// The variable to hold the result of the operation.
		assignee: ValueId,

		/// The operator for this instruction.
		op: UnaryOperator,

		/// The operand for this instruction.
		operand: ValueId,
	},

	/// A binary operation instruction.
	BinaryOp {
		/// The variable to hold the result of the operation.
		assignee: ValueId,

		/// The operator for this instruction.
		op: BinaryOperator,

		/// The left operand for this instruction.
		lhs: ValueId,

		/// The right operand for this instruction.
		rhs: ValueId,
	},

	/// An if-else branching instruction.
	///
	/// In Mia, like as in Rust, if-else statements are allowed to act as
	/// expressions which produce a value. As such, if-else IR instructions
	/// include an `assignee` field.
	IfElse {
		/// The variable to hold the result of this expression.
		assignee: ValueId,

		/// The condition to branch on.
		condition: ValueId,

		/// The id of the scope which will be executed if `condition` is true.
		true_scope: ScopeId,

		/// The id of the scope which will be executed if `condition` is false.
		false_scope: ScopeId,
	},

	/// An aliasing instruction.
	///
	/// Unlike other instructions, the alias instruction does not involve any
	/// computation. It's sole purpose is to allow for code like the following:
	///
	/// ```text
	/// fun foo(a) {
	///   let b = a; // <-- an alias instruction is needed here
	///   // code...
	/// }
	/// ```
	Alias {
		/// The new variable to alias `value`.
		assignee: ValueId,

		/// The value being aliased.
		value: ValueId,
	},

	/// A return statement signifying the end of a scope.
	///
	/// Unlike the other instructions, return does not have an assignee field as
	/// no new variables need be created for this instruction.
	Return {
		/// The value to return.
		value: ValueId,
	},
}

impl Instruction {
	/// The `assignee` field of this instruction if present.
	///
	/// Whilst most instructions contain an assignee field, some, like the return
	/// instruction, do not hence the need for an optional return value.
	pub fn assignee(&self) -> Option<ValueId> {
		use Instruction::*;
		match self {
			Call { assignee, .. }
			| UnaryOp { assignee, .. }
			| BinaryOp { assignee, .. }
			| IfElse { assignee, .. }
			| Alias { assignee, .. } => Some(*assignee),

			Return { .. } => None,
		}
	}

	/// A list of all the values which this instruction depends on.
	///
	/// This includes things like the arguments to a function call of, the left
	/// and right operands for a binary operation, etc.
	pub fn operands(&self) -> Vec<ValueId> {
		use Instruction::*;
		match self {
			Call { arguments, .. } => arguments.clone(),
			UnaryOp { operand, .. } => vec![*operand],
			BinaryOp { lhs, rhs, .. } => vec![*lhs, *rhs],
			IfElse { condition, .. } => vec![*condition],
			Alias { value, .. } => vec![*value],
			Return { value } => vec![*value],
		}
	}
}

/// IR unary operator.
#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Not, // !
	Neg, // -
}

impl From<PrefixOperator> for UnaryOperator {
	/// Translates an `ast::PrefixOperator` into an IR `UnaryOperator`.
	fn from(op: PrefixOperator) -> Self {
		use PrefixOperator::*;
		match op {
			Neg => UnaryOperator::Neg,
			Not => UnaryOperator::Not,
		}
	}
}

/// IR binary operator.
#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOperator {
	/* Arithmetical Operations */
	Add, // +
	Sub, // -
	Mul, // *
	Div, // /
	Mod, // %

	/* Logical Operations */
	Eq,  // ==
	Ne,  // !=
	And, // &&,
	Or,  // ||
	Lt,  // <
	Le,  // <=
	Gt,  // >
	Ge,  // >=
}

impl From<InfixOperator> for BinaryOperator {
	/// Translates an `ast::InfixOperator` into an IR `BinaryOperator`.
	fn from(op: InfixOperator) -> Self {
		use InfixOperator::*;
		match op {
			Add => BinaryOperator::Add,
			Sub => BinaryOperator::Sub,
			Mul => BinaryOperator::Mul,
			Div => BinaryOperator::Div,
			Mod => BinaryOperator::Mod,
			Eq => BinaryOperator::Eq,
			Ne => BinaryOperator::Ne,
			And => BinaryOperator::And,
			Or => BinaryOperator::Or,
			Lt => BinaryOperator::Lt,
			Le => BinaryOperator::Le,
			Gt => BinaryOperator::Gt,
			Ge => BinaryOperator::Ge,
		}
	}
}
