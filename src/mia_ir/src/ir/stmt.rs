use super::func::FunctionId;
use super::scope::ScopeId;
use super::value::ValueId;
use mia_syntax::ast::{InfixOperator, PrefixOperator};

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
	pub assignee: ValueId,
	pub kind: StatementKind,
}

impl Statement {
	pub fn new_call(assignee: ValueId, function: FunctionId, arguments: Vec<ValueId>) -> Self {
		Statement {
			assignee,
			kind: StatementKind::Call {
				function,
				arguments,
			},
		}
	}

	pub fn new_unary_op(assignee: ValueId, op: UnaryOperation, value: ValueId) -> Self {
		Statement {
			assignee,
			kind: StatementKind::UnaryOp { op, value },
		}
	}

	pub fn new_binary_op(
		assignee: ValueId,
		op: BinaryOperation,
		lhs: ValueId,
		rhs: ValueId,
	) -> Self {
		Statement {
			assignee,
			kind: StatementKind::BinaryOp { op, lhs, rhs },
		}
	}

	pub fn new_if_else(
		assignee: ValueId,
		condition: ValueId,
		true_scope: ScopeId,
		false_scope: ScopeId,
	) -> Self {
		Statement {
			assignee,
			kind: StatementKind::IfElse {
				condition,
				true_scope,
				false_scope,
			},
		}
	}

	pub fn new_alias(assignee: ValueId, value: ValueId) -> Self {
		Statement {
			assignee,
			kind: StatementKind::Alias { value },
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum StatementKind {
	Call {
		function: FunctionId,
		arguments: Vec<ValueId>,
	},

	UnaryOp {
		op: UnaryOperation,
		value: ValueId,
	},

	BinaryOp {
		op: BinaryOperation,
		lhs: ValueId,
		rhs: ValueId,
	},

	IfElse {
		condition: ValueId,
		true_scope: ScopeId,
		false_scope: ScopeId,
	},

	Alias {
		value: ValueId,
	},
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperation {
	Not, // !
	Neg, // -
}

impl From<PrefixOperator> for UnaryOperation {
	/// Translates an `ast::PrefixOperator` into an IR `UnaryOperator`.
	fn from(op: PrefixOperator) -> Self {
		use PrefixOperator::*;
		match op {
			Neg => UnaryOperation::Neg,
			Not => UnaryOperation::Not,
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOperation {
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

impl From<InfixOperator> for BinaryOperation {
	/// Translates an `ast::InfixOperator` into an IR `BinaryOperator`.
	fn from(op: InfixOperator) -> Self {
		use InfixOperator::*;
		match op {
			Add => BinaryOperation::Add,
			Sub => BinaryOperation::Sub,
			Mul => BinaryOperation::Mul,
			Div => BinaryOperation::Div,
			Mod => BinaryOperation::Mod,
			Eq => BinaryOperation::Eq,
			Ne => BinaryOperation::Ne,
			And => BinaryOperation::And,
			Or => BinaryOperation::Or,
			Lt => BinaryOperation::Lt,
			Le => BinaryOperation::Le,
			Gt => BinaryOperation::Gt,
			Ge => BinaryOperation::Ge,
		}
	}
}
