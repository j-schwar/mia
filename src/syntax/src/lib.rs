pub mod lex;
pub mod parser;

use error::{Span, Spanning};
pub use lex::{Lexer, Payload, Token, TokenKind};
pub use parser::{ParseError, Parser};
use std::convert::TryFrom;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
	pub functions: Vec<Function>,
}

impl Program {
	/// Constructs a new program.
	pub fn new(functions: Vec<Function>) -> Self {
		Program { functions }
	}
}

impl Spanning for Program {
	fn span(&self) -> Span {
		if self.functions.is_empty() {
			return Span::new(0, 0);
		}

		let first = self.functions.first().unwrap();
		let last = self.functions.last().unwrap();
		return Span::combine(first.span(), last.span());
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	pub ident: Identifier,
	pub parameters: Vec<(Identifier, Identifier)>,
	pub return_type: Option<Identifier>,
	pub body: FunctionBody,
	span: Span,
}

impl Function {
	/// Constructs a new function definition.
	pub fn new<B>(
		ident: Identifier,
		parameters: Vec<(Identifier, Identifier)>,
		return_type: Option<Identifier>,
		body: B,
		span: Span,
	) -> Self
	where
		B: Into<FunctionBody>,
	{
		Function {
			ident,
			parameters,
			return_type,
			body: body.into(),
			span,
		}
	}
}

impl Spanning for Function {
	fn span(&self) -> Span {
		self.span
	}
}

/// AST function body node.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionBody {
	Expr(Expression),
	Block {
		statements: Vec<Statement>,
		span: Span,
	},
}

impl FunctionBody {
	/// Constructs a new block-style function body.
	pub fn new_block(statements: Vec<Statement>, span: Span) -> Self {
		FunctionBody::Block { statements, span }
	}
}

impl<E> From<E> for FunctionBody
where
	E: Into<Expression>,
{
	/// Constructs a new expression-style function body.
	fn from(expr: E) -> Self {
		FunctionBody::Expr(expr.into())
	}
}

impl Spanning for FunctionBody {
	fn span(&self) -> Span {
		use FunctionBody::*;
		match self {
			Expr(e) => e.span(),
			Block { span, .. } => *span,
		}
	}
}

/// AST statement node.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Expr(Expression),

	Define {
		is_mutable: bool,
		ident: Identifier,
		typename: Identifier,
		expr: Expression,
		span: Span,
	},

	Assign {
		ident: Identifier,
		expr: Expression,
		span: Span,
	},

	If {
		condition: Expression,
		block: Vec<Statement>,
		span: Span,
	},

	IfElse {
		condition: Expression,
		then_block: Vec<Statement>,
		else_block: Vec<Statement>,
		span: Span,
	},

	Ret {
		expr: Expression,
		span: Span,
	},
}

impl Statement {
	/// Constructs a new `Define` statement.
	pub fn new_define<E>(
		is_mutable: bool,
		ident: Identifier,
		typename: Identifier,
		expr: E,
		span: Span,
	) -> Self
	where
		E: Into<Expression>,
	{
		Statement::Define {
			is_mutable,
			ident,
			typename,
			expr: expr.into(),
			span,
		}
	}

	/// Constructs a new `Assign` statement.
	pub fn new_assign<E>(ident: Identifier, expr: E, span: Span) -> Self
	where
		E: Into<Expression>,
	{
		Statement::Assign {
			ident,
			expr: expr.into(),
			span,
		}
	}

	/// Constructs a new `If` statement.
	pub fn new_if<E>(condition: E, block: Vec<Statement>, span: Span) -> Self
	where
		E: Into<Expression>,
	{
		Statement::If {
			condition: condition.into(),
			block,
			span,
		}
	}

	/// Constructs a new `IfElse` statement.
	pub fn new_if_else<E>(
		condition: E,
		then_block: Vec<Statement>,
		else_block: Vec<Statement>,
		span: Span,
	) -> Self
	where
		E: Into<Expression>,
	{
		Statement::IfElse {
			condition: condition.into(),
			then_block,
			else_block,
			span,
		}
	}

	/// Constructs a new `Ret` statement.
	pub fn new_ret<E>(expr: E, span: Span) -> Self
	where
		E: Into<Expression>,
	{
		Statement::Ret {
			expr: expr.into(),
			span,
		}
	}
}

impl<E> From<E> for Statement
where
	E: Into<Expression>,
{
	fn from(e: E) -> Self {
		Statement::Expr(e.into())
	}
}

impl Spanning for Statement {
	fn span(&self) -> Span {
		use Statement::*;
		match self {
			Expr(e) => e.span(),
			Define { span, .. } => *span,
			Assign { span, .. } => *span,
			If { span, .. } => *span,
			IfElse { span, .. } => *span,
			Ret { span, .. } => *span,
		}
	}
}

/// AST expression node.
///
/// Note that at strict precedence is not enforced by the AST structure; all
/// expressions, regardless of precedence are simply lumped into this abstract
/// data type. Instead, we rely on the parser to enforce precedence when
/// constructing the AST.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	Ident(Identifier),

	Lit(Literal),

	BinOp {
		op: Operator<BinaryOp>,
		lhs: Box<Expression>,
		rhs: Box<Expression>,
	},

	UnOp {
		op: Operator<UnaryOp>,
		operand: Box<Expression>,
	},

	Call {
		func_name: Identifier,
		args: Vec<Expression>,
		span: Span,
	},
}

impl Expression {
	/// Constructs a new binary operation expression node.
	pub fn new_binary_op<Lhs, Rhs>(op: Operator<BinaryOp>, lhs: Lhs, rhs: Rhs) -> Self
	where
		Lhs: Into<Expression>,
		Rhs: Into<Expression>,
	{
		Expression::BinOp {
			op,
			lhs: Box::new(lhs.into()),
			rhs: Box::new(rhs.into()),
		}
	}

	/// Constructs a new unary operator expression node.
	pub fn new_unary_op<O>(op: Operator<UnaryOp>, operand: O) -> Self
	where
		O: Into<Expression>,
	{
		Expression::UnOp {
			op,
			operand: Box::new(operand.into()),
		}
	}

	/// Constructs a new function call expression node.
	pub fn new_call(func_name: Identifier, args: Vec<Expression>, span: Span) -> Self {
		Expression::Call {
			func_name,
			args,
			span,
		}
	}
}

impl From<Identifier> for Expression {
	fn from(ident: Identifier) -> Self {
		Expression::Ident(ident)
	}
}

impl From<Literal> for Expression {
	fn from(lit: Literal) -> Self {
		Expression::Lit(lit)
	}
}

impl Spanning for Expression {
	fn span(&self) -> Span {
		use Expression::*;
		match self {
			Ident(i) => i.span(),
			Lit(l) => l.span(),
			BinOp { lhs, rhs, .. } => Span::combine(lhs.span(), rhs.span()),
			UnOp { op, operand } => Span::combine(op.span(), operand.span()),
			Call { span, .. } => *span,
		}
	}
}

/// Wrapper for operator types (e.g., `BinaryOp` and `UnaryOp`) providing an
/// additional `span` field.
#[derive(Clone, Debug, PartialEq)]
pub struct Operator<O> {
	pub op: O,
	span: Span,
}

impl<O> Operator<O> {
	/// Constructs a new operator node.
	pub fn new(op: O, span: Span) -> Self {
		Operator { op, span }
	}
}

impl<O> Spanning for Operator<O> {
	fn span(&self) -> Span {
		self.span
	}
}

/// The set of binary operators supported by the language.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
	/* Arithmetic */
	Add, // +
	Sub, // -
	Mul, // *
	Div, // /
	Rem, // %

	/* Bitwise */
	BitAnd, // &
	BitOr,  // |
	BitXor, // ^

	/* Logic */
	Eq, // ==
	Ne, // !=
	Gt, // >
	Ge, // >=
	Lt, // <
	Le, // <=
}

impl TryFrom<TokenKind> for BinaryOp {
	type Error = ();

	fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
		match value {
			TokenKind::Plus => Ok(BinaryOp::Add),
			TokenKind::Minus => Ok(BinaryOp::Sub),
			TokenKind::Star => Ok(BinaryOp::Mul),
			TokenKind::Slash => Ok(BinaryOp::Div),
			TokenKind::Percent => Ok(BinaryOp::Rem),
			TokenKind::And => Ok(BinaryOp::BitAnd),
			TokenKind::Bar => Ok(BinaryOp::BitOr),
			TokenKind::Hat => Ok(BinaryOp::BitXor),
			TokenKind::Eq => Ok(BinaryOp::Eq),
			TokenKind::Ne => Ok(BinaryOp::Ne),
			TokenKind::Gt => Ok(BinaryOp::Gt),
			TokenKind::Ge => Ok(BinaryOp::Ge),
			TokenKind::Lt => Ok(BinaryOp::Lt),
			TokenKind::Le => Ok(BinaryOp::Le),
			_ => Err(()),
		}
	}
}

/// The set of unary operators supported by the language.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
	/* Arithmetic */
	Neg, // -

	/* Bitwise */
	BitNot, // ~

	/* Logic */
	Not, // !
}

impl TryFrom<TokenKind> for UnaryOp {
	type Error = ();

	fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
		match value {
			TokenKind::Minus => Ok(UnaryOp::Neg),
			TokenKind::Tilde => Ok(UnaryOp::BitNot),
			TokenKind::Exclamation => Ok(UnaryOp::Not),
			_ => Err(()),
		}
	}
}

/// AST literal node.
#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
	pub kind: LiteralKind,
	span: Span,
}

impl Literal {
	/// Constructs a new literal node.
	pub fn new(kind: LiteralKind, span: Span) -> Self {
		Literal { kind, span }
	}
}

impl Spanning for Literal {
	fn span(&self) -> Span {
		self.span
	}
}

/// Kind discriminant for AST literal nodes.
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
	Int(i64),
	Bool(bool),
}

/// AST identifier node.
///
/// Represents a user defined identifier for a variable/function/type/etc.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	pub text: String,
	span: Span,
}

impl Identifier {
	/// Constructs a new identifier.
	pub fn new<S>(text: S, span: Span) -> Self
	where
		S: Into<String>,
	{
		Identifier {
			text: text.into(),
			span,
		}
	}
}

impl Spanning for Identifier {
	fn span(&self) -> Span {
		self.span
	}
}
