//! This module defines the Abstract Syntax Tree for the Mia language.

use crate::ast::visit::{MutVisitor, Transformer};

pub mod pass;
pub mod visit;

pub trait Ast {
	/// Applies a `MutVisitor` to this node.
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V);

	/// Applies a `Transformer` to this node.
	fn transform<T: Transformer>(self, t: &mut T) -> Self;
}

/// An identifier used to uniquely name variables, functions, and types.
///
/// Valid identifiers must match the following regular expression:
///
/// ```text
/// [_a-zA-Z][_a-zA-Z0-9]*
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
	pub name: String,
}

impl Ident {
	pub fn new(name: String) -> Self {
		Ident { name }
	}
}

impl From<String> for Ident {
	fn from(name: String) -> Self {
		Ident::new(name)
	}
}

impl From<&str> for Ident {
	fn from(name: &str) -> Self {
		Ident::new(name.to_string())
	}
}

impl Ast for Ident {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_ident(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_ident(self)
	}
}

/// A type identifier used when declaring the type of variables, parameters,
/// etc.
///
/// Type identifiers follow the following syntactic format:
///
/// ```text
/// type-value ::= [ "&" | "*" ] [ "mut" ] identifier ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeValue {
	pub name: Ident,
	pub indirection: TypeIndirection,
	mutable: bool,
}

impl TypeValue {
	pub fn new<I: Into<Ident>>(ident: I, indirection: TypeIndirection, mutable: bool) -> Self {
		TypeValue {
			name: ident.into(),
			indirection,
			mutable,
		}
	}

	/// True if `self` is a reference type.
	pub fn is_ref(&self) -> bool {
		self.indirection == TypeIndirection::Reference
	}

	/// True if `self` is a pointer type.
	pub fn is_ptr(&self) -> bool {
		self.indirection == TypeIndirection::Pointer
	}

	/// True if `self` is a mutable type.
	pub fn is_mut(&self) -> bool {
		self.mutable
	}
}

impl Ast for TypeValue {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_type_value(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_type_value(self)
	}
}

/// Types of indirection for a type identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeIndirection {
	Pointer,
	Reference,
	None,
}

/// A literal expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal {
	Int { value: u64 },
}

impl Ast for Literal {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_literal(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_literal(self)
	}
}

/// Operators for infix expressions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InfixOperator {
	// Arithmetic Operators
	Add, // '+'
	Sub, // '-'
	Mul, // '*'
	Div, // '/'
	Mod, // '%'

	// Logic Operators
	Eq,  // '=='
	Ne,  // '!='
	And, // '&&'
	Or,  // '||'
	Lt,  // '<'
	Le,  // '<='
	Gt,  // '>'
	Ge,  // '>='

	     // TODO: Bitwise Operators
}

/// Operators for prefix expressions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrefixOperator {
	Neg, // '-'
	Not, // '!'
}

/// An expression defines some in-memory computation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
	/// A basic expression consisting of just the use of a variable.
	Variable(Ident),

	/// A basic expression consisting of just the use of a literal.
	Literal(Literal),

	/// A function call expression.
	///
	/// ```text
	/// identifier "(" { expression "," } ")"
	/// ```
	Call {
		/// The name of the function to call.
		name: Ident,

		/// A list of arguments passed to the function.
		args: Vec<Expression>,
	},

	/// An expression applying a prefix operator to an expression.
	Prefix {
		op: PrefixOperator,
		expr: Box<Expression>,
	},

	/// An expression applying an infix operator to two expressions.
	Infix {
		op: InfixOperator,
		lhs: Box<Expression>,
		rhs: Box<Expression>,
	},

	/// An if-else expression.
	IfElse {
		condition: Box<Expression>,
		true_block: Box<CodeBlock>,
		false_block: Box<CodeBlock>,
	},

	/// A recursive expression enclosed in parentheses.
	///
	/// ```text
	/// "(" expression ")"
	/// ```
	Group(Box<Expression>),
}

impl Ast for Expression {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_expression(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_expression(self)
	}
}

/// An imperative statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
	/// Just an expression.
	Expr(Expression),

	/// A variable definition.
	///
	/// ```text
	/// ( "let" | "var" ) identifier [ ":" type-value ] "=" expression ";"
	/// ```
	Define {
		ident: Ident,
		is_mutable: bool,
		type_value: Option<TypeValue>,
		value: Expression,
	},

	/// A variable assignment.
	///
	/// ```text
	/// identifier "=" expression ";"
	/// ```
	Assign { ident: Ident, value: Expression },

	/// A return statement.
	Return(Expression),
}

impl Ast for Statement {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_statement(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_statement(self)
	}
}

/// A block of consecutive statements.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeBlock {
	pub statements: Vec<Statement>,
}

impl CodeBlock {
	pub fn new(statements: Vec<Statement>) -> Self {
		CodeBlock { statements }
	}

	/// An iterator over the statements in this code block.
	pub fn iter(&self) -> impl Iterator<Item = &Statement> {
		self.statements.iter()
	}
}

impl Ast for CodeBlock {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_code_block(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_code_block(self)
	}
}

/// A parameter for a function definition.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Parameter {
	pub ident: Ident,
	pub type_value: Option<TypeValue>,
}

impl Parameter {
	pub fn new<I: Into<Ident>>(ident: I, type_value: Option<TypeValue>) -> Self {
		Parameter {
			ident: ident.into(),
			type_value,
		}
	}
}

impl Ast for Parameter {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_parameter(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_parameter(self)
	}
}

/// The body of a function definition.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FunctionBody {
	Expr(Expression),
	Block(CodeBlock),
}

impl Ast for FunctionBody {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_function_body(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_function_body(self)
	}
}

/// A function definition.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDefinition {
	pub ident: Ident,
	pub parameters: Vec<Parameter>,
	pub return_type: Option<TypeValue>,
	pub body: FunctionBody,
}

impl FunctionDefinition {
	pub fn new<I: Into<Ident>>(
		ident: I,
		parameters: Vec<Parameter>,
		return_type: Option<TypeValue>,
		body: FunctionBody,
	) -> Self {
		FunctionDefinition {
			ident: ident.into(),
			parameters,
			return_type,
			body,
		}
	}

	/// True if this definition has an explicit return type.
	pub fn has_return_type(&self) -> bool {
		self.return_type.is_some()
	}
}

impl Ast for FunctionDefinition {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_function_definition(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_function_definition(self)
	}
}

/// Top-level AST node for a file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TranslationUnit {
	/// A list of functions defined in this unit.
	pub functions: Vec<FunctionDefinition>,
}

impl Ast for TranslationUnit {
	fn mut_visit<V: MutVisitor>(&mut self, v: &mut V) {
		v.visit_translation_unit(self);
	}

	fn transform<T: Transformer>(self, t: &mut T) -> Self {
		t.transform_translation_unit(self)
	}
}
