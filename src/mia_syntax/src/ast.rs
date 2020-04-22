//! This module defines the Abstract Syntax Tree for the Mia language.

/// An identifier used to uniquely name variables, functions, and types.
///
/// Valid identifiers must match the following regular expression:
///
/// ```text
/// [_a-zA-Z][_a-zA-Z0-9]*
/// ```
#[derive(Debug, Eq, PartialEq)]
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

/// A type identifier used when declaring the type of variables, parameters,
/// etc.
///
/// Type identifiers follow the following syntactic format:
///
/// ```text
/// type-value ::= [ "&" | "*" ] [ "mut" ] identifier ;
/// ```
#[derive(Debug, Eq, PartialEq)]
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

/// Types of indirection for a type identifier.
#[derive(Debug, Eq, PartialEq)]
pub enum TypeIndirection {
	Pointer,
	Reference,
	None,
}

/// A literal expression.
#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
	Int { value: u64 },
}

/// Operators for infix expressions.
#[derive(Debug, Eq, PartialEq)]
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
#[derive(Debug, Eq, PartialEq)]
pub enum PrefixOperator {
	Neg, // '-'
	Not, // '!'
}

/// An expression defines some in-memory computation.
#[derive(Debug, Eq, PartialEq)]
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

/// An imperative statement.
#[derive(Debug, Eq, PartialEq)]
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
}

/// A block of consecutive statements.
#[derive(Debug, Eq, PartialEq)]
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

/// A parameter for a function definition.
#[derive(Debug, Eq, PartialEq)]
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

/// The body of a function definition.
#[derive(Debug, Eq, PartialEq)]
pub enum FunctionBody {
	Expr(Expression),
	Block(CodeBlock),
}

/// A function definition.
#[derive(Debug, Eq, PartialEq)]
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

/// Top-level AST node for a file.
#[derive(Debug, Eq, PartialEq)]
pub struct TranslationUnit {
	/// A list of functions defined in this unit.
	pub functions: Vec<FunctionDefinition>,
}
