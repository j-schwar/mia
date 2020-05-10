//! Helper utilities for constructing IR code.

use crate::grammar::*;
use crate::types::*;

/// Utility class aiding in the construction of IR code.
pub struct Builder {
	root: Term,
}

impl Builder {
	/// Constructs a new, empty term builder.
	pub fn new() -> Self {
		Builder {
			root: Term::Undefined,
		}
	}

	/// Builder style version of [`build_bind`](#method.build_bind).
	pub fn bind<V, T, E>(mut self, variable: V, type_annotation: T, expression: E) -> Self
	where
		V: Into<Variable>,
		T: Into<Type>,
		E: Into<Expression>,
	{
		self.build_bind(variable, type_annotation, expression);
		return self;
	}

	/// Convenience builder style method for constructing a bind term with a call
	/// expression.
	pub fn call<V, T, A>(
		self,
		variable: V,
		type_annotation: T,
		name: &str,
		arguments: Vec<A>,
	) -> Self
	where
		V: Into<Variable>,
		T: Into<Type>,
		A: Into<Variable>,
	{
		let expr = Expression::new_call(name, arguments);
		self.bind(variable, type_annotation, expr)
	}

	/// Convenience builder style method for construct a bind term with a
	/// unary operation expression.
	pub fn unary_op<V, T, O>(
		self,
		variable: V,
		type_annotation: T,
		operator: Operator,
		operand: O,
	) -> Self
	where
		V: Into<Variable>,
		T: Into<Type>,
		O: Into<Variable>,
	{
		let expr = Expression::new_unary(operator, operand);
		self.bind(variable, type_annotation, expr)
	}

	/// Convenience builder style method for constructing a bind term with a
	/// binary operation expression.
	pub fn binary_op<V, T, Lhs, Rhs>(
		self,
		variable: V,
		type_annotation: T,
		operator: Operator,
		lhs: Lhs,
		rhs: Rhs,
	) -> Self
	where
		V: Into<Variable>,
		T: Into<Type>,
		Lhs: Into<Variable>,
		Rhs: Into<Variable>,
	{
		let expr = Expression::new_binary(operator, lhs, rhs);
		self.bind(variable, type_annotation, expr)
	}

	/// Builder style version of [`build_match`](#method.build_match).
	pub fn branch<V>(mut self, target: V, branches: Vec<(Pattern, Term)>) -> Self
	where
		V: Into<Variable>,
	{
		self.build_match(target, branches);
		return self;
	}

	/// Builder style version of [`build_if_else`](#method.build_if_else).
	pub fn if_else<V>(mut self, condition: V, true_term: Term, false_term: Term) -> Self
	where
		V: Into<Variable>,
	{
		self.build_if_else(condition, true_term, false_term);
		return self;
	}

	/// Builder style version of [`build_ret`](#method.build_ret).
	pub fn ret<V>(mut self, variable: V) -> Self
	where
		V: Into<Variable>,
	{
		self.build_ret(variable);
		return self;
	}

	/// Finalizes the building process returning the finished term.
	///
	/// # Panics
	///
	/// Panics if there is an `Undefined` term somewhere in the term sequence.
	pub fn build(self) -> Term {
		// Validate to ensure that the result does not contain any undefined terms.
		for term in self.root.iter() {
			if term.is_undefined() {
				panic!("attempt to build term which contains undefined variant")
			}
		}

		return self.root;
	}

	/// Builds a bind term appending it to the end of the term sequence currently
	/// being built.
	///
	/// # Parameters
	///
	/// * `variable`: The name of the variable being bound.
	///
	/// * `type_annotation`: The declared type of `variable`.
	///
	/// * `expression`: The expression to bind to `variable`.
	///
	/// # Panics
	///
	/// Panics if the last term in the sequence being built is not and
	/// `Undefined` variant. This means that the sequence has already been
	/// terminated by a `Return` or `Match` term and it is illegal to append
	/// more terms onto it.
	pub fn build_bind<V, T, E>(&mut self, variable: V, type_annotation: T, expression: E)
	where
		V: Into<Variable>,
		T: Into<Type>,
		E: Into<Expression>,
	{
		let term = Term::new_bind(variable, type_annotation, expression, Term::Undefined);
		self.append_term(term);
	}

	/// Builds a return term appending it to the end of the term sequence
	/// currently being built.
	///
	/// Note that return is a terminating term meaning that once one has been
	/// appended to the sequence no more terms may be appended.
	///
	/// # Parameters
	///
	/// * `variable`: The name of the variable to return.
	///
	/// # Panics
	///
	/// Panics if the last term in the sequence being built is not and
	/// `Undefined` variant. This means that the sequence has already been
	/// terminated by a `Return` or `Match` term and it is illegal to append
	/// more terms onto it.
	pub fn build_ret<V>(&mut self, variable: V)
	where
		V: Into<Variable>,
	{
		let term = Term::new_return(variable);
		self.append_term(term);
	}

	/// Builds a match term appending it to the end of the term sequence
	/// currently being built.
	///
	/// Note that match is a terminating term meaning that once one has been
	/// appended to the sequence no more terms may be appended.
	///
	/// # Parameters
	///
	/// * `target`: The variable to match patterns against.
	///
	/// * `branches`: Pairs of patterns and terms which define the various
	/// branches the match may take.
	///
	/// # Panics
	///
	/// Panics if the last term in the sequence being built is not and
	/// `Undefined` variant. This means that the sequence has already been
	/// terminated by a `Return` or `Match` term and it is illegal to append
	/// more terms onto it.
	pub fn build_match<V>(&mut self, target: V, branches: Vec<(Pattern, Term)>)
	where
		V: Into<Variable>,
	{
		let term = Term::new_match(target, branches);
		self.append_term(term);
	}

	/// Builds an if-else control flow construct appending it to the term
	/// sequence.
	///
	/// The IR language has no concept of if-else control flow. It is instead
	/// emulated using a `Match` term.
	///
	/// Note that this is a terminating term meaning that once it has been
	/// appended to the sequence no more terms ma be appended.
	///
	/// # Parameters
	///
	/// * `condition`: The boolean condition for the if-else term.
	///
	/// * `true_term`: The code that is executed if `condition` is true.
	///
	/// * `false_term`: The code that is executed if `condition` is false.
	///
	/// # Panics
	///
	/// Panics if the last term in the sequence being built is not and
	/// `Undefined` variant. This means that the sequence has already been
	/// terminated by a `Return` or `Match` term and it is illegal to append
	/// more terms onto it.
	pub fn build_if_else<V>(&mut self, condition: V, true_term: Term, false_term: Term)
	where
		V: Into<Variable>,
	{
		let term = Term::new_match(
			condition,
			vec![
				(Literal::Bool(true).into(), true_term),
				(Literal::Bool(false).into(), false_term),
			],
		);
		self.append_term(term);
	}

	/// Replaces the last term (which must be `Term::Undefined`) with `term`.
	///
	/// # Panics
	///
	/// Panics if the last term in the linked term list is not an `Undefined`
	/// term variant.
	pub fn append_term(&mut self, term: Term) {
		let tail = self.tail();
		if !tail.is_undefined() {
			panic!("attempt to append to terminated term sequence");
		}
		*tail = term;
	}

	/// Traverses the linked list of terms rooted at `self.root` returning a
	/// reference to the last term in the sequence.
	fn tail(&mut self) -> &mut Term {
		let mut term = &mut self.root;
		loop {
			match term {
				Term::Bind { continuation, .. } => term = continuation,
				_ => {
					break;
				}
			}
		}
		return term;
	}
}
