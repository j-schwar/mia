//! A collection of AST transformation passes.

use crate::ast::visit::{self, Transformer};
use crate::ast::{Expression, TranslationUnit};

/// A simple AST transformer which removes `Expression::Group` nodes from
/// the tree.
pub struct RemoveGroups;

impl Transformer for RemoveGroups {
	fn transform_expression(&mut self, expr: Expression) -> Expression {
		use Expression::*;

		match expr {
			Group(e) => self.transform_expression(*e),
			_ => visit::noop_transform_expression(self, expr),
		}
	}
}

/// Removes `Expression::Group` nodes from a translation unit.
///
/// While groups are required to properly determine operator precedence, once
/// that has been complete they are not needed anymore. This transformation
/// pass removes such nodes, simplifying the tree.
pub fn remove_groups(tu: TranslationUnit) -> TranslationUnit {
	let mut pass = RemoveGroups;
	pass.transform_translation_unit(tu)
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::ast::{Ident, InfixOperator, PrefixOperator};
	use Expression::*;

	#[test]
	fn test_remove_groups() {
		let expr = Infix {
			op: InfixOperator::Add,
			lhs: Box::new(Variable(Ident::from("a"))),
			rhs: Box::new(Group(Box::new(Prefix {
				op: PrefixOperator::Neg,
				expr: Box::new(Variable(Ident::from("b"))),
			}))),
		};

		let expected = Infix {
			op: InfixOperator::Add,
			lhs: Box::new(Variable(Ident::from("a"))),
			rhs: Box::new(Prefix {
				op: PrefixOperator::Neg,
				expr: Box::new(Variable(Ident::from("b"))),
			}),
		};

		let mut pass = RemoveGroups;
		let actual = pass.transform_expression(expr);

		assert_eq!(actual, expected);
	}
}
