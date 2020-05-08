//! Iterator types for the IR grammar.

use super::*;

/// An iterator over the terms in a function definition.
pub struct Terms<'a> {
	next: Option<&'a Term>,
	stack: Vec<(&'a Term, usize)>,
}

impl<'a> Terms<'a> {
	/// Constructs a new iterator over `definition`.
	pub fn new(definition: &'a Definition) -> Self {
		Terms {
			next: Some(&definition.term),
			stack: Vec::new(),
		}
	}

	/// Returns the start of the next branch from the stack, popping items when
	/// needed.
	fn pop_next_branch(&mut self) -> Option<&'a Term> {
		use Term::Match;

		if self.stack.is_empty() {
			return None;
		}

		let last_index = self.stack.len() - 1;

		match self.stack[last_index] {
			(Match { branches, .. }, index) => {
				let next_index = index + 1;
				if let Some((_, next_term)) = branches.get(next_index) {
					// There is still another branch in this match term so return the
					// first term in that branch.
					self.stack[last_index].1 += 1;
					Some(next_term)
				} else {
					// There are no more branches in this match term. Pop it off the
					// stack and try again.
					self.stack.pop();
					self.pop_next_branch()
				}
			}

			_ => unreachable!("non-match found on stack"),
		}
	}

	/// Sets the next term based on `current`.
	fn set_next(&mut self, current: &'a Term) {
		use Term::*;

		match current {
			// Bind terms mark the continuation of a sequence.
			Bind { continuation, .. } => self.next = Some(&*continuation),

			// Match terms may contain many branches which will be iterated over in
			// a depth-first manor.
			Match { branches, .. } => {
				// A match with no branches marks the end of a sequence.
				if branches.is_empty() {
					self.next = self.pop_next_branch();
					return;
				}

				// The match has at least one branch. Set the next term to be the start
				// of the first branch then pop the match term onto the stack so that
				// we can traverse the others once the first branch is finished.
				self.next = Some(&branches[0].1);
				self.stack.push((current, 0))
			}

			// Return or Undefined terms mark the end of a sequence.
			Return(_) | Undefined => {
				self.next = self.pop_next_branch();
			}
		}
	}
}

impl<'a> Iterator for Terms<'a> {
	type Item = &'a Term;

	fn next(&mut self) -> Option<Self::Item> {
		let current = self.next?;
		self.set_next(current);
		return Some(current);
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_linear_term_iterator() {
		// fun foo(a: int, b: int) -> int =
		//   let c: int = bar(a, b) in
		//   let d: int = -c in
		//   ret d

		let t3 = Term::new_return("d");
		let t2 = Term::new_bind(
			"d",
			Native::Int,
			Expression::new_unary(Operator::Neg, "c"),
			t3.clone(),
		);
		let t1 = Term::new_bind(
			"c",
			Native::Int,
			Expression::new_call("bar", vec!["a", "b"]),
			t2.clone(),
		);

		let def = Definition::new(
			"foo",
			vec![("a", Type::int_type()), ("b", Type::int_type())],
			Native::Int,
			t1.clone(),
		);

		let mut iter = def.terms();
		assert_eq!(iter.next(), Some(&t1));
		assert_eq!(iter.next(), Some(&t2));
		assert_eq!(iter.next(), Some(&t3));
		assert_eq!(iter.next(), None);
	}

	#[test]
	fn test_branching_term_iterator() {
		// fun max(a: int, b: int) -> int =
		//   let c: bool = a >= b in
		//   match c {
		//     true =>
		//       ret a;
		//     false =>
		//       ret b;
		//   }

		let t4 = Term::new_return("b");
		let t3 = Term::new_return("a");
		let t2 = Term::new_match(
			"c",
			vec![
				(Pattern::Literal(Literal::Bool(true)), t3.clone()),
				(Pattern::Literal(Literal::Bool(false)), t4.clone()),
			],
		);
		let t1 = Term::new_bind(
			"c",
			Native::Bool,
			Expression::new_binary(Operator::Ge, "a", "b"),
			t2.clone(),
		);

		let def = Definition::new(
			"max",
			vec![("a", Type::int_type()), ("b", Type::int_type())],
			Native::Int,
			t1.clone(),
		);

		let mut iter = def.terms();
		assert_eq!(iter.next(), Some(&t1));
		assert_eq!(iter.next(), Some(&t2));
		assert_eq!(iter.next(), Some(&t3));
		assert_eq!(iter.next(), Some(&t4));
		assert_eq!(iter.next(), None);
	}

	#[test]
	fn test_nested_branching_term_iterator() {
		// fun comp(a: int, b: int) -> int =
		//   let c: bool = a >= b in
		//   match c {
		//     true =>
		//       let d: bool = a == b in
		//       match d {
		//         true =>
		//           let e: int = 0 in
		//           ret e;
		//         false =>
		//           let f: int = -1 in
		//           ret f;
		//       };
		//     false =>
		//       let f: int = -1 in
		//       ret f;
		//   }

		let t10 = Term::new_return("g");
		let t9 = Term::new_bind("g", Native::Int, Literal::Int(-1), t10.clone());
		let t8 = Term::new_return("f");
		let t7 = Term::new_bind("f", Native::Int, Literal::Int(1), t8.clone());
		let t6 = Term::new_return("e");
		let t5 = Term::new_bind("e", Native::Int, Literal::Int(0), t6.clone());
		let t4 = Term::new_match(
			"d",
			vec![
				(Literal::Bool(true).into(), t5.clone()),
				(Literal::Bool(false).into(), t7.clone()),
			],
		);
		let t3 = Term::new_bind(
			"d",
			Native::Bool,
			Expression::new_binary(Operator::Eq, "a", "b"),
			t4.clone(),
		);
		let t2 = Term::new_match(
			"c",
			vec![
				(Literal::Bool(true).into(), t3.clone()),
				(Literal::Bool(false).into(), t9.clone()),
			],
		);
		let t1 = Term::new_bind(
			"c",
			Native::Int,
			Expression::new_binary(Operator::Ge, "a", "b"),
			t2.clone(),
		);

		let def = Definition::new(
			"comp",
			vec![("a", Native::Int.into()), ("b", Native::Int.into())],
			Native::Int,
			t1.clone(),
		);

		let mut iter = def.terms();
		assert_eq!(iter.next(), Some(&t1));
		assert_eq!(iter.next(), Some(&t2));
		assert_eq!(iter.next(), Some(&t3));
		assert_eq!(iter.next(), Some(&t4));
		assert_eq!(iter.next(), Some(&t5));
		assert_eq!(iter.next(), Some(&t6));
		assert_eq!(iter.next(), Some(&t7));
		assert_eq!(iter.next(), Some(&t8));
		assert_eq!(iter.next(), Some(&t9));
		assert_eq!(iter.next(), Some(&t10));
		assert_eq!(iter.next(), None);
	}
}
