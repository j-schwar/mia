//! The parser for the Mia language.
//!
//! Takes some source text and constructs an AST from it.

use crate::ast::*;
use nom::bytes::complete::{take_while, take_while1};
use nom::combinator::all_consuming;
use nom::multi::many0;
pub use nom::IResult;

/// Parses a whole translation unit.
pub fn parse(i: &str) -> IResult<&str, TranslationUnit> {
	let (i, functions) = many0(ws_delimited(func::parse))(i)?;
	Ok((i, TranslationUnit { functions }))
}

/// Parses a whole translation unit.
///
/// Fails to parse if there are any left over characters which cannot be parsed.
pub fn parse_complete(i: &str) -> IResult<&str, TranslationUnit> {
	let (i, tu) = all_consuming(parse)(i)?;
	Ok((i, tu))
}

/// Consumes 0 or more whitespaces characters.
fn eat_ws(i: &str) -> IResult<&str, ()> {
	let (i, _) = take_while(move |c: char| c.is_whitespace())(i)?;
	Ok((i, ()))
}

/// Consumes 1 or more whitespace characters.
fn eat_ws1(i: &str) -> IResult<&str, ()> {
	let (i, _) = take_while1(move |c: char| c.is_whitespace())(i)?;
	Ok((i, ()))
}

/// Parses 0 or more whitespace characters, then `sep`, then 0 or more
/// whitespace characters again.
fn ws_delimited<'a, T, F>(sep: F) -> impl Fn(&'a str) -> IResult<&'a str, T>
where
	F: Fn(&'a str) -> IResult<&'a str, T>,
{
	nom::sequence::delimited(eat_ws, sep, eat_ws)
}

/// Identifier parsing functions.
mod ident {
	use super::*;
	use nom::bytes::complete::{take_while, take_while1};

	/// True if `c` is a valid starting character for an identifier.
	fn is_ident_start(c: char) -> bool {
		c.is_ascii_alphabetic() || c == '_'
	}

	/// True if `c` is a valid continuation character for an identifier.
	fn is_ident_continue(c: char) -> bool {
		c.is_ascii_alphanumeric() || c == '_'
	}

	/// Parses an identifier.
	pub fn parse(i: &str) -> IResult<&str, Ident> {
		let (i, start) = take_while1(is_ident_start)(i)?;
		let (i, cont) = take_while(is_ident_continue)(i)?;
		let mut name = String::from(start);
		name.push_str(cont);
		Ok((i, Ident::new(name)))
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_ident() {
			let (i, ident) = parse("_1_foo").unwrap();
			assert_eq!(i, "");
			assert_eq!(ident, Ident::from("_1_foo"));
		}

		#[test]
		#[should_panic]
		fn test_invalid_ident() {
			let _ = parse("1foo").unwrap();
		}
	}
}

/// Type value parsing functions.
mod type_value {
	use super::*;
	use nom::branch::alt;
	use nom::bytes::complete::tag;
	use nom::combinator::opt;
	use nom::sequence::separated_pair;

	/// Parses a '&' symbol.
	fn reference_marker(i: &str) -> IResult<&str, TypeIndirection> {
		let (i, _) = tag("&")(i)?;
		Ok((i, TypeIndirection::Reference))
	}

	/// Parses a '*' symbol.
	fn pointer_marker(i: &str) -> IResult<&str, TypeIndirection> {
		let (i, _) = tag("*")(i)?;
		Ok((i, TypeIndirection::Pointer))
	}

	/// Parses an optional 'mut' symbol.
	///
	/// Returns `true` if the symbols was parsed, and `false` if it wasn't.
	fn mutable_marker(i: &str) -> IResult<&str, bool> {
		let (i, sym) = opt(tag("mut"))(i)?;
		Ok((i, sym.is_some()))
	}

	/// Parses a type value.
	pub fn parse(i: &str) -> IResult<&str, TypeValue> {
		let indirection = opt(alt((reference_marker, pointer_marker)));
		let prefix = separated_pair(indirection, eat_ws, mutable_marker);
		let (i, ((indirection, is_mut), ident)) = separated_pair(prefix, eat_ws, ident::parse)(i)?;
		let indirection = indirection.unwrap_or(TypeIndirection::None);
		Ok((i, TypeValue::new(ident, indirection, is_mut)))
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_basic_type() {
			let (i, t) = parse("i32").unwrap();
			assert_eq!(i, "");
			assert_eq!(t, TypeValue::new("i32", TypeIndirection::None, false));
		}

		#[test]
		fn test_reference_type() {
			let (i, t) = parse("&i32").unwrap();
			assert_eq!(i, "");
			assert_eq!(t, TypeValue::new("i32", TypeIndirection::Reference, false));
		}

		#[test]
		fn test_mutable_reference_type() {
			let (i, t) = parse("&mut i32").unwrap();
			assert_eq!(i, "");
			assert_eq!(t, TypeValue::new("i32", TypeIndirection::Reference, true));
		}

		#[test]
		fn test_pointer_type() {
			let (i, t) = parse("*i32").unwrap();
			assert_eq!(i, "");
			assert_eq!(t, TypeValue::new("i32", TypeIndirection::Pointer, false));
		}

		#[test]
		fn test_mutable_pointer_type() {
			let (i, t) = parse("*mut i32").unwrap();
			assert_eq!(i, "");
			assert_eq!(t, TypeValue::new("i32", TypeIndirection::Pointer, true));
		}
	}
}

/// Literal parsing functions.
mod literal {
	use super::*;
	use nom::bytes::complete::take_while1;

	/// Parses an integer literal.
	fn parse_int(i: &str) -> IResult<&str, Literal> {
		// TODO: Support other bases like binary and hex.
		let (i, digits) = take_while1(move |c: char| c.is_ascii_digit())(i)?;
		let value = digits.parse::<u64>().unwrap();
		Ok((i, Literal::Int { value }))
	}

	/// Parses a literal.
	pub fn parse(i: &str) -> IResult<&str, Literal> {
		// TODO: Support other literal types like floats and strings.
		parse_int(i)
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_int_literal() {
			let (i, x) = parse("1234").unwrap();
			assert_eq!(i, "");
			assert_eq!(x, Literal::Int { value: 1234 });
		}
	}
}

/// Expression parsing functions.
mod expr {
	use super::*;
	use nom::branch::alt;
	use nom::bytes::complete::tag;
	use nom::character::complete::one_of;
	use nom::combinator::{cut, map};
	use nom::multi::separated_list;
	use nom::sequence::{delimited, terminated, tuple};

	/// Parses a function call.
	fn function_call(i: &str) -> IResult<&str, Expression> {
		let (i, ident) = ident::parse(i)?;
		let (i, _) = eat_ws(i)?;
		let (i, _) = tag("(")(i)?;

		// From here on, we know we are parsing a function call so, if there is an
		// error, transform it into a failure and don't backtrack.
		let params = separated_list(delimited(eat_ws, tag(","), eat_ws), expr::parse);
		let (i, params) = cut(terminated(params, tag(")")))(i)?;

		Ok((
			i,
			Expression::Call {
				name: ident,
				args: params,
			},
		))
	}

	/// Parses a prefix operator.
	fn prefix_operator(i: &str) -> IResult<&str, PrefixOperator> {
		use PrefixOperator::*;
		let (i, op) = one_of("!-")(i)?;
		let op = match op {
			'!' => Not,
			'-' => Neg,
			_ => unreachable!(),
		};
		Ok((i, op))
	}

	/// Parses a prefix expression.
	fn prefix_expression(i: &str) -> IResult<&str, Expression> {
		let (i, (op, e)) = tuple((prefix_operator, expr::parse))(i)?;
		Ok((
			i,
			Expression::Prefix {
				op,
				expr: Box::new(e),
			},
		))
	}

	/// Parses an infix operator.
	fn infix_operator(i: &str) -> IResult<&str, InfixOperator> {
		use InfixOperator::*;
		let arithmetic = alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("%")));
		let logic = alt((
			tag("=="),
			tag("!="),
			tag("&&"),
			tag("||"),
			tag("<="),
			tag(">="),
			tag("<"),
			tag(">"),
		));
		let (i, op) = alt((arithmetic, logic))(i)?;
		let op = match op {
			"+" => Add,
			"-" => Sub,
			"*" => Mul,
			"/" => Div,
			"%" => Mod,
			"==" => Eq,
			"!=" => Ne,
			"&&" => And,
			"||" => Or,
			"<" => Lt,
			"<=" => Le,
			">" => Gt,
			">=" => Ge,
			_ => unreachable!(),
		};
		Ok((i, op))
	}

	/// Parses an infix expression.
	fn infix_expression(i: &str) -> IResult<&str, Expression> {
		let (i, lhs) = non_infix_expr(i)?;
		let (i, _) = eat_ws(i)?;
		let (i, op) = infix_operator(i)?;
		let (i, _) = eat_ws(i)?;
		let (i, rhs) = cut(expr::parse)(i)?;
		Ok((
			i,
			Expression::Infix {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			},
		))
	}

	/// Parses an if-else expression.
	fn if_else_expression(i: &str) -> IResult<&str, Expression> {
		let (i, _) = tag("if")(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, condition) = cut(expr::parse)(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, true_block) = cut(block::parse)(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, _) = cut(tag("else"))(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, false_block) = cut(block::parse)(i)?;
		Ok((
			i,
			Expression::IfElse {
				condition: Box::new(condition),
				true_block: Box::new(true_block),
				false_block: Box::new(false_block),
			},
		))
	}

	/// Parses a group expression.
	fn group(i: &str) -> IResult<&str, Expression> {
		let (i, _) = tag("(")(i)?;
		let (i, _) = eat_ws(i)?;
		let (i, (e, _, _)) = cut(tuple((expr::parse, eat_ws, tag(")"))))(i)?;
		Ok((i, Expression::Group(Box::new(e))))
	}

	/// Parses any type of expression except infix expressions.
	///
	/// Used as the parser for the lhs of an infix expression to avoid infinite
	/// left recursion.
	fn non_infix_expr(i: &str) -> IResult<&str, Expression> {
		let parse_literal = map(literal::parse, |l| Expression::Literal(l));
		let parse_ident = map(ident::parse, |i| Expression::Variable(i));
		alt((
			group,
			if_else_expression,
			prefix_expression,
			function_call,
			parse_literal,
			parse_ident,
		))(i)
	}

	/// Parses an expression.
	///
	/// Note that the precedence and associativity of infix operators has not
	/// yet been resolved in the resultant expression. The corresponding AST
	/// transformation pass must be applied to the resultant AST in order to
	/// resolve operator precedence/associativity.
	pub fn parse(i: &str) -> IResult<&str, Expression> {
		alt((infix_expression, non_infix_expr))(i)
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_function_call() {
			let (_, e) = parse("foo(x, y + 1)").unwrap();
			assert_eq!(
				e,
				Expression::Call {
					name: Ident::from("foo"),
					args: vec![
						Expression::Variable(Ident::from("x")),
						Expression::Infix {
							op: InfixOperator::Add,
							lhs: Expression::Variable(Ident::from("y")).into(),
							rhs: Expression::Literal(Literal::Int { value: 1 }).into(),
						}
					],
				}
			);
		}

		#[test]
		fn test_prefix() {
			let (_, e) = parse("!x").unwrap();
			assert_eq!(
				e,
				Expression::Prefix {
					op: PrefixOperator::Not,
					expr: Expression::Variable(Ident::from("x")).into(),
				}
			);
		}

		#[test]
		fn test_infix() {
			let (_, e) = parse("x + 1").unwrap();
			assert_eq!(
				e,
				Expression::Infix {
					op: InfixOperator::Add,
					lhs: Expression::Variable(Ident::from("x")).into(),
					rhs: Expression::Literal(Literal::Int { value: 1 }).into(),
				}
			);
		}
	}

	#[test]
	fn test_extended_infix() {
		let (_, e) = parse("a + b - c").unwrap();
		assert_eq!(
			e,
			Expression::Infix {
				op: InfixOperator::Add,
				lhs: Expression::Variable(Ident::from("a")).into(),
				rhs: Expression::Infix {
					op: InfixOperator::Sub,
					lhs: Expression::Variable(Ident::from("b")).into(),
					rhs: Expression::Variable(Ident::from("c")).into(),
				}
				.into(),
			}
		);
	}

	#[test]
	fn test_group_in_infix() {
		let (_, e) = parse("(a + b) - c").unwrap();
		assert_eq!(
			e,
			Expression::Infix {
				op: InfixOperator::Sub,
				lhs: Expression::Group(
					Expression::Infix {
						op: InfixOperator::Add,
						lhs: Expression::Variable(Ident::from("a")).into(),
						rhs: Expression::Variable(Ident::from("b")).into(),
					}
					.into(),
				)
				.into(),
				rhs: Expression::Variable(Ident::from("c")).into(),
			}
		);
	}
}

/// Parse functions for statements.
mod stmt {
	use super::*;
	use nom::branch::alt;
	use nom::bytes::complete::tag;
	use nom::character::complete::char;
	use nom::combinator::{cut, opt};
	use nom::sequence::preceded;

	/// Parses a declaration keyword returning `true` if the declaration is
	/// mutable (i.e., uses "var") or `false` if it is immutable (i.e., uses
	/// "let").
	fn decl_keyword(i: &str) -> IResult<&str, bool> {
		let (i, word) = alt((tag("let"), tag("var")))(i)?;
		let mutable = match word {
			"let" => false,
			"var" => true,
			_ => unreachable!(),
		};
		Ok((i, mutable))
	}

	/// Parses a variable declaration.
	fn declaration(i: &str) -> IResult<&str, Statement> {
		// ( "let" | "var" ) identifier
		let (i, is_mutable) = decl_keyword(i)?;
		let (i, _) = cut(eat_ws1)(i)?;
		let (i, name) = cut(ident::parse)(i)?;
		let (i, _) = cut(eat_ws)(i)?;

		// [ ":" type-value ]
		let (i, tv) = opt(preceded(preceded(char(':'), eat_ws), type_value::parse))(i)?;

		// "="
		let (i, _) = cut(eat_ws)(i)?;
		let (i, _) = cut(char('='))(i)?;
		let (i, _) = cut(eat_ws)(i)?;

		// expression ";"
		let (i, e) = cut(expr::parse)(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, _) = cut(char(';'))(i)?;

		Ok((
			i,
			Statement::Define {
				ident: name,
				is_mutable,
				type_value: tv,
				value: e,
			},
		))
	}

	/// Parses a variable assignment.
	fn assignment(i: &str) -> IResult<&str, Statement> {
		// identifier
		let (i, name) = ident::parse(i)?;
		let (i, _) = eat_ws(i)?;

		// "="
		let (i, _) = char('=')(i)?;
		let (i, _) = cut(eat_ws)(i)?;

		// expression ";"
		let (i, e) = cut(expr::parse)(i)?;
		let (i, _) = cut(eat_ws)(i)?;
		let (i, _) = cut(char(';'))(i)?;

		Ok((
			i,
			Statement::Assign {
				ident: name,
				value: e,
			},
		))
	}

	/// Parses an expression statement.
	fn expression(i: &str) -> IResult<&str, Statement> {
		let (i, e) = expr::parse(i)?;
		let (i, _) = eat_ws(i)?;
		let (i, _) = char(';')(i)?;
		Ok((i, Statement::Expr(e)))
	}

	/// Parses a statement.
	pub fn parse(i: &str) -> IResult<&str, Statement> {
		alt((declaration, assignment, expression))(i)
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_expression() {
			let (_, s) = parse("foo();").unwrap();
			assert_eq!(
				s,
				Statement::Expr(Expression::Call {
					name: Ident::from("foo"),
					args: vec![]
				})
			);
		}

		#[test]
		fn test_assignment() {
			let (_, s) = parse("a = 0;").unwrap();
			assert_eq!(
				s,
				Statement::Assign {
					ident: Ident::from("a"),
					value: Expression::Literal(Literal::Int { value: 0 })
				}
			);
		}

		#[test]
		fn test_declaration_with_implicit_type() {
			let (_, s) = parse("let a = 0;").unwrap();
			assert_eq!(
				s,
				Statement::Define {
					ident: Ident::from("a"),
					is_mutable: false,
					type_value: None,
					value: Expression::Literal(Literal::Int { value: 0 })
				}
			);
		}

		#[test]
		fn test_declaration_with_explicit_type() {
			let (_, s) = parse("let a: i32 = 0;").unwrap();
			assert_eq!(
				s,
				Statement::Define {
					ident: Ident::from("a"),
					is_mutable: false,
					type_value: Some(TypeValue::new("i32", TypeIndirection::None, false)),
					value: Expression::Literal(Literal::Int { value: 0 })
				}
			);
		}
	}
}

/// Parse functions for code blocks.
mod block {
	use super::*;
	use nom::character::complete::char;
	use nom::combinator::cut;
	use nom::multi::separated_list;
	use nom::sequence::{delimited, tuple};

	/// Parses a code block.
	pub fn parse(i: &str) -> IResult<&str, CodeBlock> {
		let opener = tuple((char('{'), eat_ws));
		let closer = cut(tuple((eat_ws, char('}'))));
		let inner = cut(separated_list(eat_ws, stmt::parse));
		let (i, statements) = delimited(opener, inner, closer)(i)?;
		Ok((i, CodeBlock { statements }))
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_code_block() {
			let (_, block) = parse("{\n  let a = 0;\n  a = 1;\n  foo(a);\n}").unwrap();

			let s1 = Statement::Define {
				ident: Ident::from("a"),
				is_mutable: false,
				type_value: None,
				value: Expression::Literal(Literal::Int { value: 0 }),
			};

			let s2 = Statement::Assign {
				ident: Ident::from("a"),
				value: Expression::Literal(Literal::Int { value: 1 }),
			};

			let s3 = Statement::Expr(Expression::Call {
				name: Ident::from("foo"),
				args: vec![Expression::Variable(Ident::from("a"))],
			});

			assert_eq!(
				block,
				CodeBlock {
					statements: vec![s1, s2, s3]
				}
			);
		}
	}
}

/// Parse functions for function definitions.
mod func {
	use super::*;
	use nom::branch::alt;
	use nom::bytes::complete::tag;
	use nom::character::complete::char;
	use nom::combinator::{cut, map, opt};
	use nom::multi::separated_list;
	use nom::sequence::{delimited, preceded, terminated, tuple};

	/// Parses a function parameter.
	fn parameter(i: &str) -> IResult<&str, Parameter> {
		let name = terminated(ident::parse, eat_ws);
		let tv = preceded(preceded(char(':'), eat_ws), type_value::parse);
		let (i, (name, tv)) = tuple((name, opt(tv)))(i)?;
		Ok((
			i,
			Parameter {
				ident: name,
				type_value: tv,
			},
		))
	}

	fn body(i: &str) -> IResult<&str, FunctionBody> {
		let expression = map(
			delimited(
				ws_delimited(char('=')),
				cut(expr::parse),
				cut(ws_delimited(char(';'))),
			),
			|e| FunctionBody::Expr(e),
		);
		let block = map(preceded(eat_ws, block::parse), |b| FunctionBody::Block(b));
		alt((expression, block))(i)
	}

	/// Parses a function definition.
	pub fn parse(i: &str) -> IResult<&str, FunctionDefinition> {
		let keyword = terminated(tag("fun"), eat_ws1);
		let name = cut(terminated(ident::parse, eat_ws));
		let opener = tuple((char('('), eat_ws));
		let closer = tuple((eat_ws, char(')')));
		let param_list = cut(delimited(
			opener,
			separated_list(ws_delimited(char(',')), parameter),
			closer,
		));
		let return_type = cut(opt(preceded(ws_delimited(tag("->")), type_value::parse)));

		let (i, (_, name, parameters, return_type, body)) =
			tuple((keyword, name, param_list, return_type, body))(i)?;
		Ok((
			i,
			FunctionDefinition {
				ident: name,
				parameters,
				return_type,
				body,
			},
		))
	}

	#[cfg(test)]
	mod test {
		use super::*;

		#[test]
		fn test_function_with_block_body() {
			let (_, f) = parse(
				"fun my_function(b: &MyStruct) -> i32 {
				let a = 0;
				a = 1;
				foo(a, b);
			}",
			)
			.unwrap();

			let s1 = Statement::Define {
				ident: Ident::from("a"),
				is_mutable: false,
				type_value: None,
				value: Expression::Literal(Literal::Int { value: 0 }),
			};

			let s2 = Statement::Assign {
				ident: Ident::from("a"),
				value: Expression::Literal(Literal::Int { value: 1 }),
			};

			let s3 = Statement::Expr(Expression::Call {
				name: Ident::from("foo"),
				args: vec![
					Expression::Variable(Ident::from("a")),
					Expression::Variable(Ident::from("b")),
				],
			});

			let body = CodeBlock {
				statements: vec![s1, s2, s3],
			};

			let parameters = vec![Parameter::new(
				"b",
				Some(TypeValue::new(
					"MyStruct",
					TypeIndirection::Reference,
					false,
				)),
			)];

			let return_type = TypeValue::new("i32", TypeIndirection::None, false);

			assert_eq!(
				f,
				FunctionDefinition::new(
					"my_function",
					parameters,
					Some(return_type),
					FunctionBody::Block(body)
				)
			);
		}
	}
}
