use super::*;

pub trait PrecedenceGroupParser {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression>;

	fn operator_kinds() -> Vec<TokenKind>;

	fn parse_initial_operation(parser: &mut Parser) -> ParseResult<Expression> {
		let lhs = ret_if_that!(Self::parse_term(parser));
		parser.skip_whitespace();

		// If the next token is not one of the desired operators, then simply
		// return the left operand.
		if !parser.peek_is_one_of(Self::operator_kinds()) {
			return lhs;
		}

		// Otherwise, parse the operand and the right hand side.
		let operator = parser.consume_operator_unwrap();
		parser.skip_whitespace();
		let rhs = ret_if_that!(Self::parse_term(parser));
		let composed = combine_results(lhs, rhs);
		return composed.map_first(|(lhs, rhs)| Expression::new_binary_op(operator, lhs, rhs));
	}

	fn extend_operation(
		parser: &mut Parser,
		initial: ParseResult<Expression>,
	) -> ParseResult<Expression> {
		parser.skip_whitespace();
		if !parser.peek_is_one_of(Self::operator_kinds()) {
			return initial;
		}

		let operator = parser.consume_operator_unwrap();
		parser.skip_whitespace();
		let rhs = ret_if_that!(Self::parse_term(parser));
		let composed = combine_results(initial, rhs);
		let extended =
			composed.map_first(|(lhs, rhs)| Expression::new_binary_op(operator, lhs, rhs));
		return Self::extend_operation(parser, extended);
	}

	fn parse(parser: &mut Parser) -> ParseResult<Expression> {
		let initial = Self::parse_initial_operation(parser);
		return Self::extend_operation(parser, initial);
	}
}

/// Precedence group 2 includes the arithmetical multiplication, division, and
/// remainder operations. It is defined as such:
///
/// ```text
/// expr_grp_2 ::= expr_grp_1
///              | expr_grp_2 ( "*" | "/" | "%" ) expr_grp_1
///              ;
/// ```
pub struct PrecedenceGroup2;

impl PrecedenceGroupParser for PrecedenceGroup2 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		parser.parse_expr_precedence_group_1()
	}

	fn operator_kinds() -> Vec<TokenKind> {
		use TokenKind::*;
		vec![Star, Slash, Percent]
	}
}

/// Precedence group 3 contains the arithmetical addition and subtraction
/// operations and is defined as such:
///
/// ```text
/// expr_grp_3 ::= expr_grp_2
///              | expr_grp_3 ( "+" | "-" ) expr_grp_2
///              ;
/// ```
pub struct PrecedenceGroup3;

impl PrecedenceGroupParser for PrecedenceGroup3 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup2::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		use TokenKind::*;
		vec![Plus, Minus]
	}
}

/// Precedence group 4 contains the relational operators.
///
/// ```text
/// expr_grp_4 ::= expr_grp_3
///              | expr_grp_4 ( "<" | "<=" | ">" | ">=" ) expr_grp_3
///              ;
/// ```
pub struct PrecedenceGroup4;

impl PrecedenceGroupParser for PrecedenceGroup4 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup3::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		use TokenKind::*;
		vec![Gt, Ge, Lt, Le]
	}
}

/// Precedence group 5 contains the equality and inequality operators.
///
/// ```text
/// expr_grp_5 ::= expr_grp_4
///              | expr_grp_5 ( "<" | "<=" | ">" | ">=" ) expr_grp_4
///              ;
/// ```
pub struct PrecedenceGroup5;

impl PrecedenceGroupParser for PrecedenceGroup5 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup4::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		use TokenKind::*;
		vec![Eq, Ne]
	}
}

/// Precedence group 6 contains just the bitwise and operator.
///
/// ```text
/// expr_grp_6 ::= expr_grp_5
///              | expr_grp_6 "& expr_grp_5
///              ;
/// ```
pub struct PrecedenceGroup6;

impl PrecedenceGroupParser for PrecedenceGroup6 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup5::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		vec![TokenKind::And]
	}
}

/// Precedence group 7 contains just the bitwise xor operator.
///
/// ```text
/// expr_grp_7 ::= expr_grp_6
///              | expr_grp_7 "& expr_grp_6
///              ;
/// ```
pub struct PrecedenceGroup7;

impl PrecedenceGroupParser for PrecedenceGroup7 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup6::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		vec![TokenKind::Hat]
	}
}

/// Precedence group 8 contains just the bitwise or operator.
///
/// ```text
/// expr_grp_8 ::= expr_grp_7
///              | expr_grp_8 "& expr_grp_7
///              ;
/// ```
pub struct PrecedenceGroup8;

impl PrecedenceGroupParser for PrecedenceGroup8 {
	fn parse_term(parser: &mut Parser) -> ParseResult<Expression> {
		PrecedenceGroup7::parse(parser)
	}

	fn operator_kinds() -> Vec<TokenKind> {
		vec![TokenKind::Bar]
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_parse_precedence_group_2_with_literals() {
		let mut parser = Parser::new("-1 * 2 / 4");

		let a = Expression::new_unary_op(
			Operator::new(UnaryOp::Neg, Span::new(0, 1)),
			Literal::new(LiteralKind::Int(1), Span::new(1, 2)),
		);
		let b: Expression = Literal::new(LiteralKind::Int(2), Span::new(5, 6)).into();
		let c: Expression = Literal::new(LiteralKind::Int(4), Span::new(9, 10)).into();
		let mul = Operator::new(BinaryOp::Mul, Span::new(3, 4));
		let div = Operator::new(BinaryOp::Div, Span::new(7, 8));

		let expected = Expression::new_binary_op(div, Expression::new_binary_op(mul, a, b), c);

		assert_eq!(PrecedenceGroup2::parse(&mut parser), This(expected));
	}
}
