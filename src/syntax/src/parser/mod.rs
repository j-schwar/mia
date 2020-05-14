use crate::lex::Keyword;
use crate::parser::prec_group::PrecedenceGroupParser;
use crate::*;
use error::{Error, ErrorClassification};
use std::iter::Peekable;
use these::These::{That, These, This};

mod prec_group;

/// Short circuits control flow by retuning from the current function if
/// `$these` is a `That` variant. If `$these` is not `That`, then it is
/// simply returned.
///
/// The current return type of the current function must be `ParseResult`.
#[rustfmt::skip]
#[macro_export]
macro_rules! ret_if_that {
	( $these:expr ) => {
		{
			let these = $these;
			if let That(err) = these {
				return That(err);
			} else {
				these
			}
		}
	};
}

/// Combines a bunch of results together grouping all of the `Here` values into
/// a tuple.
///
/// There is probably some smarter way to do this, but this will work for now.
#[rustfmt::skip]
macro_rules! combine_results {
	( $a:expr, $b:expr ) => { combine_results($a, $b) };

	( $a:expr, $b:expr, $c:expr ) => {
		combine_results($a, combine_results!($b, $c))
			.map_first(|(a, (b, c))| (a, b, c))
	};

	( $a:expr, $b:expr, $c:expr, $d:expr ) => {
		combine_results($a, combine_results!($b, $c, $d))
			.map_first(|(a, (b, c, d))| (a, b, c, d))
	};

	( $a:expr, $b:expr, $c:expr, $d:expr, $e:expr ) => {
		combine_results($a, combine_results!($b, $c, $d, $e))
			.map_first(|(a, (b, c, d, e))| (a, b, c, d, e))
	};

	( $a:expr, $b:expr, $c:expr, $d:expr, $e:expr, $f:expr ) => {
		combine_results($a, combine_results!($b, $c, $d, $e, $f))
			.map_first(|(a, (b, c, d, e, f))| (a, b, c, d, e, f))
	};
}

/// Error types for parsing operations.
#[derive(Debug, PartialEq)]
pub enum ParseError {
	UnexpectedToken {
		expected_one_of: Vec<TokenKind>,
		actual: Token,
	},

	UnexpectedEof,

	List(Vec<ParseError>),
}

impl ParseError {
	/// Converts a parse error into a list of errors.
	///
	/// By convention, the resultant vector should not contain any `List` error
	/// variants.
	pub fn into_vec(self) -> Vec<ParseError> {
		match self {
			ParseError::List(vec) => vec,
			err => vec![err],
		}
	}

	/// Extends this error by appending a new one to it.
	pub fn extend(self, other: ParseError) -> ParseError {
		match self {
			ParseError::List(mut vec) => {
				vec.append(&mut other.into_vec());
				ParseError::List(vec)
			}
			err => {
				let mut vec = err.into_vec();
				vec.append(&mut other.into_vec());
				ParseError::List(vec)
			}
		}
	}
}

impl Spanning for ParseError {
	fn span(&self) -> Span {
		use ParseError::*;
		match self {
			UnexpectedToken { actual, .. } => actual.span,
			UnexpectedEof => Span::new(0, 0),
			List(errors) => errors.first().unwrap().span(),
		}
	}
}

impl Error for ParseError {
	fn classification(&self) -> ErrorClassification {
		ErrorClassification::Error
	}

	fn message(&self, source_text: &str) -> String {
		use ParseError::*;
		match self {
			UnexpectedToken {
				expected_one_of,
				actual,
			} => format!(
				"unexpected token {}, expected one of {:?}",
				actual.span.substr_in(source_text),
				expected_one_of
			),
			UnexpectedEof => format!("unexpected end-of-file"),
			List(errors) => errors.first().unwrap().message(source_text),
		}
	}

	fn context_span(&self) -> Option<Span> {
		Some(self.span())
	}
}

/// The result type for parsing operations.
///
/// We use an error correcting parser which enables us to continue parsing even
/// if an error has occurred. This is in contrast to the naive approach which
/// simply aborts parsing at the first instance of an error.
pub type ParseResult<T> = these::These<T, ParseError>;

/// The parser converts a stream of tokens into an AST.
pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	/// Constructs a new parser for some source text.
	pub fn new(source: &'a str) -> Self {
		Parser {
			lexer: Lexer::new(source).peekable(),
		}
	}

	/// Parses a source file converting it into an abstract syntax tree.
	pub fn parse_source(source: &'a str) -> Result<Program, ParseError> {
		match Parser::new(source).parse_program() {
			This(program) => Ok(program),
			These(_, err) => Err(err),
			That(err) => Err(err),
		}
	}

	/// Parses a program.
	fn parse_program(&mut self) -> ParseResult<Program> {
		let mut functions = Vec::new();
		while self.lexer.peek().is_some() {
			functions.push(ret_if_that!(self.parse_function()));
			self.skip_whitespace();
		}
		return propagate_results(functions).map_first(Program::new);
	}

	/// Parses a function definition.
	fn parse_function(&mut self) -> ParseResult<Function> {
		// Parse the "fun" keyword.
		let keyword = ret_if_that!(self.expect(TokenKind::Keyword(Keyword::Fun), Payload::None));
		self.skip_whitespace();

		// Parse the function name.
		let ident = ret_if_that!(self.parse_ident());
		self.skip_whitespace();

		// Parse the parameter list.
		let params =
			self.parse_comma_separated_list(TokenKind::OpenParen, TokenKind::CloseParen, |p| {
				let ident = ret_if_that!(p.parse_ident());
				p.skip_whitespace();
				let colon = ret_if_that!(p.expect(TokenKind::Colon, Payload::None));
				p.skip_whitespace();
				let typename = ret_if_that!(p.parse_ident());
				combine_results!(ident, colon, typename)
					.map_first(|(ident, _, typename)| (ident, typename))
			});
		self.skip_whitespace();

		// Parse the return type.
		let return_type = if self.peek_has_kind(TokenKind::Arrow) {
			let arrow = ret_if_that!(self.expect(TokenKind::Arrow, Payload::None));
			self.skip_whitespace();
			let typename = ret_if_that!(self.parse_ident());
			combine_results(arrow, typename)
				.map_first(|(_, t)| t)
				.map_first(Option::Some)
		} else {
			This(None)
		};
		self.skip_whitespace();

		// Parse the function body.
		let body = if self.peek_has_kind(TokenKind::Assign) {
			self.consume(); // consume "="
			self.skip_whitespace();
			let expr = ret_if_that!(self.parse_expression());
			self.skip_whitespace();
			let semicolon = ret_if_that!(self.expect(TokenKind::Semicolon, Payload::None));
			combine_results(expr, semicolon).map_first(|(e, _)| FunctionBody::from(e))
		} else {
			ret_if_that!(self.parse_statement_block())
				.map_first(|(stmts, span)| FunctionBody::new_block(stmts, span))
		};

		// Combine results and return.
		combine_results!(keyword, ident, params, return_type, body).map_first(
			|(keyword, ident, (params, _), return_type, body)| {
				let span = Span::combine(keyword.span, body.span());
				Function::new(ident, params, return_type, body, span)
			},
		)
	}

	/// Parses a block of statements enclosed in "{" "}".
	fn parse_statement_block(&mut self) -> ParseResult<(Vec<Statement>, Span)> {
		// Parse opening "{".
		let opener = ret_if_that!(self.expect(TokenKind::OpenBrace, Payload::None));
		self.skip_whitespace();

		// Parse statements.
		let mut statements = Vec::new();
		while !self.peek_has_kind(TokenKind::CloseBrace) {
			statements.push(ret_if_that!(self.parse_statement()));
			self.skip_whitespace();
		}
		let statements = propagate_results(statements);

		// Parse closing "}".
		let closer = ret_if_that!(self.expect(TokenKind::CloseBrace, Payload::None));

		// Combine results and return.
		combine_results!(opener, statements, closer).map_first(|(opener, statements, closer)| {
			let span = Span::combine(opener.span, closer.span);
			(statements, span)
		})
	}

	/// Parses a statement.
	fn parse_statement(&mut self) -> ParseResult<Statement> {
		match self.peek_kind() {
			Some(TokenKind::Keyword(Keyword::Let)) | Some(TokenKind::Keyword(Keyword::Var)) => {
				self.parse_define_statement()
			}
			Some(TokenKind::Keyword(Keyword::If)) => self.parse_if_else_statement(),
			Some(TokenKind::Keyword(Keyword::Ret)) => self.parse_return_statement(),
			Some(TokenKind::Identifier) => self.parse_assign_statement(),
			Some(_) => self.parse_expression().map_first(Statement::from),
			_ => That(ParseError::UnexpectedEof),
		}
	}

	/// Parses a variable definition statement.
	fn parse_define_statement(&mut self) -> ParseResult<Statement> {
		// Parse "let" or "var" keyword token to determine if this definition is
		// mutable. Also record the span of this token as it will be used when
		// creating the span of the whole statement.
		let span_and_mutable = ret_if_that! {
			self.expect_one_of(
				vec![
					TokenKind::Keyword(Keyword::Let),
					TokenKind::Keyword(Keyword::Var),
				],
				Payload::None,
			)
			.map_first(|token| (token.span, token.kind == TokenKind::Keyword(Keyword::Let)))
		};
		self.skip_whitespace();

		// Parse the identifier.
		let ident = ret_if_that!(self.parse_ident());
		self.skip_whitespace();

		// Parse the typename.
		let typename = {
			let colon = ret_if_that!(self.expect(TokenKind::Colon, Payload::None));
			self.skip_whitespace();
			let typename = ret_if_that!(self.parse_ident());
			combine_results(colon, typename).map_first(|(_, tn)| tn)
		};
		self.skip_whitespace();

		// Parse the assignment operator.
		let assign = ret_if_that!(self.expect(TokenKind::Assign, Payload::None));
		self.skip_whitespace();

		// Parse the expression.
		let expr = ret_if_that!(self.parse_expression());
		self.skip_whitespace();

		// Parse the semicolon.
		let semicolon = ret_if_that!(self.expect(TokenKind::Semicolon, Payload::None));

		// Combine the results and return.
		combine_results!(span_and_mutable, ident, typename, assign, expr, semicolon).map_first(
			|((span, is_mutable), ident, typename, _, expr, semicolon)| {
				let span = Span::combine(span, semicolon.span);
				Statement::new_define(is_mutable, ident, typename, expr, span)
			},
		)
	}

	/// Parses an assignment statement.
	fn parse_assign_statement(&mut self) -> ParseResult<Statement> {
		// Parse the identifier.
		let ident = ret_if_that!(self.parse_ident());
		self.skip_whitespace();

		// Parse the assignment operator.
		let assign = ret_if_that!(self.expect(TokenKind::Assign, Payload::None));
		self.skip_whitespace();

		// Parse the expression.
		let expr = ret_if_that!(self.parse_expression());
		self.skip_whitespace();

		// Parse the semicolon.
		let semicolon = ret_if_that!(self.expect(TokenKind::Semicolon, Payload::None));

		// Combine the results and return.
		combine_results!(ident, assign, expr, semicolon).map_first(|(ident, _, expr, semicolon)| {
			let span = Span::combine(ident.span(), semicolon.span);
			Statement::new_assign(ident, expr, span)
		})
	}

	/// Parses an if statement with an optional else block.
	fn parse_if_else_statement(&mut self) -> ParseResult<Statement> {
		// Parse the "if" keyword.
		let if_keyword = ret_if_that!(self.expect(TokenKind::Keyword(Keyword::If), Payload::None));
		self.skip_whitespace();

		// Parse the condition.
		let condition = ret_if_that!(self.parse_expression());
		self.skip_whitespace();

		// Parse the true block.
		let then_block = ret_if_that!(self.parse_statement_block());
		self.skip_whitespace();

		// If the next token is not an "else" keyword, we are done.
		if !self.peek_has_kind(TokenKind::Keyword(Keyword::Else)) {
			return combine_results!(if_keyword, condition, then_block).map_first(
				|(keyword, condition, (block, block_span))| {
					let span = Span::combine(keyword.span, block_span);
					Statement::new_if(condition, block, span)
				},
			);
		}

		// Otherwise, parse the else keyword.
		self.consume(); // consume "else"
		self.skip_whitespace();

		// Parse the false block.
		let else_block = ret_if_that!(self.parse_statement_block());

		// Combine results and return.
		combine_results!(if_keyword, condition, then_block, else_block).map_first(
			|(keyword, condition, (then_block, _), (else_block, span))| {
				let span = Span::combine(keyword.span, span);
				Statement::new_if_else(condition, then_block, else_block, span)
			},
		)
	}

	/// Parses a return statement.
	fn parse_return_statement(&mut self) -> ParseResult<Statement> {
		let keyword = ret_if_that!(self.expect(TokenKind::Keyword(Keyword::Ret), Payload::None));
		self.skip_whitespace();
		let expr = ret_if_that!(self.parse_expression());
		self.skip_whitespace();
		let semicolon = ret_if_that!(self.expect(TokenKind::Semicolon, Payload::None));
		combine_results!(keyword, expr, semicolon).map_first(|(keyword, expr, semicolon)| {
			let span = Span::combine(keyword.span, semicolon.span);
			Statement::new_ret(expr, span)
		})
	}

	/// Parses a whole expression taking care to structure the resultant AST node
	/// in such a way which respects the precedence of the operators used.
	fn parse_expression(&mut self) -> ParseResult<Expression> {
		prec_group::PrecedenceGroup8::parse(self)
	}

	/// Parses expression precedence group 1.
	///
	/// This group is defined as such:
	///
	/// ```text
	/// expr_grp_1 ::= factor
	///              | ( "-" | "~" | "!" ) factor
	///              ;
	/// ```
	fn parse_expr_precedence_group_1(&mut self) -> ParseResult<Expression> {
		use TokenKind::*;
		if !self.peek_is_one_of(vec![Minus, Tilde, Exclamation]) {
			return self.parse_factor();
		}

		let operator = self.consume_operator_unwrap();
		let operand = self.parse_factor();
		return operand.map_first(|expr| Expression::new_unary_op(operator, expr));
	}

	/// Consumes the next token from the lexer and unwraps it as an operator token.
	///
	/// # Panics
	///
	/// Panics if there is no next token or it is unable to be converted into the
	/// desired operator type `O`. This method should only be used when we are
	/// certain that the next token is an operator token.
	fn consume_operator_unwrap<O>(&mut self) -> Operator<O>
	where
		O: TryFrom<TokenKind, Error = ()>,
	{
		let token = self.consume().unwrap();
		let span = token.span;
		let op = O::try_from(token.kind).unwrap();
		return Operator::new(op, span);
	}

	/// Parses a factor expression.
	///
	/// A factor is an expression with the highest precedence (i.e., the recursive
	/// base case) and is defined as:
	///
	/// ```text
	/// factor ::= identifier
	///          | literal
	///          | identifier "(" { expression "," } ")"  -- function call
	///          | "(" expression ")                      -- group
	///          ;
	/// ```
	fn parse_factor(&mut self) -> ParseResult<Expression> {
		use TokenKind::*;

		// If the next token is a literal, then parse that and return.
		if self.peek_is_one_of(vec![IntLiteral, BoolLiteral]) {
			return self.parse_literal().map_first(Expression::from);
		}

		// If the next token is an opening paren, parse a grouped expression.
		if self.peek_has_kind(OpenParen) {
			self.consume(); // consume '('
			self.skip_whitespace();
			let expr = ret_if_that!(self.parse_expression());
			self.skip_whitespace();
			let closer = ret_if_that!(self.expect(CloseParen, Payload::None));
			return combine_results(expr, closer).map_first(|(e, _)| e);
		}

		// Otherwise, then next token should be an identifier.
		let ident = ret_if_that!(self.parse_ident());
		self.skip_whitespace();

		// If there is no '(' after the identifier then we are done, otherwise we
		// parse a function call.
		if !self.peek_has_kind(OpenParen) {
			return ident.map_first(Expression::from);
		}

		let args =
			self.parse_comma_separated_list(TokenKind::OpenParen, TokenKind::CloseParen, |p| {
				p.parse_expression()
			});
		combine_results(ident, args).map_first(|(ident, (args, span))| {
			let span = Span::combine(ident.span(), span);
			Expression::new_call(ident, args, span)
		})
	}

	/// Parses a literal value.
	fn parse_literal(&mut self) -> ParseResult<Literal> {
		use TokenKind::*;
		self.expect_one_of(vec![IntLiteral, BoolLiteral], Payload::IntLiteral(0))
			.map_first(|token| {
				let span = token.span;
				match token.kind {
					IntLiteral => {
						let value = token.payload.unwrap_int_literal();
						Literal::new(LiteralKind::Int(value), span)
					}

					BoolLiteral => {
						let value = token.payload.unwrap_bool_literal();
						Literal::new(LiteralKind::Bool(value), span)
					}

					_ => unreachable!(),
				}
			})
	}

	/// Parses an identifier.
	fn parse_ident(&mut self) -> ParseResult<Identifier> {
		self.expect(TokenKind::Identifier, Payload::Identifier(String::new()))
			.map_first(|token| {
				let span = token.span;
				let text = token.payload.unwrap_identifier();
				Identifier::new(text, span)
			})
	}

	/// Invokes `sub_parser` 0 or more times to parse a comma separated list
	/// enclosed in specified tokens.
	fn parse_comma_separated_list<F, T>(
		&mut self,
		opener: TokenKind,
		closer: TokenKind,
		sub_parser: F,
	) -> ParseResult<(Vec<T>, Span)>
	where
		F: Fn(&mut Parser) -> ParseResult<T>,
	{
		// Parse the opener.
		let opener = ret_if_that!(self.expect(opener, Payload::None));
		self.skip_whitespace();

		// Parse the elements.
		let mut elements = Vec::new();
		if !self.peek_has_kind(closer) {
			loop {
				elements.push(ret_if_that!(sub_parser(self)));
				self.skip_whitespace();
				if !self.peek_has_kind(TokenKind::Comma) {
					break;
				}
				self.consume(); // consume ","
				self.skip_whitespace();
			}
		}
		let elements = propagate_results(elements);

		// Parse the closer.
		let closer = ret_if_that!(self.expect(closer, Payload::None));

		// Combine results and return.
		combine_results!(opener, elements, closer).map_first(|(opener, elements, closer)| {
			let span = Span::combine(opener.span, closer.span);
			(elements, span)
		})
	}

	/// True if the next token has a specific kind.
	///
	/// Does not consume the next token.
	fn peek_has_kind(&mut self, kind: TokenKind) -> bool {
		self.lexer.peek().map_or(false, |token| token.kind == kind)
	}

	/// True if the next token is one of a specific set of kinds.
	///
	/// Does not consume the next token.
	fn peek_is_one_of(&mut self, kinds: Vec<TokenKind>) -> bool {
		self.lexer
			.peek()
			.map_or(false, |token| token.is_one_of(kinds))
	}

	/// Consumes a single token from the lexer making sure it is the kind of
	/// token we expect.
	///
	/// There are three possible cases to consider, all of which have different
	/// return values:
	///
	/// * If the consumed token is the correct kind, then it is returned as
	/// a `This` variant.
	///
	/// * If the consumed token is not the correct kind, then an error is
	/// returned along with a default token of the expected kind as a `These`
	/// variant.
	///
	/// * If we are unable to consume a token due to there being no more tokens
	/// left to consume, then an unexpected EOF error is returned as a `That`
	/// variant.
	fn expect(&mut self, kind: TokenKind, default_payload: Payload) -> ParseResult<Token> {
		match self.lexer.next() {
			Some(token) => {
				if token.kind == kind {
					This(token)
				} else {
					let span = token.span;
					let err = ParseError::UnexpectedToken {
						expected_one_of: vec![kind],
						actual: token,
					};
					These(Token::with_payload(kind, default_payload, span), err)
				}
			}
			None => That(ParseError::UnexpectedEof),
		}
	}

	/// Consumes a single token from the lexer making sure it is one of the
	/// expected kinds of tokens.
	///
	/// The result scheme is the same as the [`expect`](#method.expect) method
	/// except that in the event of an unexpected token, the first kind in
	/// `kinds` is used as the corrected token.
	fn expect_one_of(
		&mut self,
		kinds: Vec<TokenKind>,
		default_payload: Payload,
	) -> ParseResult<Token> {
		match self.lexer.next() {
			Some(token) => {
				if kinds.contains(&token.kind) {
					This(token)
				} else {
					let corrected_kind = kinds[0];
					let span = token.span;
					let err = ParseError::UnexpectedToken {
						expected_one_of: kinds,
						actual: token,
					};
					These(
						Token::with_payload(corrected_kind, default_payload, span),
						err,
					)
				}
			}
			None => That(ParseError::UnexpectedEof),
		}
	}

	fn consume_while<P>(&mut self, predicate: P) -> Vec<Token>
	where
		P: Fn(&Token) -> bool,
	{
		let mut tokens = Vec::new();
		while self.lexer.peek().map_or(false, &predicate) {
			tokens.push(self.lexer.next().unwrap());
		}
		return tokens;
	}

	/// Skips over whitespace and comment tokens.
	fn skip_whitespace(&mut self) {
		self.consume_while(|t| t.is_whitespace() || t.is_comment());
	}

	/// Consumes the next token.
	fn consume(&mut self) -> Option<Token> {
		self.lexer.next()
	}

	/// Peeks a the kind of the next token.
	fn peek_kind(&mut self) -> Option<TokenKind> {
		self.lexer.peek().map(|t| t.kind)
	}
}

fn decompose<T, U>(these: these::These<T, U>) -> (Option<T>, Option<U>) {
	match these {
		This(a) => (Some(a), None),
		These(a, e) => (Some(a), Some(e)),
		That(e) => (None, Some(e)),
	}
}

/// Combines two results into a single one.
fn combine_results<T, U>(a: ParseResult<T>, b: ParseResult<U>) -> ParseResult<(T, U)> {
	let (a, a_err) = decompose(a);
	let (b, b_err) = decompose(b);

	let err = if let Some(a_err) = a_err {
		if let Some(b_err) = b_err {
			Some(a_err.extend(b_err))
		} else {
			Some(a_err)
		}
	} else if let Some(b_err) = b_err {
		Some(b_err)
	} else {
		None
	};

	if a.is_none() || b.is_none() {
		// err can only be `None` when both `a` and `b` are `That`.
		return That(err.unwrap());
	}

	let ok = (a.unwrap(), b.unwrap());
	match err {
		Some(err) => These(ok, err),
		None => This(ok),
	}
}

/// Transforms a vector of results into a result containing a vector.
fn propagate_results<T>(vec: Vec<ParseResult<T>>) -> ParseResult<Vec<T>> {
	let mut ok_vec = Vec::new();
	let mut err_vec = Vec::new();
	for x in vec {
		match x {
			This(y) => {
				ok_vec.push(y);
			}
			These(y, z) => {
				ok_vec.push(y);
				err_vec.push(z);
			}
			That(x) => return That(x),
		}
	}

	if err_vec.is_empty() {
		This(ok_vec)
	} else {
		These(ok_vec, ParseError::List(err_vec))
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_parse_identifier() {
		let mut parser = Parser::new("hello_world");
		assert_eq!(
			parser.parse_ident(),
			This(Identifier::new("hello_world", Span::new(0, 11)))
		);
	}

	#[test]
	fn test_parse_identifier_error() {
		let mut parser = Parser::new("!foo");
		assert_eq!(
			parser.parse_ident(),
			These(
				Identifier::new("", Span::new(0, 1)),
				ParseError::UnexpectedToken {
					expected_one_of: vec![TokenKind::Identifier],
					actual: Token::new(TokenKind::Exclamation, Span::new(0, 1))
				}
			)
		);
	}

	#[test]
	fn test_parse_int_literal() {
		let mut parser = Parser::new("123");
		assert_eq!(
			parser.parse_literal(),
			This(Literal::new(LiteralKind::Int(123), Span::new(0, 3)))
		);
	}

	#[test]
	fn test_parse_int_literal_error() {
		let mut parser = Parser::new("a123");
		assert_eq!(
			parser.parse_literal(),
			These(
				Literal::new(LiteralKind::Int(0), Span::new(0, 4)),
				ParseError::UnexpectedToken {
					expected_one_of: vec![TokenKind::IntLiteral, TokenKind::BoolLiteral],
					actual: Token::with_payload(
						TokenKind::Identifier,
						Payload::Identifier("a123".to_string()),
						Span::new(0, 4),
					)
				}
			)
		);
	}

	#[test]
	fn test_parse_bool_literal() {
		let mut parser = Parser::new("false");
		assert_eq!(
			parser.parse_literal(),
			This(Literal::new(LiteralKind::Bool(false), Span::new(0, 5)))
		);
	}

	#[test]
	fn test_parse_literal_factor() {
		let mut parser = Parser::new("true");
		assert_eq!(
			parser.parse_factor(),
			This(Expression::from(Literal::new(
				LiteralKind::Bool(true),
				Span::new(0, 4)
			))),
		);
	}

	#[test]
	fn test_parse_precedence_group_1_with_literal() {
		let mut parser = Parser::new("!true");
		let expected = Expression::new_unary_op(
			Operator::new(UnaryOp::Not, Span::new(0, 1)),
			Literal::new(LiteralKind::Bool(true), Span::new(1, 5)),
		);
		assert_eq!(parser.parse_expr_precedence_group_1(), This(expected));
	}
}
