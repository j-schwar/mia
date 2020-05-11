//! This module contains the lexer for the language.

use crate::Span;
use std::convert::TryFrom;
use std::iter::Peekable;
use std::str::Chars;

/// A source token.
///
/// Tokens are constructed by the lexer, then passed to the parser which uses
/// then to construct an AST. The `kind` field of the token contains all the
/// useful information about what kind of token it is and any associated data
/// that accompanies it. The `span` field contains the location of the token
/// in the source text it was extracted from.
#[derive(Debug, PartialEq)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

impl Token {
	/// Constructs a new token.
	pub fn new(kind: TokenKind, span: Span) -> Self {
		Token { kind, span }
	}
}

/// Describes the kind of a token and contains additional information about it
/// like a literal's value or an identifier's name for example.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
	/* Background Tokens */
	Whitespace,
	Comment,

	/* Textual Tokens */
	Keyword(Keyword),
	Identifier(String),

	/* Literal Tokens */
	IntLiteral(i64),
	BoolLiteral(bool),

	/* Operators */
	Plus,        // +
	Minus,       // -
	Star,        // *
	Slash,       // /
	Percent,     // %
	And,         // &
	Bar,         // |
	Hat,         // ^
	Eq,          // ==
	Ne,          // !=
	Gt,          // >
	Ge,          // >=
	Lt,          // <
	Le,          // <=
	Exclamation, // !
	Assign,      // =

	/* Control Tokens */
	Arrow,        // ->
	Comma,        // ,
	Colon,        // :
	Semicolon,    // ;
	OpenParen,    // (
	CloseParen,   // )
	OpenBracket,  // [
	CloseBracket, // ]
	OpenBrace,    // {
	CloseBrace,   // }

	/* Error */
	Unknown,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
	/* Declaration Keywords */
	Fun, // fun
	Let, // let
	Var, // var

	/* Control Flow Keywords */
	If,   // if
	Else, // else
	Ret,  // ret
}

impl TryFrom<&str> for Keyword {
	type Error = ();

	/// Attempts to convert a string into a keyword.
	///
	/// Returns an error if unable to do so.
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		use Keyword::*;
		match value {
			"fun" => Ok(Fun),
			"let" => Ok(Let),
			"var" => Ok(Var),
			"if" => Ok(If),
			"else" => Ok(Else),
			"ret" => Ok(Ret),
			_ => Err(()),
		}
	}
}

/// The lexer converts source text into a sequence of tokens.
///
/// It is built as an iterator over `Token` items.
pub struct Lexer<'a> {
	source_iter: Peekable<Chars<'a>>,
	count: usize,
}

impl<'a> Lexer<'a> {
	/// Constructs a new lexer over `source`.
	pub fn new(source: &'a str) -> Self {
		Lexer {
			source_iter: source.chars().peekable(),
			count: 0,
		}
	}

	/// Advances the lexer returning the next character in the source.
	fn consume_char(&mut self) -> Option<char> {
		self.count += 1;
		self.source_iter.next()
	}

	/// Returns the next character in the source without consuming it.
	fn peek_char(&mut self) -> Option<char> {
		self.source_iter.peek().map(|c| c.clone())
	}

	/// Consumes characters while a given predicate is satisfied.
	///
	/// Returns the accumulated characters as a string once finished.
	fn take_while<P>(&mut self, predicate: P) -> String
	where
		P: Fn(char) -> bool,
	{
		let mut s = String::new();
		while self.peek_char().map_or(false, &predicate) {
			s.push(self.consume_char().unwrap());
		}
		return s;
	}

	/// Consumes characters while a given predicate is satisfied.
	///
	/// Unlike `take_while`, the accumulated characters are simply discarded.
	fn skip_while<P>(&mut self, predicate: P)
	where
		P: Fn(char) -> bool,
	{
		while self.peek_char().map_or(false, &predicate) {
			self.consume_char();
		}
	}

	/// Consumes a single character and returns it as a specific kind of token.
	///
	/// # Panics
	///
	/// Panics if the consumed character does not match `c`. This is done solely
	/// for bug-finding purposes (may remove later).
	fn consume_single_char_token(&mut self, kind: TokenKind) -> Token {
		let start_index = self.count;
		self.consume_char();
		let span = Span::new(start_index, self.count);
		return Token::new(kind, span);
	}

	/// Consumes a sequence of whitespace characters.
	fn consume_whitespace(&mut self) -> Token {
		let start_index = self.count;
		self.skip_while(|c: char| c.is_ascii_whitespace());
		let span = Span::new(start_index, self.count);
		return Token::new(TokenKind::Whitespace, span);
	}

	/// True if `c` is a starting character for an identifier.
	fn is_ident_start(c: char) -> bool {
		c == '_' || c.is_ascii_alphabetic()
	}

	/// True if `c` is a continuation character for an identifier.
	fn is_ident_continue(c: char) -> bool {
		c == '_' || c.is_ascii_alphanumeric()
	}

	/// Consumes an identifier or keyword token.
	///
	/// Identifiers are strings which match the following regular expression:
	///
	/// ```text
	/// [_a-zA-Z][_a-zA-Z0-9]*
	/// ```
	///
	/// # Panics
	///
	/// Panics if unable to construct an identifier token from the lexer's
	/// current location. Care should be taken by the distributed to ensure
	/// that there is actually an identifier or keyword token at this location.
	fn consume_identifier_or_keyword(&mut self) -> Token {
		let start_index = self.count;
		let start = self.take_while(Self::is_ident_start);
		if start.is_empty() {
			panic!("internal lexer error: unable to start identifier token");
		}
		let continuation = self.take_while(Self::is_ident_continue);
		let span = Span::new(start_index, self.count);
		let text = start + &continuation;

		if text == "true" {
			return Token::new(TokenKind::BoolLiteral(true), span);
		} else if text == "false" {
			return Token::new(TokenKind::BoolLiteral(false), span);
		}

		if let Ok(keyword) = Keyword::try_from(text.as_str()) {
			Token::new(TokenKind::Keyword(keyword), span)
		} else {
			Token::new(TokenKind::Identifier(text), span)
		}
	}

	/// Consumes an integer literal.
	///
	/// # Panics
	///
	/// Panics if unable to construct the token.
	fn consume_integer_literal(&mut self) -> Token {
		let start_index = self.count;
		let text = self.take_while(|c: char| c.is_ascii_digit());
		if text.is_empty() {
			panic!("internal lexer error: unable to construct integer literal");
		}
		let value = text.parse::<i64>().unwrap();
		let span = Span::new(start_index, self.count);
		return Token::new(TokenKind::IntLiteral(value), span);
	}

	/// Consumes a 1 or 2 character long token where the first character `c1` is
	/// mandatory and the second character `c2` is optional.
	///
	/// Returns a token with kind `single_kind` if the second character did not
	/// match and kind `dual_kind` if it did.
	///
	/// # Panics
	///
	/// Panics if unable to match the first character `c1`.
	fn consume_dual_width_token(
		&mut self,
		c1: char,
		c2: char,
		single_kind: TokenKind,
		dual_kind: TokenKind,
	) -> Token {
		let start_index = self.count;
		if self.consume_char() != Some(c1) {
			panic!("internal lexer error: expected '{}'", c1);
		}
		let kind = if self.peek_char() == Some(c2) {
			self.consume_char();
			dual_kind
		} else {
			single_kind
		};
		let span = Span::new(start_index, self.count);
		return Token::new(kind, span);
	}

	/// Consumes a "/" or comment token.
	///
	/// # Panics
	///
	/// Panics if the leading "/" cannot be found.
	fn consume_slash_or_comment(&mut self) -> Token {
		let start_index = self.count;
		if self.consume_char() != Some('/') {
			panic!("internal lexer error: expected '/'");
		}
		let kind = match self.peek_char() {
			Some('/') => {
				self.skip_while(|c| c != '\n');
				TokenKind::Comment
			}
			_ => TokenKind::Slash,
		};
		let span = Span::new(start_index, self.count);
		return Token::new(kind, span);
	}

	/// Consumes a single character and returns an `Unknown` token.
	fn consume_unknown_character(&mut self) -> Token {
		let start_index = self.count;
		self.consume_char();
		let span = Span::new(start_index, self.count);
		return Token::new(TokenKind::Unknown, span);
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token;

	/// Constructs and returns the next token.
	fn next(&mut self) -> Option<Self::Item> {
		match self.peek_char()? {
			/* Single Char Tokens */
			'+' => self.consume_single_char_token(TokenKind::Plus),
			'*' => self.consume_single_char_token(TokenKind::Star),
			'%' => self.consume_single_char_token(TokenKind::Percent),
			'&' => self.consume_single_char_token(TokenKind::And),
			'|' => self.consume_single_char_token(TokenKind::Bar),
			'^' => self.consume_single_char_token(TokenKind::Hat),
			',' => self.consume_single_char_token(TokenKind::Comma),
			':' => self.consume_single_char_token(TokenKind::Colon),
			';' => self.consume_single_char_token(TokenKind::Semicolon),
			'(' => self.consume_single_char_token(TokenKind::OpenParen),
			')' => self.consume_single_char_token(TokenKind::CloseParen),
			'[' => self.consume_single_char_token(TokenKind::OpenBracket),
			']' => self.consume_single_char_token(TokenKind::CloseBracket),
			'{' => self.consume_single_char_token(TokenKind::OpenBrace),
			'}' => self.consume_single_char_token(TokenKind::CloseBrace),

			/* Multi-Char Operator Tokens */
			'-' => self.consume_dual_width_token('-', '>', TokenKind::Minus, TokenKind::Arrow),
			'!' => self.consume_dual_width_token('!', '=', TokenKind::Exclamation, TokenKind::Ne),
			'=' => self.consume_dual_width_token('=', '=', TokenKind::Assign, TokenKind::Eq),
			'<' => self.consume_dual_width_token('<', '=', TokenKind::Lt, TokenKind::Le),
			'>' => self.consume_dual_width_token('>', '=', TokenKind::Gt, TokenKind::Ge),
			'/' => self.consume_slash_or_comment(),

			/* Complex Tokens */
			c if c.is_ascii_whitespace() => self.consume_whitespace(),
			c if Self::is_ident_start(c) => self.consume_identifier_or_keyword(),
			c if c.is_ascii_digit() => self.consume_integer_literal(),
			_ => self.consume_unknown_character(),
		}
		.into()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	/// Extracts a single token from `source` and returns it.
	///
	/// Ensures that there are not extra tokens after the one that was extracted.
	fn lex_single_token(source: &str) -> Token {
		let mut lexer = Lexer::new(source);
		let token = lexer.next().expect("expected token, found None");
		assert_eq!(lexer.next(), None);
		return token;
	}

	fn token(kind: TokenKind, start: usize, end: usize) -> Token {
		Token::new(kind, Span::new(start, end))
	}

	#[test]
	fn test_whitespace_token() {
		assert_eq!(
			lex_single_token(" \r\n\t\t "),
			token(TokenKind::Whitespace, 0, 6)
		);
	}

	#[test]
	fn test_comment_token() {
		assert_eq!(
			lex_single_token("// comment"),
			token(TokenKind::Comment, 0, 10)
		);
	}

	#[test]
	fn test_keyword_token() {
		assert_eq!(
			lex_single_token("fun"),
			token(TokenKind::Keyword(Keyword::Fun), 0, 3)
		);
		assert_eq!(
			lex_single_token("let"),
			token(TokenKind::Keyword(Keyword::Let), 0, 3)
		);
		assert_eq!(
			lex_single_token("var"),
			token(TokenKind::Keyword(Keyword::Var), 0, 3)
		);
		assert_eq!(
			lex_single_token("if"),
			token(TokenKind::Keyword(Keyword::If), 0, 2)
		);
		assert_eq!(
			lex_single_token("else"),
			token(TokenKind::Keyword(Keyword::Else), 0, 4)
		);
		assert_eq!(
			lex_single_token("ret"),
			token(TokenKind::Keyword(Keyword::Ret), 0, 3)
		);
	}

	#[test]
	fn test_identifier_token() {
		assert_eq!(
			lex_single_token("_Foo_42"),
			token(TokenKind::Identifier("_Foo_42".into()), 0, 7),
		);
		assert_eq!(
			lex_single_token("_"),
			token(TokenKind::Identifier("_".into()), 0, 1),
		);
	}

	#[test]
	fn test_int_literal_token() {
		assert_eq!(lex_single_token("0"), token(TokenKind::IntLiteral(0), 0, 1));
		assert_eq!(
			lex_single_token("120345"),
			token(TokenKind::IntLiteral(120345), 0, 6)
		);
	}

	#[test]
	fn test_bool_literal() {
		assert_eq!(
			lex_single_token("true"),
			token(TokenKind::BoolLiteral(true), 0, 4)
		);
		assert_eq!(
			lex_single_token("false"),
			token(TokenKind::BoolLiteral(false), 0, 5),
		);
	}

	#[test]
	fn test_plus_token() {
		assert_eq!(lex_single_token("+"), token(TokenKind::Plus, 0, 1));
	}

	#[test]
	fn test_minus_token() {
		assert_eq!(lex_single_token("-"), token(TokenKind::Minus, 0, 1));
	}

	#[test]
	fn test_star_token() {
		assert_eq!(lex_single_token("*"), token(TokenKind::Star, 0, 1));
	}

	#[test]
	fn test_slash_token() {
		assert_eq!(lex_single_token("/"), token(TokenKind::Slash, 0, 1));
	}

	#[test]
	fn test_percent_token() {
		assert_eq!(lex_single_token("%"), token(TokenKind::Percent, 0, 1));
	}

	#[test]
	fn test_and_token() {
		assert_eq!(lex_single_token("&"), token(TokenKind::And, 0, 1));
	}

	#[test]
	fn test_bar_token() {
		assert_eq!(lex_single_token("|"), token(TokenKind::Bar, 0, 1));
	}

	#[test]
	fn test_hat_token() {
		assert_eq!(lex_single_token("^"), token(TokenKind::Hat, 0, 1));
	}

	#[test]
	fn test_eq_token() {
		assert_eq!(lex_single_token("=="), token(TokenKind::Eq, 0, 2));
	}

	#[test]
	fn test_ne_token() {
		assert_eq!(lex_single_token("!="), token(TokenKind::Ne, 0, 2));
	}

	#[test]
	fn test_gt_token() {
		assert_eq!(lex_single_token(">"), token(TokenKind::Gt, 0, 1));
	}

	#[test]
	fn test_ge_token() {
		assert_eq!(lex_single_token(">="), token(TokenKind::Ge, 0, 2));
	}

	#[test]
	fn test_lt_token() {
		assert_eq!(lex_single_token("<"), token(TokenKind::Lt, 0, 1));
	}

	#[test]
	fn test_le_token() {
		assert_eq!(lex_single_token("<="), token(TokenKind::Le, 0, 2));
	}

	#[test]
	fn test_exclamation_token() {
		assert_eq!(lex_single_token("!"), token(TokenKind::Exclamation, 0, 1));
	}

	#[test]
	fn test_assign_token() {
		assert_eq!(lex_single_token("="), token(TokenKind::Assign, 0, 1));
	}

	#[test]
	fn test_arrow_token() {
		assert_eq!(lex_single_token("->"), token(TokenKind::Arrow, 0, 2));
	}

	#[test]
	fn test_comma_token() {
		assert_eq!(lex_single_token(","), token(TokenKind::Comma, 0, 1));
	}

	#[test]
	fn test_colon_token() {
		assert_eq!(lex_single_token(":"), token(TokenKind::Colon, 0, 1));
	}

	#[test]
	fn test_semicolon_token() {
		assert_eq!(lex_single_token(";"), token(TokenKind::Semicolon, 0, 1));
	}

	#[test]
	fn test_open_paren_token() {
		assert_eq!(lex_single_token("("), token(TokenKind::OpenParen, 0, 1));
	}

	#[test]
	fn test_close_paren_token() {
		assert_eq!(lex_single_token(")"), token(TokenKind::CloseParen, 0, 1));
	}

	#[test]
	fn test_open_bracket_token() {
		assert_eq!(lex_single_token("["), token(TokenKind::OpenBracket, 0, 1));
	}

	#[test]
	fn test_close_bracket_token() {
		assert_eq!(lex_single_token("]"), token(TokenKind::CloseBracket, 0, 1));
	}

	#[test]
	fn test_open_brace_token() {
		assert_eq!(lex_single_token("{"), token(TokenKind::OpenBrace, 0, 1));
	}

	#[test]
	fn test_close_brace_token() {
		assert_eq!(lex_single_token("}"), token(TokenKind::CloseBrace, 0, 1));
	}
}
