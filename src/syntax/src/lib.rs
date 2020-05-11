pub mod lex;

/// A start and end pair of indices describing a substring.
///
/// Spans are used to mark locations of tokens in some source text. They are
/// dereferenced when an error is found and we want to show the user where it
/// occurred in the source text.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
	start: usize,
	end: usize,
}

impl Span {
	/// Constructs a new span with a given start and end.
	///
	/// # Panics
	///
	/// Panics if `end` is strictly less than `start`.
	pub fn new(start: usize, end: usize) -> Self {
		if end < start {
			panic!("invalid span: end is less than start: {} < {}", end, start);
		}

		Span { start, end }
	}

	/// Combines two spans into a new one which encompasses them both.
	///
	/// The resultant span will have the same start as `a` and the same end
	/// as `b`.
	///
	/// # Panics
	///
	/// Panics if `b.end` is strictly less than `a.start`.
	pub fn combine(a: Span, b: Span) -> Self {
		Span::new(a.start, b.end)
	}

	/// The length of this span.
	pub fn len(&self) -> usize {
		self.end - self.start
	}

	/// Indexes a given string returning the substring of it represented by this
	/// span.
	///
	/// # Panics
	///
	/// Panics if this span does not create a valid substring in `s`. This could
	/// happen because the span is out of bounds or it splits the string in the
	/// middle of a utf8 codepoint.
	pub fn substr_in<'a>(&self, s: &'a str) -> &'a str {
		&s[self.start..self.end]
	}
}
