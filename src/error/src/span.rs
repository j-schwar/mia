use std::cmp;

/// A inclusive start and exclusive end pair of indices describing a substring.
///
/// Spans are used to mark locations of tokens in some source text. They are
/// dereferenced when an error is found and we want to show the user where it
/// occurred in the source text.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
	pub start: usize,
	pub end: usize,
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

	/// Combines two spans into a new one representing the intersection between
	/// `a` and `b`.
	///
	/// # Panics
	///
	/// Panics if `a` and `b` don't intersect.
	pub fn intersect(a: Span, b: Span) -> Self {
		let start = cmp::max(a.start, b.start);
		let end = cmp::min(a.end, b.end);
		return Span::new(start, end);
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

	/// True if `index` is contained within this span.
	pub fn contains_index(&self, index: usize) -> bool {
		index >= self.start && index < self.end
	}

	/// True if this span and `other` intersect.
	pub fn intersects(&self, other: &Span) -> bool {
		self.contains_index(other.start) || other.contains_index(self.start)
	}

	/// Returns a list of line spans (paired with their index's) which intersect
	/// the span.
	pub fn intersecting_lines(&self, line_spans: &LineSpans) -> Vec<(usize, Span)> {
		line_spans
			.iter()
			.enumerate()
			.filter(|(_, line)| self.intersects(*line))
			.map(|(num, line)| (num, *line))
			.collect()
	}
}

/// Trait for types which emit a `Span`.
pub trait Spanning {
	/// The span of this object.
	fn span(&self) -> Span;

	/// Uses this object's span to create as substring in `s`.
	///
	/// # Panics
	///
	/// Panics if this object's span does not create a valid substring in `s`.
	fn substr_in<'a>(&self, s: &'a str) -> &'a str {
		self.span().substr_in(s)
	}
}

pub type LineSpans = Vec<Span>;

/// Computes the start and end positions of the lines in `s`.
pub fn line_spans(s: &str) -> LineSpans {
	s.lines()
		.map(|line| {
			let offset = line.as_ptr() as usize - s.as_ptr() as usize;
			let end = offset + line.len();
			Span::new(offset, end)
		})
		.collect()
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_span_contains_index() {
		let span = Span::new(1, 5);
		assert!(span.contains_index(1));
		assert!(span.contains_index(4));
		assert!(!span.contains_index(5));
		assert!(!span.contains_index(7));
	}

	#[test]
	fn test_span_intersection() {
		let a = Span::new(2, 5);
		let b = Span::new(4, 6);
		let c = Span::new(6, 8);
		assert!(a.intersects(&b));
		assert!(b.intersects(&a));
		assert!(!a.intersects(&c));
		assert!(!b.intersects(&c));
	}

	#[test]
	fn test_empty_span_intersection() {
		let a = Span::new(0, 0);
		let b = Span::new(0, 1);
		let c = Span::new(1, 2);
		assert!(a.intersects(&b));
		assert!(!a.intersects(&c));
	}

	#[test]
	fn test_line_spans_lf_only() {
		let s = "hello\nworld\nhow are you doing today?";
		let lines = vec!["hello", "world", "how are you doing today?"];
		let actual_lines = line_spans(s)
			.into_iter()
			.map(|span| span.substr_in(s))
			.collect::<Vec<_>>();
		assert_eq!(actual_lines, lines);
	}

	#[test]
	fn test_line_spans_crlf_only() {
		let s = "hello\r\nworld\r\nhow are you doing today?";
		let lines = vec!["hello", "world", "how are you doing today?"];
		let actual_lines = line_spans(s)
			.into_iter()
			.map(|span| span.substr_in(s))
			.collect::<Vec<_>>();
		assert_eq!(actual_lines, lines);
	}

	#[test]
	fn test_line_spans_mixed_endings() {
		let s = "hello\nworld\r\nhow are you \rdoing today?";
		let lines = vec!["hello", "world", "how are you \rdoing today?"];
		let actual_lines = line_spans(s)
			.into_iter()
			.map(|span| span.substr_in(s))
			.collect::<Vec<_>>();
		assert_eq!(actual_lines, lines);
	}
}
