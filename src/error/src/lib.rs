mod span;

use colored::*;
use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

pub use span::{Span, Spanning};

/// Panics with a custom message.
#[rustfmt::skip]
#[macro_export]
macro_rules! internal_compiler_error {
	() => {
		panic!("internal compiler error")
	};

	( $( $arg:tt ),+ ) => {
		panic!("internal compiler error: {}", format!($( $arg ),+))
	}
}

/// Common error trait used across compiler crates allowing for a uniform
/// handling of logging and displaying errors.
pub trait Error {
	/// The classification for this error determines how it will be displayed
	/// to the user and handled by the system.
	fn classification(&self) -> ErrorClassification;

	/// A message describing the error.
	///
	/// A reference to the source text passed to this method so that implementers
	/// can extract information from it in order to compose their error message.
	/// A common example of this would be using a `Span` to embed some actual
	/// source text in the error message.
	fn message(&self, source_text: &str) -> String;

	/// An optional span denoting where in the source text the error occurred.
	///
	/// If supplied, an excerpt from the source text highlighting the span
	/// will be displayed along with the error message.
	fn context_span(&self) -> Option<Span> {
		None
	}
}

/// The classification of an error. Errors are grouped into three types:
///
/// * `Error`: for proper errors which are not recoverable and will cause
/// compilation to fail.
///
/// * `Warning`: for scenarios which are not proper errors but still hint that
/// something may be wrong. They do not cause compilation to fail.
///
/// * `Info`: for supplying additional information/context for errors/warnings.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorClassification {
	Error,
	Warning,
	Info,
}

impl ErrorClassification {
	fn message_color(&self) -> Color {
		use ErrorClassification::*;
		match self {
			Error => Color::BrightRed,
			Warning => Color::BrightYellow,
			Info => Color::White,
		}
	}
}

impl Display for ErrorClassification {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use ErrorClassification::*;
		let s = match self {
			Error => "Error",
			Warning => "Warning",
			Info => "Info",
		};
		write!(f, "{}", s)
	}
}

/// Renders an error type into a string.
pub fn render_error(err: &dyn Error, source_text: &str, filename: &str) -> String {
	let color = err.classification().message_color();
	let classification_text = format!("{}", err.classification()).color(color);
	let header = format!("{}: {}", classification_text, err.message(source_text));
	let filename_text = format!("  in {}", filename);
	let context = if let Some(span) = err.context_span() {
		format_context(source_text, span, color)
	} else {
		String::new()
	};
	[header, filename_text, context].join("\n")
}

/// Generates a textual representation of `span`.
///
/// # Panics
///
/// Panics if `span` is not a valid span in `source_text`
fn format_context(source_text: &str, span: Span, color: Color) -> String {
	let intersecting_lines = span.intersecting_lines(&span::line_spans(source_text));
	if intersecting_lines.is_empty() {
		return String::new();
	}

	// Compute the width of the line numbers which will be displayed in the margin.
	let line_number_width = intersecting_lines
		.iter()
		.map(|(num, _)| format!("{}", num + 1).len())
		.max()
		.unwrap();
	let line_number_padding = std::iter::repeat(' ').take(line_number_width).join("");

	// Format the intersecting lines.
	let lines = intersecting_lines.iter().map(|(num, span)| {
		let margin = format!(" {} | ", num + 1).cyan().bold();
		format!("{}{}", margin, span.substr_in(source_text))
	});
	let marker_lines = intersecting_lines.iter().map(|(_, line)| {
		format!(
			" {} {} {}",
			line_number_padding,
			"|".cyan().bold(),
			marker_line(*line, span).color(color).bold()
		)
	});

	// Interleave the source lines and marker lines to construct the final string.
	let interleaved = lines.interleave(marker_lines).join("\n");
	return [
		format!(" {} |", line_number_padding)
			.cyan()
			.bold()
			.to_string(),
		interleaved,
	]
	.join("\n");
}

/// Constructs a marker line for intersecting line and error spans.
///
/// # Panics
///
/// Panics if `err_span` does not intersect `line_span`.
fn marker_line(line_span: Span, err_span: Span) -> String {
	let marker = Span::intersect(line_span, err_span);
	let padding_len = marker.start - line_span.start;
	let padding = std::iter::repeat(' ').take(padding_len).join("");
	let marker = std::iter::repeat('~').take(marker.len()).join("");
	return padding + &marker;
}

/// An accumulator object for errors.
pub struct ErrorAccumulator {
	limit: usize,
	pub errors: Vec<Box<dyn Error>>,
}

impl ErrorAccumulator {
	/// Constructs a new accumulated with a specific limit on the number of
	/// errors allowed.
	pub fn with_limit(limit: usize) -> Self {
		ErrorAccumulator {
			limit,
			errors: Vec::new(),
		}
	}

	/// Adds a new error to the accumulator.
	pub fn push<E: Error + 'static>(&mut self, err: E) {
		self.errors.push(Box::new(err));
	}

	/// True if the accumulator contains an error.
	///
	/// Note that this does not include warning or info items.
	pub fn has_error(&self) -> bool {
		self.errors
			.iter()
			.any(|e| e.classification() == ErrorClassification::Error)
	}

	/// True if the number of errors logged has reached the preconfigured limit.
	///
	/// This is commonly used as an indication to the compiler that it should
	/// give up whatever it is doing and exit early.
	pub fn limit_reached(&self) -> bool {
		self.limit
			<= self
				.errors
				.iter()
				.filter(|e| e.classification() == ErrorClassification::Error)
				.count()
	}

	/// Writes the set of accumulated errors to standard error.
	pub fn write_to_stderr(&self, source_text: &str, filename: &str) {
		for err in &self.errors {
			eprintln!("{}\n", render_error(err.as_ref(), source_text, filename));
		}
	}
}
