use std::fmt::{Display, Formatter, Result as FmtResult};

/// Display trait with additional context.
pub trait ContextualDisplay<Context> {
	/// Formats this object using a given context.
	fn fmt(&self, context: Context, f: &mut Formatter<'_>) -> FmtResult;
}

#[derive(Clone)]
pub struct IndentContext {
	/// The current size of the indent.
	size: usize,

	/// The text to display as an indent.
	text: &'static str,
}

impl IndentContext {
	/// Constructs a new indent context using `text` as the indent text.
	pub fn new(text: &'static str) -> Self {
		IndentContext { size: 0, text }
	}

	/// Constructs an indent context which uses two spaces as indents.
	pub fn two_spaces() -> Self {
		Self::new("  ")
	}

	/// Increases the indent.
	pub fn increase_indent(&mut self) {
		self.size += 1;
	}

	/// Decreases the indent.
	pub fn decrease_indent(&mut self) {
		if self.size != 0 {
			self.size -= 1;
		}
	}

	/// Constructs an indent string.
	pub fn indent(&self) -> String {
		self.text.repeat(self.size as usize)
	}
}

impl Display for IndentContext {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		self.indent().fmt(f)
	}
}
