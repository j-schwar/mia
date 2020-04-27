//! Common error types for IR passes/compilers.

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// Type returned by IR compilers/passes which could result in an error.
pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// Errors revolving around the names of variables/functions.
#[derive(Debug)]
pub enum NamingError {
	RedefinitionOfFunction(String),
	RedefinitionOfVariable(String),
	UseOfKeywordAsName(String),
	UseOfUndefinedFunction(String),
	UseOfUndefinedVariable(String),
	UseOfUndefinedType(String),
}

impl Display for NamingError {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		use NamingError::*;
		match self {
			RedefinitionOfFunction(s) => write!(f, "redefinition of function: '{}'", s),
			RedefinitionOfVariable(s) => write!(f, "redefinition of variable: '{}'", s),
			UseOfKeywordAsName(s) => write!(f, "'{}' is a keyword and cannot be used as a name", s),
			UseOfUndefinedFunction(s) => write!(f, "use of undefined function: '{}'", s),
			UseOfUndefinedVariable(s) => write!(f, "use of undefined variable: '{}'", s),
			UseOfUndefinedType(s) => write!(f, "use of undefined type: '{}'", s),
		}
	}
}

impl Error for NamingError {}
