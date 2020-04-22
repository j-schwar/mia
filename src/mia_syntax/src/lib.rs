//! This crate contains the frontend for the Mia compiler. This includes the
//! lexer, parser, as well as the AST and various transformation passes of
//! it.
//!
//! We separate the syntax processing from the codegen in order to reuse it
//! in various tooling applications like formatters for example.

pub mod ast;
pub mod parser;
