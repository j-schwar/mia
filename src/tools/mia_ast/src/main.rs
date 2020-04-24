//! `mia-ast` is a simple debug executable which takes some Mia source code,
//! parses it, then prints the resultant AST to stdout or some specified file.

use mia_syntax::ast::pass;
use mia_syntax::ast::TranslationUnit;
use std::process::exit;
use std::str::FromStr;
use structopt::StructOpt;

/// A debug program which displays the Abstract Syntax Tree of Mia source code.
#[derive(Debug, StructOpt)]
#[structopt(name = "mia-ast")]
struct Opt {
	/// Displays the AST as a single line
	#[structopt(long)]
	pub inline: bool,

	/// Allow an incomplete parse
	#[structopt(long)]
	pub incomplete: bool,

	/// AST pass to apply
	///
	/// Only one pass per -P flag may be applied, if none are supplied then
	/// no passes will be applied to the AST
	///
	/// PASS:
	///   all                 - Run all passes
	///   remove-groups       - Removes Group nodes
	#[structopt(short = "P", name = "PASS", number_of_values = 1, verbatim_doc_comment)]
	pub passes: Option<Vec<Pass>>,

	/// Source file to parse
	pub input_file: String,
}

#[derive(Debug, Eq, PartialEq)]
enum Pass {
	All,
	RemoveGroups,
}

impl FromStr for Pass {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"all" => Ok(Pass::All),
			"remove-groups" => Ok(Pass::RemoveGroups),
			_ => Err(format!("'{}'\nuse mia_ast --help for a list of passes", s)),
		}
	}
}

/// Invoke AST passes.
///
/// The order in which passes are performed in is static and does not relate to
/// the order in which program arguments are supplied. Ideally passes should be
/// invoked in the same order as the compiler.
fn run_passes(opt: &Opt, ast: TranslationUnit) -> TranslationUnit {
	if opt.passes.is_none() {
		return ast;
	}

	let passes = opt.passes.as_ref().unwrap();
	let do_pass = |pass: Pass| passes.contains(&Pass::All) || passes.contains(&pass);

	let ast = if do_pass(Pass::RemoveGroups) {
		pass::remove_groups(ast)
	} else {
		ast
	};

	return ast;
}

fn main() {
	let opt = Opt::from_args();

	// Read the source file.
	let source = match std::fs::read_to_string(&opt.input_file) {
		Ok(s) => s,
		Err(e) => {
			println!("unable to read {}: {}", opt.input_file, e);
			exit(1);
		}
	};

	// Parse and get AST.
	let result = if opt.incomplete {
		mia_syntax::parser::parse(&source)
	} else {
		mia_syntax::parser::parse_complete(&source)
	};

	// Unwrap result.
	//
	// TODO: When parsing errors are implemented, show them here.
	let (_, ast) = match result {
		Ok(tuple) => tuple,
		Err(e) => {
			println!("a parsing error occurred: {}", e);
			exit(1);
		}
	};

	// Invoke requested AST passes.
	let ast = run_passes(&opt, ast);

	// Display AST.
	if opt.inline {
		println!("{:?}", ast);
	} else {
		println!("{:#?}", ast);
	}
}
