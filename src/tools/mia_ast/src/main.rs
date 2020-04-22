//! `mia-ast` is a simple debug executable which takes some Mia source code,
//! parses it, then prints the resultant AST to stdout or some specified file.

use mia_syntax as mia;
use structopt::StructOpt;
use std::process::exit;

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

	/// Source file to parse
	pub input_file: String,
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
		mia::parser::parse(&source)
	} else {
		mia::parser::parse_complete(&source)
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

	// Display AST.
	if opt.inline {
		println!("{:?}", ast);
	} else {
		println!("{:#?}", ast);
	}
}
