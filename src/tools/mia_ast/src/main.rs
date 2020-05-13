//! `mia-ast` is a simple debug executable which takes some Mia source code,
//! parses it, then prints the resultant AST to stdout or some specified file.

use std::process::exit;
use structopt::StructOpt;
use syntax::*;

/// A debug program which displays the Abstract Syntax Tree of Mia source code.
#[derive(Debug, StructOpt)]
#[structopt(name = "mia-ast")]
struct Opt {
	/// Displays the AST as a single line
	#[structopt(long)]
	pub inline: bool,

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
	let program = match Parser::parse_source(&source) {
		Ok(program) => program,
		Err(err) => {
			println!("{:?}", err);
			exit(1);
		}
	};

	// Display AST.
	if opt.inline {
		println!("{:?}", program);
	} else {
		println!("{:#?}", program);
	}
}
