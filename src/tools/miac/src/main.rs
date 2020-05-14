//! The Mia Compiler!

use colored::*;
use std::path::{Path, PathBuf};
use std::process::exit;
use structopt::StructOpt;
use syntax::Parser;

/// The Mia Compiler
#[derive(Debug, StructOpt)]
#[structopt(name = "miac")]
struct Opt {
	/// Print generated AST to standard error
	#[structopt(long)]
	pub emit_ast: bool,

	/// Print generated IR code to standard error
	#[structopt(long)]
	pub emit_ir: bool,

	/// Print generated LLVM IR to standard error
	#[structopt(long)]
	pub emit_llvm: bool,

	/// Run a verification pass on the generated LLVM IR
	#[structopt(long)]
	pub verify_llvm: bool,

	/// Don't produce an output file.
	///
	/// Overrides `-o` if supplied.
	#[structopt(long)]
	pub no_output: bool,

	/// Name of the file to output compiled code to
	#[structopt(short)]
	pub output_file: Option<String>,

	/// The source file to compile
	pub input_file: String,
}

fn output_file(user_supplied: &Option<String>, source_filename: &str) -> String {
	user_supplied.clone().unwrap_or_else(|| {
		let mut buf = PathBuf::from(source_filename);
		buf.set_extension("ll");
		buf.to_str().unwrap().to_string()
	})
}

fn main() {
	let opt = Opt::from_args();

	let source = match std::fs::read_to_string(&opt.input_file) {
		Ok(s) => s,
		Err(e) => {
			println!(
				"{}: Unable to open {}: {}",
				"IO Error".red(),
				opt.input_file,
				e
			);
			exit(1);
		}
	};

	let source_filename = PathBuf::from(&opt.input_file)
		.file_name()
		.unwrap()
		.to_str()
		.unwrap()
		.to_string();

	println!("  {} {}", "Compiling".green().bold(), source_filename);
	println!("   {}", "Parsing".green().bold());
	let ast_program = match Parser::parse_source(&source) {
		Ok(program) => program,
		Err(err) => {
			println!("{}", error::render_error(&err, &source, &source_filename));
			exit(1);
		}
	};
	if opt.emit_ast {
		eprintln!("\n{:#?}\n", ast_program);
	}

	println!("   {}", "Lowering".green().bold());
	let ir_program = lower_ast::lower_ast(&ast_program);
	if opt.emit_ir {
		eprintln!("\n{}\n", ir_program);
	}

	let output_filename = output_file(&opt.output_file, &source_filename);

	println!("   {} {}", "Generating".green().bold(), output_filename);
	let context = ir::codegen::Context::create();
	let module = context.create_module(&source_filename);
	let codegen = ir::codegen::CodeGen::new(&context, module);
	codegen.compile_program(&ir_program);

	if opt.emit_llvm {
		eprintln!();
		codegen.module.print_to_stderr();
		eprintln!();
	}

	if opt.verify_llvm {
		match codegen.module.verify() {
			Ok(()) => {}
			Err(err) => {
				eprintln!(
					"  {}: {}",
					"LLVM Validation Error".red().bold(),
					err.to_string()
				);
				exit(1);
			}
		}
	}

	if !opt.no_output {
		codegen
			.module
			.write_bitcode_to_path(Path::new(&output_filename));
	}

	println!("  {}", "Done".green().bold());
}
