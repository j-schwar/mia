extern crate mia_syntax;

mod common;

type TestResult = Result<(), Box<dyn std::error::Error>>;

#[test]
fn test_successful_parse() -> TestResult {
	for path in common::sample_files()? {
		let contents = std::fs::read_to_string(&path)?;
		let result = mia_syntax::parser::parse_complete(&contents);
		assert!(
			result.is_ok(),
			"Failed to parse {}: {:?}",
			path.file_name().unwrap().to_str().unwrap(),
			result
		);
	}
	Ok(())
}

#[test]
fn test_idempotent_remove_group_pass() -> TestResult {
	for path in common::sample_files()? {
		let contents = std::fs::read_to_string(&path)?;
		let (_, ast) = mia_syntax::parser::parse_complete(&contents).unwrap();

		let first_pass = mia_syntax::ast::pass::remove_groups(ast);
		let second_pass = mia_syntax::ast::pass::remove_groups(first_pass.clone());
		assert_eq!(first_pass, second_pass);
	}
	Ok(())
}
