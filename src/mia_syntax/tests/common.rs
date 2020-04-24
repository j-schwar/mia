use std::error::Error;
use std::path::PathBuf;

/// Retrieves a list of paths to sample files.
pub fn sample_files() -> Result<Vec<PathBuf>, Box<dyn Error>> {
	let mut samples_dir = std::env::current_dir()?;
	samples_dir.push("tests");
	samples_dir.push("samples");
	std::fs::read_dir(samples_dir)?
		.map(|entry| entry.map(|e| e.path()))
		.collect::<Result<Vec<PathBuf>, std::io::Error>>()
		.map_err(|e| e.into())
}
