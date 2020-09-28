use std::path::{Path, PathBuf};

/// Main function of the Hayagriva CLI.
fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        if args.len() >= 1 {
            println!("Usage: {} bib.yml", args[0]);
        } else {
            println!("Usage: hayagriva bib.yml");
        }
        return;
    }

    let src_path = Path::new(&args[1]);
    println!("Hi!");
}
