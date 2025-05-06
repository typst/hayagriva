use std::path::PathBuf;

#[cfg(feature = "biblatex")]
pub use biblatex_module::*;

/// Used once before all but the 1st errors.
const TOP_INDENT: &str = "\nCaused by:";

/// Used to indent next error.
const INDENT: &str = "    ";

/// The main error that is handled in the `main()` function.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// Bibliography error.
    #[error("Bibliography error: {0}")]
    BibliographyError(#[from] BibliographyError),
    /// Invalid Bib(La)TeX input file format.
    #[cfg(feature = "biblatex")]
    #[error("Invalid format: expected Bib(La)TeX\n{TOP_INDENT}\n{INDENT}{0}")]
    InvalidBiblatex(#[from] BibLaTeXErrors),
    /// Invalid YAML input file format.
    #[error("Invalid format: expected YAML\n{TOP_INDENT}\n{INDENT}{0}")]
    InvalidYaml(#[from] serde_yaml::Error),
    /// Other error.
    #[error("{0}")]
    OtherError(String),
}

/// The error when reading bibliography file.
#[derive(thiserror::Error, Debug)]
pub enum BibliographyError {
    /// Bibliography file not found.
    #[error(r#"Bibliography file "{0}" not found."#)]
    NotFound(PathBuf),
    /// Error while reading the bibliography file (with OS error code).
    #[error(r#"Error while reading the bibliography file "{0}": {1}"#)]
    ReadErrorWithCode(PathBuf, i32),
    /// Error while reading the bibliography file.
    #[error(r#"Error while reading the bibliography file "{0}"."#)]
    ReadError(PathBuf),
}

impl From<&str> for Error {
    fn from(value: &str) -> Self {
        Self::OtherError(value.into())
    }
}

#[cfg(feature = "biblatex")]
mod biblatex_module {
    use super::INDENT;
    use std::fmt::{Display, Formatter, Result};

    /// Errors that may occur when parsing a BibLaTeX file.
    #[derive(thiserror::Error, Clone, Debug)]
    pub enum BibLaTeXError {
        /// An error occurred when parsing a BibLaTeX file.
        #[error("BibLaTeX parse error\n{INDENT}{0}")]
        Parse(biblatex::ParseError),
        /// One of the BibLaTeX fields was malformed for its type.
        #[error("BibLaTeX type error\n{INDENT}{0}")]
        Type(biblatex::TypeError),
    }

    /// Wrapper over an array of `BibLaTeXError` elements.
    #[derive(thiserror::Error, Clone, Debug)]
    pub struct BibLaTeXErrors(pub Vec<BibLaTeXError>);

    impl Display for BibLaTeXErrors {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            let sources = self
                .0
                .clone()
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n\t");
            write!(f, "{sources}")
        }
    }
}
