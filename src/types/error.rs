use std::path::PathBuf;

#[cfg(feature = "biblatex")]
pub use biblatex_module::*;

/// The main error that is handled in the `main()` function.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// Bibliography error.
    #[error("Bibliography error: {0}")]
    BibliographyError(#[from] BibliographyError),
    /// Invalid Bib(La)TeX input file format.
    #[cfg(feature = "biblatex")]
    #[error("Invalid format: expected Bib(La)TeX\nCaused by:\n\t{0:?}")]
    InvalidBiblatex(#[from] BibLaTeXErrors),
    /// Invalid YAML input file format.
    #[error("Invalid format: expected YAML\nCaused by:\n\t{0:?}")]
    InvalidYaml(#[from] serde_yaml::Error),
    /// Other error.
    #[error("{0}")]
    OtherError(String),
}

/// The error when reading bibliography file.
#[derive(thiserror::Error, Debug)]
pub enum BibliographyError {
    /// Bibliography file not found.
    #[error("Bibliography file {0:?} not found.")]
    NotFound(PathBuf),
    /// Error while reading the bibliography file (with OS error code).
    #[error("Error while reading the bibliography file {0:?}: {1}")]
    ReadErrorWithCode(PathBuf, i32),
    /// Error while reading the bibliography file.
    #[error("Error while reading the bibliography file {0:?}.")]
    ReadError(PathBuf),
}

impl From<&str> for Error {
    fn from(value: &str) -> Self {
        Self::OtherError(value.into())
    }
}

#[cfg(feature = "biblatex")]
mod biblatex_module {
    use std::{error::Error, fmt::Display};

    /// Errors that may occur when parsing a BibLaTeX file.
    #[derive(thiserror::Error, Clone, Debug)]
    pub enum BibLaTeXError {
        /// An error occurred when parsing a BibLaTeX file.
        Parse(biblatex::ParseError),
        /// One of the BibLaTeX fields was malformed for its type.
        Type(biblatex::TypeError),
    }

    impl Display for BibLaTeXError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Parse(err) => write!(f, "biblatex parse error: {}", err),
                Self::Type(err) => write!(f, "biblatex type error: {}", err),
            }
        }
    }

    /// Wrapper over an array of `BibLaTeXError` elements.
    #[derive(thiserror::Error, Clone, Debug)]
    pub struct BibLaTeXErrors(pub Vec<BibLaTeXError>);

    impl Display for BibLaTeXErrors {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let sources = self
                .0
                .clone()
                .into_iter()
                .filter_map(|x| x.source().map(|err| format!("{err:?}")))
                .collect::<Vec<String>>()
                .join("\n\t");
            write!(f, "{sources}")
        }
    }
}
