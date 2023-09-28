//! Reading and writing YAML bibliographies.

#[cfg(feature = "biblatex")]
use biblatex::{Bibliography, TypeError};

use crate::Entry;

/// Errors that may occur when parsing a BibLaTeX file.
#[cfg(feature = "biblatex")]
#[derive(Clone, Debug)]
pub enum BibLaTeXError {
    /// An error occurred when parsing a BibLaTeX file.
    Parse(biblatex::ParseError),
    /// One of the BibLaTeX fields was malformed for its type.
    Type(biblatex::TypeError),
}

#[cfg(feature = "biblatex")]
impl std::fmt::Display for BibLaTeXError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(err) => write!(f, "biblatex parse error: {}", err),
            Self::Type(err) => write!(f, "biblatex type error: {}", err),
        }
    }
}

/// Parse a bibliography from a BibLaTeX source string.
#[cfg(feature = "biblatex")]
pub fn from_biblatex_str(biblatex: &str) -> Result<Vec<Entry>, Vec<BibLaTeXError>> {
    let bibliography =
        Bibliography::parse(biblatex).map_err(|e| vec![BibLaTeXError::Parse(e)])?;

    from_biblatex(&bibliography)
        .map_err(|e| e.into_iter().map(BibLaTeXError::Type).collect())
}

/// Parse a bibliography from a BibLaTeX [`Bibliography`].
#[cfg(feature = "biblatex")]
pub fn from_biblatex(bibliography: &Bibliography) -> Result<Vec<Entry>, Vec<TypeError>> {
    use std::convert::TryInto;

    let res: Vec<Result<Entry, TypeError>> =
        bibliography.iter().map(TryInto::try_into).collect();
    let errors: Vec<TypeError> = res
        .iter()
        .filter_map(|item| match item {
            Ok(_) => None,
            Err(err) => Some(err.clone()),
        })
        .collect();

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(res.into_iter().map(|item| item.unwrap()).collect())
    }
}
