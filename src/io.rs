//! Reading and writing YAML bibliographies.

#[cfg(feature = "biblatex")]
use biblatex::{Bibliography, TypeError};

#[cfg(feature = "biblatex")]
use crate::Entry;
use crate::Library;

/// Parse a bibliography from a YAML string.
///
/// ```
/// use hayagriva::io::from_yaml_str;
///
/// let yaml = r#"
/// crazy-rich:
///     type: Book
///     title: Crazy Rich Asians
///     author: Kwan, Kevin
///     date: 2014
///     publisher: Anchor Books
///     location: New York, NY, US
/// "#;
/// let bib = from_yaml_str(yaml).unwrap();
/// assert_eq!(bib.nth(0).unwrap().date().unwrap().year, 2014);
/// ```
pub fn from_yaml_str(s: &str) -> Result<Library, serde_yaml::Error> {
    serde_yaml::from_str(s)
}

/// Serialize a bibliography to a YAML string.
pub fn to_yaml_str(entries: &Library) -> Result<String, serde_yaml::Error> {
    serde_yaml::to_string(&entries)
}

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
            Self::Parse(err) => write!(f, "biblatex parse error: {err}"),
            Self::Type(err) => write!(f, "biblatex type error: {err}"),
        }
    }
}

/// Parse a bibliography from a BibLaTeX source string.
#[cfg(feature = "biblatex")]
pub fn from_biblatex_str(biblatex: &str) -> Result<Library, Vec<BibLaTeXError>> {
    let bibliography =
        Bibliography::parse(biblatex).map_err(|e| vec![BibLaTeXError::Parse(e)])?;

    from_biblatex(&bibliography)
        .map_err(|e| e.into_iter().map(BibLaTeXError::Type).collect())
}

/// Parse a bibliography from a BibLaTeX [`Bibliography`].
#[cfg(feature = "biblatex")]
pub fn from_biblatex(bibliography: &Bibliography) -> Result<Library, Vec<TypeError>> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn roundtrip() {
        let contents = fs::read_to_string("tests/data/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let yaml = to_yaml_str(&entries).unwrap();
        println!("{}", &yaml);

        let reconstructed = from_yaml_str(&yaml).unwrap();
        assert_eq!(entries.len(), reconstructed.len());

        for entry in entries {
            let match_e = reconstructed.iter().find(|x| x.key == entry.key).unwrap();
            assert_eq!(match_e, &entry);
        }
    }
}
