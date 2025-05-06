//! Reading and writing YAML bibliographies.

#[cfg(feature = "biblatex")]
use biblatex::{Bibliography, TypeError};

#[cfg(feature = "biblatex")]
use crate::Entry;
use crate::Library;

use crate::error::Error;

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
pub fn from_yaml_str(s: &str) -> Result<Library, Error> {
    serde_yaml::from_str(s).map_err(Error::from)
}

/// Serialize a bibliography to a YAML string.
pub fn to_yaml_str(entries: &Library) -> Result<String, Error> {
    serde_yaml::to_string(&entries).map_err(Error::from)
}

/// Parse a bibliography from a BibLaTeX source string.
#[cfg(feature = "biblatex")]
pub fn from_biblatex_str(biblatex: &str) -> Result<Library, Error> {
    use crate::error::{BibLaTeXError, BibLaTeXErrors};

    let bibliography = Bibliography::parse(biblatex)
        .map_err(BibLaTeXError::Parse)
        .map_err(|e| BibLaTeXErrors(vec![e]))
        .map_err(Error::from)?;

    from_biblatex(&bibliography)
        .map_err(|e| e.into_iter().map(BibLaTeXError::Type).collect::<Vec<_>>())
        .map_err(BibLaTeXErrors)
        .map_err(Error::from)
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
