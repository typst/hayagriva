/*!
Hayagriva provides a YAML-backed format and data model for various
bibliography items as well as formatting for both in-text citations and
reference lists based on these literature databases.

The crate is intended to assist scholarly writing and reference management
and can be used both through a CLI and an API.

Below, there is an example of how to parse a YAML database and get a Modern
Language Association-style citation.

# Supported styles

- Institute of Electrical and Electronics Engineers (IEEE)
    - [References](style::Ieee)
    - [Numerical citations](style::Numerical)
- Modern Language Association (MLA), 8th edition of the MLA Handbook
    - ["Works Cited" references](style::Mla)
- Chicago Manual of Style (CMoS), 17th edition
    - [Notes and Bibliography](style::ChicagoNotes)
    - [Author-Date references and citations](style::ChicagoAuthorDate)
- American Psychological Association (APA), 7th edition of the APA Publication Manual
    - [References](style::Apa)
- Other in-text citation styles
    - [Alphanumerical](style::Alphanumerical) (e. g. "Rass97")
    - [Author Title](style::AuthorTitle)

# Usage

```rust
use hayagriva::from_yaml_str;
use hayagriva::style::{Database, Mla};

let yaml = r#"
crazy-rich:
    type: Book
    title: Crazy Rich Asians
    author: Kwan, Kevin
    date: 2014
    publisher: Anchor Books
    location: New York, NY, US
"#;

// Parse a bibliography
let bib = from_yaml_str(yaml).unwrap();
assert_eq!(bib[0].date.unwrap().year, 2014);

// Format the reference
let db = Database::from_entries(bib.iter());
let mut mla = Mla::new();
let reference = db.bibliography(&mut mla, None);
assert_eq!(reference[0].display.value, "Kwan, Kevin. Crazy Rich Asians. Anchor Books, 2014.");
```

Formatting for in-text citations is available through implementors of the
[`style::CitationStyle`] trait whereas bibliographies can be created by
[`style::BibliographyStyle`]. Both traits are used through a
[`style::Database`] which provides methods to format its records as
bibliographies and citations using references to implementors to these
traits.

If the default features are enabled, Hayagriva supports BibTeX and BibLaTeX
bibliographies. You can use [`io::from_biblatex_str`] to parse such
bibliographies.

Should you need more manual control, the library's native `Entry` struct
also offers an implementation of the `From<&biblatex::Entry>`-Trait. You will
need to depend on the [biblatex](https://docs.rs/biblatex/latest/biblatex/)
crate to obtain its `Entry`. Therefore, you could also use your BibLaTeX
content like this:

```ignore
use hayagriva::Entry;
let converted: Entry = your_biblatex_entry.into();
```

If you do not need BibLaTeX compatibility, you can use Hayagriva without the
default features by writing this in your `Cargo.toml`:

```toml
[dependencies]
hayagriva = { version = "0.2", default-features = false }
```

# Selectors

Hayagriva uses a custom selector language that enables you to filter
bibliographies by type of media. For more information about selectors, refer
to the [selectors.md
file](https://github.com/typst/hayagriva/blob/main/docs/selectors.md). While
you can parse user-defined selectors using the function `Selector::parse`,
you may instead want to use the selector macro to avoid the run time cost of
parsing a selector when working with constant selectors.

```rust
use hayagriva::select;
use hayagriva::from_yaml_str;

let yaml = r#"
quantized-vortex:
    type: Article
    author: Gross, E. P.
    title: Structure of a Quantized Vortex in Boson Systems
    date: 1961-05
    page-range: 454-477
    doi: 10.1007/BF02731494
    parent:
        issue: 3
        volume: 20
        title: Il Nuovo Cimento
"#;

let entries = from_yaml_str(yaml).unwrap();
let journal = select!((Article["date"]) > ("journal":Periodical));
assert!(journal.matches(&entries[0]));
```

There are two ways to check if a selector matches an entry.
You should use [`Selector::matches`] if you just want to know if an item
matches a selector and [`Selector::apply`] to continue to work with the data from
parents of a matching entry. Keep in mind that the latter function will
return `Some` even if no sub-entry was bound / if the hash map is empty.
*/

#![warn(missing_docs)]
#![allow(clippy::comparison_chain)]

#[macro_use]
mod selectors;
#[cfg(feature = "biblatex")]
mod interop;

pub mod io;
pub mod lang;
pub mod style;
pub mod types;

pub use selectors::{Selector, SelectorError};

use thiserror::Error;
use types::*;
use unic_langid::LanguageIdentifier;
use yaml_rust::{Yaml, YamlLoader};

macro_rules! entry {
    ($($(#[$docs:meta])+ $s:literal => $i:ident : $t:ty $(,)?),*) => {
        /// A citable item in a bibliography.
        #[derive(Debug, Clone, PartialEq)]
        pub struct Entry {
            /// The key of the entry.
            pub key: String,
            /// The type of the item.
            pub entry_type: EntryType,
            /// Item in which the item was published / to which it is strongly
            /// associated to.
            pub parents: Vec<Entry>,
            $(
                $(#[$docs])+
                pub $i: Option<$t>,
            )*
        }

        impl Entry {
            /// Construct a new, empty entry.
            pub fn new(key: &str, entry_type: EntryType) -> Self {
                Self {
                    key: key.to_string(),
                    entry_type,
                    $(
                        $i: None,
                    )*
                    parents: Vec::new(),
                }
            }

            /// Check whether the entry has some key.
            pub fn has(&self, key: &str) -> bool {
                match key {
                    $(
                        $s => self.$i.is_some(),
                    )*
                    _ => false,
                }
            }
        }

        impl HayagrivaValue for Entry {
            fn from_yaml(
                yaml: &yaml_rust::Yaml,
                ctx: &mut ParseContext<'_>,
            ) -> Result<Self, DeserializationError>
            where
                Self: Sized,
            {
                let Yaml::Hash(h) = yaml else {
                    return Err(Self::expected_error());
                };
                let entry_type = match h.get_with_str("type", ctx) {
                    Ok(y) => {
                        let res = EntryType::from_yaml(y, ctx);
                        ctx.pop_dict_key();
                        res
                    }
                    Err(e) => ctx.default_type().ok_or(e),
                }?;
                ctx.path.push(entry_type);

                let parents = match h.get_with_str("parent", ctx) {
                    Ok(y) => {
                        let res = Vec::<Entry>::from_yaml(y, ctx)?;
                        ctx.pop_dict_key();
                        res
                    }
                    Err(_) => Vec::new(),
                };

                let res = Ok(Self {
                    key: ctx.key.to_owned(),
                    entry_type,
                    $(
                        $i: match h.get_with_str($s, ctx) {
                            Ok(y) => {
                                let res = <$t>::from_yaml(y, ctx)?;
                                ctx.pop_dict_key();
                                Some(res)
                            }
                            Err(_) => None,
                        },
                    )*
                    parents,
                });

                ctx.path.pop();
                res
            }

            fn to_yaml(&self) -> yaml_rust::Yaml {
                let mut h = yaml_rust::yaml::Hash::new();
                h.insert_with_str("type", self.entry_type.to_yaml());
                if !self.parents.is_empty() {
                    h.insert_with_str("parent", self.parents.to_yaml());
                }

                $(
                    if let Some($i) = &self.$i {
                        h.insert_with_str($s, $i.to_yaml());
                    }
                )*

                Yaml::Hash(h)
            }

            fn explain() -> &'static str {
                "a dictionary with a `type` key"
            }
        }
    };
}

entry! {
    /// Title of the item.
    "title" => title: FormatStr,
    /// Persons primarily responsible for creating the item.
    "author" => authors: Vec<Person>,
    /// Date at which the item was published.
    "date" => date: Date,
    /// Persons responsible for selecting and revising the content of the item.
    "editor" => editors: Vec<Person>,
    /// Persons involved in the production of the item that are not authors or editors.
    "affiliated" => affiliated: Vec<PersonsWithRoles>,
    /// Publisher of the item.
    "publisher" => publisher: FormatStr,
    /// Physical location at which the item was published or created.
    "location" => location: FormatStr,
    /// Organization at/for which the item was created.
    "organization" => organization: FormatStr,
    /// For an item whose parent has multiple issues, indicates the position in
    /// the issue sequence. Also used to indicate the episode number for TV.
    "issue" => issue: MaybeTyped<Numeric>,
    /// For an item whose parent has multiple volumes/parts/seasons ... of which
    /// this item is one.
    "volume" => volume: MaybeTyped<Numeric>,
    /// Total number of volumes/parts/seasons ... this item consists of.
    "volume-total" => volume_total: Numeric,
    /// Published version of an item.
    "edition" => edition: MaybeTyped<Numeric>,
    /// The range of pages within the parent this item occupies
    "page-range" => page_range: Numeric,
    /// The total number of pages the item has.
    "page-total" => page_total: Numeric,
    /// The time range within the parent this item starts and ends at.
    "time-range" => time_range: MaybeTyped<DurationRange>,
    /// The total runtime of the item.
    "runtime" => runtime: MaybeTyped<Duration>,
    /// Canonical public URL of the item, can have access date.
    "url" => url: QualifiedUrl,
    /// The Digital Object Identifier of the item.
    "doi" => doi: String,
    /// Any serial number or version describing the item that is not appropriate
    /// for the fields doi, edition, isbn or issn (may be assigned by the author
    /// of the item; especially useful for preprint archives).
    "serial-number" => serial_number: String,
    /// International Standard Book Number (ISBN), prefer ISBN-13.
    "isbn" => isbn: String,
    /// International Standard Serial Number (ISSN).
    "issn" => issn: String,
    /// The language of the item.
    "language" => language: LanguageIdentifier,
    /// Name of the institution/collection where the item is kept.
    "archive" => archive: FormatStr,
    /// Physical location of the institution/collection where the item is kept.
    "archive-location" => archive_location: FormatStr,
    /// The call number of the item in the institution/collection.
    "call-number" => call_number: FormatStr,
    /// Additional description to be appended in the bibliographic entry.
    "note" => note: FormatStr,
}

impl Entry {
    /// Get and parse the `affiliated` field and only return persons of a given
    /// [role](PersonRole).
    pub(crate) fn affiliated_with_role(&self, role: PersonRole) -> Vec<Person> {
        self.affiliated
            .iter()
            .flatten()
            .cloned()
            .filter_map(|(persons, r)| if r == role { Some(persons) } else { None })
            .flatten()
            .collect()
    }

    /// Get the key of the entry.
    pub fn key(&self) -> &str {
        &self.key
    }

    /// Get the unconverted value of a certain field from this entry or any of
    /// its parents.
    pub fn map<'a, F, T>(&'a self, mut f: F) -> Option<T>
    where
        F: FnMut(&'a Self) -> Option<T>,
    {
        if let Some(value) = f(self) {
            Some(value)
        } else {
            self.map_parents(f)
        }
    }

    /// Get the unconverted value of a certain field from the parents only by BFS.
    pub fn map_parents<'a, F, T>(&'a self, mut f: F) -> Option<T>
    where
        F: FnMut(&'a Self) -> Option<T>,
    {
        let mut path: Vec<usize> = vec![0];
        let up = |path: &mut Vec<usize>| {
            path.pop();
            if let Some(last) = path.last_mut() {
                *last += 1;
            }
        };

        'outer: loop {
            // Index parents with the items in path. If, at any level, the index
            // exceeds the number of parents, increment the index at the
            // previous level. If no other level remains, return.
            let Some(first_path) = path.first() else {
                return None;
            };

            if self.parents.len() <= *first_path {
                return None;
            }

            let mut item = &self.parents[*first_path];

            for i in 1..path.len() {
                if path[i] >= item.parents.len() {
                    up(&mut path);
                    continue 'outer;
                }
                item = &item.parents[path[i]];
            }

            if let Some(first_path) = path.first_mut() {
                *first_path += 1;
            }

            if let Some(value) = f(item) {
                return Some(value);
            }
        }
    }

    /// Will recursively get a date off either the entry or any of its ancestors.
    pub fn date_any(&self) -> Option<&Date> {
        self.map(|e| e.date.as_ref())
    }

    /// Will recursively get an URL off either the entry or any of its ancestors.
    pub fn url_any(&self) -> Option<&QualifiedUrl> {
        self.map(|e| e.url.as_ref())
    }

    /// Extract the twitter handle for the nth author from their alias.
    /// Will make sure the handle starts with `@`.
    ///
    /// If the `user_index` is 0, the function will try to extract
    /// the handle from the URL.
    pub(crate) fn twitter_handle(&self, user_index: usize) -> Option<String> {
        if self.entry_type != EntryType::Tweet {
            return None;
        }

        let authors = self.authors.as_deref().unwrap_or_default();

        if user_index > 0 && user_index >= authors.len() {
            return None;
        }

        if let Some(alias) = &authors[user_index].alias {
            return if alias.starts_with('@') {
                Some(alias.clone())
            } else {
                Some(format!("@{}", alias))
            };
        }

        if user_index == 0 {
            if let Some(url) = self.url.as_ref().map(|u| &u.value) {
                if !matches!(url.host(), Some(url::Host::Domain("twitter.com" | "x.com")))
                {
                    return None;
                }

                if let Some(handle) = url.path_segments().and_then(|mut c| c.next()) {
                    return Some(format!("@{}", handle));
                }
            }
        }

        None
    }
}

#[cfg(feature = "biblatex")]
impl Entry {
    /// Adds a parent to the current entry. The parent
    /// list will be created if there is none.
    pub(crate) fn add_parent(&mut self, entry: Entry) {
        self.parents.push(entry);
    }

    /// Adds affiliated persons. The list will be created if there is none.
    pub(crate) fn add_affiliated_persons(
        &mut self,
        new_persons: (Vec<Person>, PersonRole),
    ) {
        if let Some(affiliated) = &mut self.affiliated {
            affiliated.push(new_persons);
        } else {
            self.affiliated = Some(vec![new_persons]);
        }
    }

    pub(crate) fn parents_mut(&mut self) -> &mut [Entry] {
        &mut self.parents
    }
}

/// Errors that may occur while parsing a library.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LibraryError {
    /// The YAML is malformed.
    #[error("malformed YAML: {0}")]
    YamlError(#[from] yaml_rust::ScanError),
    /// The YAML string does not contain exactly one document.
    #[error("expected exactly one document")]
    WrongNumberOfDocuments,
    /// The library is not a dictionary.
    #[error("no top-level dictionary found")]
    NoTopLevelHash,
    /// The key for an entry is not a string.
    #[error("expected a string key")]
    NonStringKey,
    /// There was an error while parsing a value.
    #[error("error while deserializing: {0}")]
    DeserializationError(#[from] DeserializationError),
}

/// Parse a bibliography from a YAML string.
///
/// ```
/// use hayagriva::from_yaml_str;
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
/// assert_eq!(bib[0].date.unwrap().year, 2014);
/// ```
pub fn from_yaml_str(s: &str) -> Result<Vec<Entry>, LibraryError> {
    let yaml = YamlLoader::load_from_str(s)?;
    if yaml.len() != 1 {
        return Err(LibraryError::WrongNumberOfDocuments);
    }

    let yaml = &yaml[0];
    from_yaml(yaml)
}

/// Parse a bibliography from YAML.
pub fn from_yaml(yaml: &Yaml) -> Result<Vec<Entry>, LibraryError> {
    let Yaml::Hash(h) = yaml else {
        return Err(LibraryError::NoTopLevelHash);
    };

    let mut res = Vec::with_capacity(h.len());

    for (key, value) in h.into_iter() {
        let Yaml::String(key) = key else {
            return Err(LibraryError::NonStringKey);
        };

        let mut ctx = ParseContext::new(key);
        let entry = Entry::from_yaml(value, &mut ctx)?;
        res.push(entry);
    }

    Ok(res)
}

/// Serialize a bibliography to YAML.
pub fn to_yaml(entries: &[Entry]) -> Yaml {
    let mut h = yaml_rust::yaml::Hash::new();

    for entry in entries {
        h.insert_with_str(&entry.key, entry.to_yaml());
    }

    Yaml::Hash(h)
}

/// Serialize a bibliography to a YAML string.
pub fn to_yaml_str(entries: &[Entry]) -> Result<String, yaml_rust::EmitError> {
    let yaml = to_yaml(entries);
    let mut buf = String::new();
    yaml_rust::YamlEmitter::new(&mut buf).dump(&yaml)?;
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use style::Citation;

    use super::*;
    use crate::style::{Apa, ChicagoNotes, Database, Ieee, Mla};

    #[test]
    fn apa() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let apa = Apa::new();

        let mut db = Database::new();
        for entry in &entries {
            db.push(entry);
        }

        for reference in db.bibliography(&apa, None) {
            println!("{:#}", reference.display);
        }
    }

    #[test]
    fn ieee() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let ieee = Ieee::new();

        let mut db = Database::new();
        for entry in &entries {
            db.push(entry);
        }

        for reference in db.bibliography(&ieee, None) {
            println!("{:#}", reference.display);
        }
    }

    #[test]
    fn mla() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let mla = Mla::new();

        let mut db = Database::new();
        for entry in &entries {
            db.push(entry);
        }

        for reference in db.bibliography(&mla, None) {
            println!("{:#}", reference.display);
        }
    }

    #[test]
    fn chicago_n() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let mut chicago = ChicagoNotes::default();

        let mut db = Database::new();
        for entry in &entries {
            db.push(entry);
        }

        for entry in &entries {
            let citation = Citation::new(entry, None);
            println!("{:#}", db.citation(&mut chicago, &[citation]).display);
        }
    }

    #[test]
    fn chicago_b() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let chicago = ChicagoNotes::default();

        let mut db = Database::new();
        for entry in &entries {
            db.push(entry);
        }

        for reference in db.bibliography(&chicago, None) {
            println!("{:#}", reference.display);
        }
    }

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = [$($key,)*];
            let selector = Selector::parse($select).unwrap();
            for entry in &$entries {
                let res = selector.apply(entry);
                if keys.contains(&entry.key.as_str()) {
                    if res.is_none() {
                        panic!("Key {} not found in results", entry.key);
                    }
                } else {
                    if res.is_some() {
                        panic!("Key {} found in results", entry.key);
                    }
                }
            }
        }
    }

    macro_rules! select {
        ($select:expr, $entries:tt >> $entry_key:expr, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
            let entry = $entries.iter().filter_map(|i| if i.key == $entry_key {Some(i)} else {None}).next().unwrap();
            let selector = Selector::parse($select).unwrap();
            let res = selector.apply(entry).unwrap();
            if !keys.into_iter().all(|k| res.get(k).is_some()) {
                panic!("Results do not contain binding");
            }
        }
    }

    #[test]
    fn selectors() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();

        select_all!("article > proceedings", entries, ["zygos"]);
        select_all!(
            "article > (periodical | newspaper)",
            entries,
            ["omarova-libra", "kinetics", "house", "swedish",]
        );
        select_all!(
            "(chapter | anthos) > (anthology | book)",
            entries,
            ["harry", "gedanken"]
        );
        select_all!(
            "*[url]",
            entries,
            [
                "omarova-libra",
                "science-e-issue",
                "oiseau",
                "georgia",
                "really-habitable",
                "electronic-music",
                "mattermost",
                "worth",
                "wrong",
                "un-hdr",
                "audio-descriptions",
                "camb",
                "logician",
                "dns-encryption",
                "overleaf",
                "editors",
            ]
        );
        select_all!(
            "!(*[url] | (* > *[url]))",
            entries,
            [
                "zygos",
                "harry",
                "terminator-2",
                "interior",
                "wire",
                "kinetics",
                "house",
                "plaque",
                "renaissance",
                "gedanken",
                "donne",
                "roe-wade",
                "foia",
                "drill",
                "swedish",
                "latex-users",
                "barb",
            ]
        );
    }

    #[test]
    fn selector_bindings() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();

        select!(
            "a:article > (b:conference & c:(video|blog|web))",
            entries >> "wwdc-network",
            ["a", "b", "c"]
        );
    }

    #[test]
    fn roundtrip() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let yaml = to_yaml_str(&entries).unwrap();
        let reconstructed = from_yaml_str(&yaml).unwrap();
        assert_eq!(entries.len(), reconstructed.len());

        for entry in entries {
            let match_e = reconstructed.iter().find(|x| x.key == entry.key).unwrap();
            assert_eq!(match_e, &entry);
        }
    }
}
