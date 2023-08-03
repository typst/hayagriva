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
use hayagriva::io::from_yaml_str;
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
assert_eq!(bib[0].date().unwrap().year, 2014);

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
use hayagriva::io::from_yaml_str;

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

#[macro_use]
mod selectors;
#[cfg(feature = "biblatex")]
mod interop;

pub mod io;
pub mod lang;
pub mod style;
pub mod types;

pub use selectors::{Selector, SelectorError};

use std::collections::HashMap;

use paste::paste;
use std::convert::TryFrom;
use strum::Display;
use thiserror::Error;
use unic_langid::LanguageIdentifier;

use types::{
    Date, Duration, EntryType, FmtString, NumOrStr, Person, PersonRole, QualifiedUrl,
    Title,
};

/// The data types that can possibly be held by the various fields of an
/// [`Entry`].
#[derive(Clone, Debug, Display, PartialEq)]
#[non_exhaustive]
#[strum(serialize_all = "lowercase")]
pub enum Value {
    /// A [Title] containing a canonical value and optionally translations and
    /// shorthands, all of which are formattable.
    Title(Title),
    /// A [FmtString] with which the user can override various
    /// automatic formatters.
    FmtString(FmtString),
    /// A string to be reproduced as-is.
    Text(String),
    /// An integer.
    Integer(i64),
    /// A date, possibly only a year.
    Date(Date),
    /// A number of [Person]s.
    Persons(Vec<Person>),
    /// A list of [roles](PersonRole) with their assigned [Person]s.
    PersonsWithRoles(Vec<(Vec<Person>, PersonRole)>),
    /// This could be both an Integer or a Number.
    IntegerOrText(NumOrStr),
    /// A range between two integers.
    Range(std::ops::Range<i64>),
    /// A duration (of a song or an performance for example).
    Duration(Duration),
    /// A part of a period.
    TimeRange(std::ops::Range<Duration>),
    /// An [URL, possibly with a date of when it has been consulted](QualifiedUrl).
    Url(QualifiedUrl),
    /// A [Unicode Language Identifier](LanguageIdentifier).
    Language(LanguageIdentifier),
    /// Other [entries](Entry).
    Entries(Vec<Entry>),
}

/// A citable item in a bibliography.
#[derive(Clone, Debug, PartialEq)]
pub struct Entry {
    /// A string by which the Entry can be identified.
    key: String,
    /// The kind of media this Entry represents.
    entry_type: EntryType,
    /// Information about the Entry.
    pub(crate) content: HashMap<String, Value>,
}

impl Entry {
    /// Construct a new, empty entry.
    pub fn new(key: &str, entry_type: EntryType) -> Self {
        Self {
            key: key.to_string(),
            entry_type,
            content: HashMap::new(),
        }
    }

    /// Get the entry's database key.
    pub fn key(&self) -> &str {
        self.key.as_ref()
    }

    /// Get the entry's [`EntryType`].
    pub fn kind(&self) -> EntryType {
        self.entry_type
    }

    /// Get the unconverted value of a certain field.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.content.get(key)
    }

    /// Set [any data type](Value) as value for a given field.
    pub fn set(
        &mut self,
        field: impl Into<String>,
        value: Value,
    ) -> Result<(), SetFieldError> {
        let field = field.into();
        let valid = match field.as_ref() {
            "parent" => matches!(value, Value::Entries(_)),
            "title" => matches!(value, Value::Title(_)),
            "location" | "publisher" | "archive" | "archive-location" => {
                matches!(value, Value::FmtString(_))
            }
            "author" | "editor" => matches!(value, Value::Persons(_)),
            "date" => matches!(value, Value::Date(_)),
            "affiliated" => matches!(value, Value::PersonsWithRoles(_)),
            "organization" | "issn" | "isbn" | "doi" | "serial-number" | "note" => {
                matches!(value, Value::Text(_))
            }
            "issue" | "edition" => matches!(value, Value::IntegerOrText(_)),
            "volume" | "page-range" => matches!(value, Value::Range(_)),
            "volume-total" | "page-total" => matches!(value, Value::Integer(_)),
            "time-range" => matches!(value, Value::TimeRange(_)),
            "runtime" => matches!(value, Value::Duration(_)),
            "url" => matches!(value, Value::Url(_)),
            "language" => matches!(value, Value::Language(_)),
            _ => true,
        };

        if valid {
            self.content.insert(field, value);
            Ok(())
        } else {
            Err(SetFieldError { field, value })
        }
    }
}

/// The error when a value is invalid for a field.
#[derive(Clone, Error, Debug, PartialEq)]
#[error("type `{value}` is not allowed in field `{field}`")]
pub struct SetFieldError {
    /// The field for which the value is invalid.
    pub field: String,
    /// The invalid value.
    pub value: Value,
}

macro_rules! fields {
    ($($name:ident: $field_name:expr $(=> $set_type:ty $(, $get_type:ty)?)?);* $(;)*) => {
        $(
            fields!(@get $name: $field_name $(=> $($get_type,)? &$set_type)?);
            fields!(@set $name: $field_name $(=> $set_type)?);
        )*
    };

    (@get $name:ident: $field_name:expr) => {
        fields!(@get $name: $field_name => &str);
    };

    (@get $name:ident: $field_name:expr => $get_type:ty $(, $ignore:ty)?) => {
        paste! {
            #[doc = "Get and parse the `" $field_name "` field."]
            pub fn $name(&self) -> Option<$get_type> {
                self.get($field_name)
                    .map(|item| <$get_type>::try_from(item).unwrap())
            }
        }
    };

    (@set $name:ident: $field_name:expr) => {
        fields!(@set $name: $field_name => String);
    };

    (@set $name:ident: $field_name:expr => $set_type:ty) => {
        paste! {
            #[doc = "Set a value in the `" $field_name "` field."]
            pub fn [<set_ $name>](&mut self, item: $set_type) {
                self.content.insert($field_name.to_string(), Value::from(item));
            }
        }
    };
}

impl Entry {
    fields! {
        title: "title" => Title;
        authors: "author" => Vec<Person>, &[Person];
    }
    fields! { @get date: "date" => &Date }

    /// Will recursively get a date off either the entry or any of its ancestors.
    pub fn date_any(&self) -> Option<&Date> {
        self.date().or_else(|| {
            self.parents()
                .into_iter()
                .flatten()
                .filter_map(|p| p.date_any())
                .next()
        })
    }

    fields! { @set date: "date" => Date }
    fields! {
        parents: "parent" => Vec<Entry>, &[Entry];
        editors: "editor" => Vec<Person>, &[Person];
        affiliated_persons: "affiliated" => Vec<(Vec<Person>, PersonRole)>, &[(Vec<Person>, PersonRole)];
        publisher: "publisher" => FmtString;
        location: "location" => FmtString;
        organization: "organization";
        issue: "issue" => NumOrStr;
        volume: "volume" => std::ops::Range<i64>;
        volume_total: "volume-total" => i64;
        edition: "edition" => NumOrStr;
        page_range: "page-range" => std::ops::Range<i64>;
    }

    /// Get and parse the `page-total` field, falling back on `page-range` if
    /// not specified.
    pub fn page_total(&self) -> Option<i64> {
        self.get("page-total")
            .cloned()
            .or_else(|| self.page_range().map(|r| Value::from(r.end - r.start)))
            .map(|item| i64::try_from(item).unwrap())
    }

    fields! { @set page_total: "page-total" => i64 }
    fields! { time_range: "time-range" => std::ops::Range<Duration> }

    /// Get and parse the `runtime` field, falling back on `time-range` if not
    /// specified.
    pub fn runtime(&self) -> Option<Duration> {
        self.get("runtime")
            .cloned()
            .or_else(|| self.time_range().map(|r| Value::from(r.end - r.start)))
            .map(|item| Duration::try_from(item).unwrap())
    }

    fields! { @set runtime: "runtime" => Duration }
    fields! { @get url: "url" => &QualifiedUrl }

    /// Will recursively get an URL off either the entry or any of its ancestors.
    pub fn url_any(&self) -> Option<&QualifiedUrl> {
        self.url().or_else(|| {
            self.parents()
                .into_iter()
                .flatten()
                .filter_map(|p| p.url_any())
                .next()
        })
    }

    fields! { @set url: "url" => QualifiedUrl }
    fields! {
        doi: "doi";
        serial_number: "serial-number";
        isbn: "isbn";
        issn: "issn";
        language: "language" => LanguageIdentifier;
        archive: "archive" => FmtString;
        archive_location: "archive-location" => FmtString;
        note: "note";
    }
}

impl Entry {
    /// Get and parse the `affiliated` field and only return persons of a given
    /// [role](PersonRole).
    pub(crate) fn affiliated_with_role(&self, role: PersonRole) -> Vec<Person> {
        self.affiliated_persons()
            .into_iter()
            .flatten()
            .filter_map(|(persons, r)| if r == &role { Some(persons) } else { None })
            .flatten()
            .cloned()
            .collect()
    }
}

#[cfg(feature = "biblatex")]
impl Entry {
    /// Adds a parent to the current entry. The parent
    /// list will be created if there is none.
    pub(crate) fn add_parent(&mut self, entry: Entry) {
        if let Some(parents) = self.content.get_mut("parent").and_then(|f| {
            if let Value::Entries(e) = f {
                Some(e)
            } else {
                None
            }
        }) {
            parents.push(entry);
        } else {
            self.set_parents(vec![entry]);
        }
    }

    /// Adds affiliated persons. The list will be created if there is none.
    pub(crate) fn add_affiliated_persons(
        &mut self,
        new_persons: (Vec<Person>, PersonRole),
    ) {
        if let Some(affiliated) = self.content.get_mut("affiliated").and_then(|f| {
            if let Value::PersonsWithRoles(e) = f {
                Some(e)
            } else {
                None
            }
        }) {
            affiliated.push(new_persons);
        } else {
            self.set_affiliated_persons(vec![new_persons]);
        }
    }

    pub(crate) fn parents_mut(&mut self) -> Option<&mut [Entry]> {
        self.content.get_mut("parent").and_then(|f| {
            if let Value::Entries(e) = f {
                Some(e.as_mut_slice())
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use style::Citation;

    use super::*;
    use crate::io::from_yaml_str;
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
            let citation = Citation::new(&entry, None);
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

    #[test]
    fn test_entry_set() {
        let mut entry = Entry::new("key", EntryType::Misc);
        let err = entry.set("author", Value::Integer(1)).unwrap_err();
        assert_eq!(err.to_string(), "type `integer` is not allowed in field `author`",)
    }

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
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
}
