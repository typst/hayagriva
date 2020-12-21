//! Hayagriva provides a YAML-backed format and data model for various
//! bibliography items as well as formatting for both in-text citations and
//! reference lists based on these literature databases.
//!
//! The crate is intended to assist scholarly writing and reference management
//! and can be used both through a CLI and an API.

#![warn(missing_docs)]

use std::collections::HashMap;

pub mod input;
#[cfg(feature = "default")]
mod interop;
pub mod lang;
pub mod output;
pub mod selectors;
pub mod types;

use lang::CaseTransformer;
use types::{
    Date, Duration, EntryType, FormattableString, FormattedString, FormattedTitle,
    NumOrStr, Person, PersonRole, QualifiedUrl, Title,
};

use paste::paste;
use std::convert::TryFrom;
use strum::Display;
use thiserror::Error;
use unic_langid::LanguageIdentifier;

/// The field type enum variants describe what data types can possibly be held
/// by the various content items of an [`Entry`].
#[derive(Clone, Debug, Display, PartialEq)]
pub enum FieldType {
    /// A [Title] containing a canonical value and optionally translations and
    /// shorthands, all of which are formattable.
    Title(Title),
    /// A [FormattableString] with which the user can override various
    /// automatic formatters.
    FormattableString(FormattableString),
    /// A [FormattedString] is a [FormattableString] to which all desired
    /// formatters have been applied.
    FormattedString(FormattedString),
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
    content: HashMap<String, FieldType>,
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
    pub fn kind(&self) -> &EntryType {
        &self.entry_type
    }

    /// Get the unconverted value of a certain key.
    pub fn get(&self, key: &str) -> Option<&FieldType> {
        self.content.get(key)
    }

    /// Set [any data type](FieldType) as value for a given key.
    pub fn set(&mut self, key: String, value: FieldType) -> Result<(), EntrySetError> {
        let valid = match key.as_ref() {
            "parent" => matches!(value, FieldType::Entries(_)),
            "title" => matches!(value, FieldType::Title(_)),
            "location" | "publisher" | "archive" | "archive-location" => {
                matches!(value, FieldType::FormattableString(_))
            }
            "author" | "editor" => matches!(value, FieldType::Persons(_)),
            "date" => matches!(value, FieldType::Date(_)),
            "affiliated" => matches!(value, FieldType::PersonsWithRoles(_)),
            "organization" | "issn" | "isbn" | "doi" | "serial-number" | "note" => {
                matches!(value, FieldType::Text(_))
            }
            "issue" | "edition" => matches!(value, FieldType::IntegerOrText(_)),
            "volume" | "page-range" => matches!(value, FieldType::Range(_)),
            "volume-total" | "page-total" => matches!(value, FieldType::Integer(_)),
            "time-range" => matches!(value, FieldType::TimeRange(_)),
            "runtime" => matches!(value, FieldType::Duration(_)),
            "url" => matches!(value, FieldType::Url(_)),
            "language" => matches!(value, FieldType::Language(_)),
            _ => true,
        };

        if valid {
            self.content.insert(key, value);
            Ok(())
        } else {
            Err(EntrySetError::WrongType(key, value))
        }
    }
}

/// Error which occurs if the value of an entry field could not be retrieved.
#[derive(Clone, Error, Debug, PartialEq)]
pub enum EntrySetError {
    /// The [field type enum](FieldType) does not match the expected variant(s).
    #[error("Type `{1}` not allowed in field `{0}`")]
    WrongType(String, FieldType),
}

macro_rules! fields {
    ($($name:ident: $field_name:expr => FormattableString),* $(,)*) => {
        $(
            fields!(fmt $name: $field_name => FormattableString, FormattedString);
            paste! {
                #[doc = "Get and parse the `" $field_name "` field's value.'"]
                pub fn $name(&self) -> Option<&str> {
                    self.get($field_name)
                        .map(|item| <&FormattableString>::try_from(item).unwrap().value.as_ref())
                }
            }
        )*
    };
    ($($name:ident: $field_name:expr => Title),* $(,)*) => {
        $(
            fields!(fmt $name: $field_name => Title, FormattedTitle);
            paste! {
                #[doc = "Get and parse the `" $field_name "` field's value.'"]
                pub fn $name(&self) -> Option<&str> {
                    self.get($field_name)
                        .map(|item| <&Title>::try_from(item).unwrap().value.value.as_ref())
                }
            }
        )*
    };
    (fmt $($name:ident: $field_name:expr => $src_type:ty, $dst_type:ty),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field as it stands (no formatting applied)."]
                pub fn [<$name _raw>](&self) -> Option<&$src_type> {
                    self.get($field_name)
                        .map(|item| <&$src_type>::try_from(item).unwrap())
                }

                #[doc = "Get, parse, and format the `" $field_name "` field."]
                pub fn [<$name _fmt>](
                    &self,
                    title: Option<&dyn CaseTransformer>,
                    sentence: Option<&dyn CaseTransformer>,
                ) -> Option<$dst_type> {
                    self.get($field_name)
                        .map(|item| <$src_type>::try_from(item.clone()).unwrap().format(title, sentence))
                }

                fields!(single_set $name => $field_name, $src_type);
            }
        )*
    };

    ($($name:ident: $field_name:expr $(=> $res:ty)?),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn $name(&self) -> Option<fields!(@type ref $($res)?)> {
                    self.get($field_name)
                        .map(|item| <fields!(@type ref $($res)?)>::try_from(item).unwrap())
                }

                fields!(single_set $name => $field_name, fields!(@type $($res)?));
            }
        )*
    };

    ($($name:ident: $field_name:expr => $res:ty, $res_ref:ty),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn $name(&self) -> Option<$res_ref> {
                    self.get($field_name)
                        .map(|item| <$res_ref>::try_from(item).unwrap())
                }

                fields!(single_set $name => $field_name, $res);
            }
        )*
    };

    (single_set $name:ident => $field_name:expr, $other_type:ty) => {
        paste! {
            #[doc = "Set a value in the `" $field_name "` field."]
            pub fn [<set_ $name>](&mut self, item: $other_type) {
                self.set($field_name.to_string(), FieldType::from(item)).unwrap();
            }
        }
    };

    (@type) => {String};
    (@type $res:ty) => {$res};
    (@type ref) => {&str};
    (@type ref $res:ty) => {&$res};
}

impl Entry {
    fields!(parents: "parent" => Vec<Entry>, &[Entry]);
    fields!(title: "title" => Title);

    /// Get and parse the `author` field. This will always return a result
    pub fn authors(&self) -> &[Person] {
        self.get("author")
            .map(|item| <&[Person]>::try_from(item).unwrap())
            .unwrap_or_default()
    }

    /// Get and parse the `author` field.
    /// This will fail if there are no authors.
    pub fn authors_fallible(&self) -> Option<&[Person]> {
        self.get("author").map(|item| <&[Person]>::try_from(item).unwrap())
    }

    fields!(single_set authors => "author", Vec<Person>);

    fields!(date: "date" => Date);
    fields!(
        editors: "editor" => Vec<Person>, &[Person],
        affiliated_persons: "affiliated" => Vec<(Vec<Person>, PersonRole)>, &[(Vec<Person>, PersonRole)]
    );

    /// Get and parse the `affiliated` field and only return persons of a given
    /// [role](PersonRole).
    pub fn affiliated_filtered(&self, role: PersonRole) -> Vec<Person> {
        self.affiliated_persons()
            .into_iter()
            .flat_map(|v| v)
            .filter_map(|(v, erole)| if erole == &role { Some(v) } else { None })
            .flatten()
            .cloned()
            .collect::<Vec<Person>>()
    }

    fields!(
        organization: "organization",
        issue: "issue" => NumOrStr,
        edition: "edition" => NumOrStr,
        volume: "volume" => std::ops::Range<i64>,
        total_volumes: "volume-total" => i64,
        page_range: "page-range" => std::ops::Range<i64>
    );

    /// Will recursively get a date off either the entry or its parents.
    pub fn any_date(&self) -> Option<&Date> {
        self.date().or_else(|| {
            self.parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.any_date())
                .next()
        })
    }

    /// Adds a parent to the currrent entry. The parent
    /// list will be created if there is none.
    pub fn add_parent(&mut self, entry: Entry) {
        if let Some(parents) = self
            .content
            .get_mut("parent")
            .and_then(|f| if let FieldType::Entries(e) = f { Some(e) } else { None })
        {
            parents.push(entry);
        } else {
            self.set_parents(vec![entry]);
        }
    }

    /// Adds affiliated persons. The list will be created if there is none.
    pub fn add_affiliated_persons(&mut self, new_persons: (Vec<Person>, PersonRole)) {
        if let Some(affiliated) = self.content.get_mut("affiliated").and_then(|f| {
            if let FieldType::PersonsWithRoles(e) = f {
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

    /// Adds a parent to the currrent entry. The parent
    /// list will be created if there is none.
    pub fn parents_mut(&mut self) -> Option<&mut [Entry]> {
        self.content.get_mut("parent").and_then(|f| {
            if let FieldType::Entries(e) = f {
                Some(e.as_mut_slice())
            } else {
                None
            }
        })
    }

    /// Will recursively get an url off either the entry or its parents.
    pub fn any_url(&self) -> Option<&QualifiedUrl> {
        self.url().or_else(|| {
            self.parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.any_url())
                .next()
        })
    }

    /// Get and parse the `page-total` field, falling back on
    /// `page-range` if not specified.
    pub fn page_total(&self) -> Option<i64> {
        self.get("page-total")
            .map(|ft| ft.clone())
            .or_else(|| self.page_range().map(|r| FieldType::from(r.end - r.start)))
            .map(|item| i64::try_from(item).unwrap())
    }

    fields!(single_set total_pages => "page-total", i64);
    fields!(time_range: "time-range" => std::ops::Range<Duration>);

    /// Get and parse the `runtime` field, falling back on
    /// `time-range` if not specified.
    pub fn runtime(&self) -> Option<Duration> {
        self.get("runtime")
            .map(|ft| ft.clone())
            .or_else(|| self.time_range().map(|r| FieldType::from(r.end - r.start)))
            .map(|item| Duration::try_from(item).unwrap())
    }

    fields!(single_set runtime => "runtime", Duration);

    fields!(
        issn: "issn",
        isbn: "isbn",
        doi: "doi",
        serial_number: "serial-number",
        url: "url" => QualifiedUrl,
        language: "language" => LanguageIdentifier,
        note: "note"
    );
    fields!(
        location: "location" => FormattableString,
        publisher: "publisher" => FormattableString,
        archive: "archive" => FormattableString,
        archive_location: "archive-location" => FormattableString,
    );
}

#[cfg(test)]
mod tests {
    use crate::input::load_yaml_structure;
    use crate::output::{apa, chicago, ieee, mla, AtomicCitation, BibliographyFormatter};
    use crate::selectors::parse;
    use std::fs;

    #[test]
    fn apa() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let apa = apa::Apa::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = apa.format(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn ieee() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let ieee = ieee::Ieee::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = ieee.format(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn mla() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let mla = mla::Mla::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = mla.format(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn chicago_n() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let chicago = chicago::notes::Note::new(entries.iter());

        for entry in &entries {
            let citation = AtomicCitation::new(&entry.key, None, None);

            let refs =
                chicago.get_note(citation, chicago::notes::NoteType::Full).unwrap();
            println!("{}", refs.print_ansi_vt100());
        }
    }

    #[test]
    fn chicago_b() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let chicago =
            chicago::bibliography::BibliographyFormatter::new(chicago::Mode::AuthorDate);

        for entry in &entries {
            let refs = chicago.format(&entry, None);
            println!("{}", refs.print_ansi_vt100());
        }
    }

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
            let expr = parse($select).unwrap();
            for entry in &$entries {
                let res = expr.apply(entry);
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
            let expr = parse($select).unwrap();
            let res = expr.apply(entry).unwrap();
            if !keys.into_iter().all(|k| res.get(k).is_some()) {
                panic!("Results do not contain binding");
            }
        }
    }

    #[test]
    fn selectors() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();

        select_all!("Article > Proceedings", entries, ["zygos"]);
        select_all!("Article > (Periodical | Newspaper)", entries, [
            "omarova-libra",
            "kinetics",
            "house",
            "swedish",
        ]);
        select_all!("(Chapter | Anthos) > (Anthology | Book)", entries, [
            "harry", "gedanken"
        ]);
        select_all!("*[url]", entries, [
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
        ]);
        select_all!("!(*[url] | (* > *[url]))", entries, [
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
        ]);
    }

    #[test]
    fn selector_bindings() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();

        select!(
            "a:Article > (b:Conference & c:(Video|Blog|Web))",
            entries >> "wwdc-network",
            ["a", "b", "c"]
        );
    }
}
