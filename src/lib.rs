/*!
Hayagriva provides a YAML-backed format and data model for various
bibliography items as well as a CSL processor formatting both in-text citations and
reference lists based on these literature databases.

The crate is intended to assist scholarly writing and reference management
and can be used both through a CLI and an API.

Below, there is an example of how to parse a YAML database and get a Modern
Language Association-style citation.

# Supported styles

Hayagriva supports all styles provided in the
[official Citation Style Language repository](https://github.com/citation-style-language/styles),
currently over 2,600. You must provide your own style file, which can be
obtained there.

# Usage

```rust
use hayagriva::io::from_yaml_str;

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
assert_eq!(bib.get("crazy-rich").unwrap().date().unwrap().year, 2014);

// Format the reference
use std::fs;
use hayagriva::{
    BibliographyDriver, BibliographyRequest, BufWriteFormat,
    CitationItem, CitationRequest, LocaleFile, IndependentStyle
};

let en_locale = fs::read_to_string("tests/data/locales-en-US.xml").unwrap();
let locales = [LocaleFile::from_xml(&en_locale).unwrap().into()];

let style = fs::read_to_string("tests/data/art-history.csl").unwrap();
let style = IndependentStyle::from_xml(&style).unwrap();

let mut driver = BibliographyDriver::new();

for entry in bib.iter() {
    let items = vec![CitationItem::with_entry(entry)];
    driver.citation(CitationRequest::from_items(items, &style, &locales));
}

let result = driver.finish(BibliographyRequest {
    style: &style,
    locale: None,
    locale_files: &locales,
});

for cite in result.citations {
    println!("{}", cite.citation.to_string())
}
```

To format entries, you need to wrap them in a [`CitationRequest`]. Each of these
can reference multiple entries in their respective [`CitationItem`]s.
Use these with a [`BibliographyDriver`] to obtain formatted citations and bibliographies.

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
assert!(journal.matches(entries.nth(0).unwrap()));
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

mod csl;
pub mod io;
pub mod lang;
pub mod types;
mod util;

use std::collections::BTreeMap;

#[cfg(feature = "rkyv")]
pub use crate::csl::archive;
pub use citationberg::LocaleCode;
pub use citationberg::{
    Display, FontStyle, FontVariant, FontWeight, IndependentStyle, LocaleFile,
    LongShortForm, Style, TextDecoration, VerticalAlign,
};
pub use csl::{
    standalone_citation, BibliographyDriver, BibliographyRequest, Brackets,
    BufWriteFormat, CitationItem, CitationRequest, Elem, ElemChild, ElemChildren,
    ElemMeta, Formatted, Formatting, Rendered, RenderedBibliography, RenderedCitation,
    SpecialForm, SpecificLocator,
};
pub use selectors::{Selector, SelectorError};

use indexmap::IndexMap;
use paste::paste;
use serde::{de::Visitor, Deserialize, Serialize};
use types::*;
use unic_langid::LanguageIdentifier;
use util::{
    deserialize_one_or_many_opt, serialize_one_or_many, serialize_one_or_many_opt,
    OneOrMany,
};

/// A collection of bibliographic entries.
#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub struct Library(IndexMap<String, Entry>);

impl Library {
    /// Construct a new, empty bibliography library.
    pub fn new() -> Self {
        Self(IndexMap::new())
    }

    /// Add an entry to the library.
    pub fn push(&mut self, entry: &Entry) {
        self.0.insert(entry.key.clone(), entry.clone());
    }

    /// Retrieve an entry from the library.
    pub fn get(&self, key: &str) -> Option<&Entry> {
        self.0.get(key)
    }

    /// Get an iterator over the entries in the library.
    pub fn iter(&self) -> impl Iterator<Item = &Entry> {
        self.0.values()
    }

    /// Get an iterator over the keys in the library.
    pub fn keys(&self) -> impl Iterator<Item = &str> {
        self.0.keys().map(|k| k.as_str())
    }

    /// Remove an entry from the library.
    pub fn remove(&mut self, key: &str) -> Option<Entry> {
        self.0.remove(key)
    }

    /// Get the length of the library.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check whether the library is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the nth entry in the library.
    pub fn nth(&self, n: usize) -> Option<&Entry> {
        self.0.get_index(n).map(|(_, v)| v)
    }
}

impl<'a> IntoIterator for &'a Library {
    type Item = &'a Entry;
    type IntoIter = indexmap::map::Values<'a, String, Entry>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.values()
    }
}

impl IntoIterator for Library {
    type Item = Entry;
    type IntoIter = std::iter::Map<
        indexmap::map::IntoIter<String, Entry>,
        fn((String, Entry)) -> Entry,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().map(|(_, v)| v)
    }
}

impl FromIterator<Entry> for Library {
    fn from_iter<T: IntoIterator<Item = Entry>>(iter: T) -> Self {
        Self(iter.into_iter().map(|e| (e.key().to_string(), e)).collect())
    }
}

macro_rules! entry {
    ($(
        $(#[doc = $doc:literal])*
        $(#[serde $serde:tt])*
        $s:literal => $i:ident : $t:ty
        $(| $d:ty)? $(,)?
    ),*) => {
        // Build the struct and make it serializable.

        /// A citable item in a bibliography.
        #[derive(Debug, Clone, PartialEq, Eq, Serialize, Hash)]
        pub struct Entry {
            /// The key of the entry.
            #[serde(skip)]
            key: String,
            /// The type of the item.
            #[serde(rename = "type")]
            entry_type: EntryType,
            $(
                $(#[doc = $doc])*
                $(#[serde $serde])*
                #[serde(skip_serializing_if = "Option::is_none")]
                #[serde(rename = $s)]
                $i: Option<$t>,
            )*
            /// Item in which the item was published / to which it is strongly
            /// associated to.
            #[serde(serialize_with = "serialize_one_or_many")]
            #[serde(skip_serializing_if = "Vec::is_empty")]
            #[serde(rename = "parent")]
            parents: Vec<Entry>,
        }

        impl Entry {
            /// Get the key of the entry.
            pub fn key(&self) -> &str {
                &self.key
            }

            /// Construct a new, empty entry.
            pub fn new(key: &str, entry_type: EntryType) -> Self {
                Self {
                    key: key.to_owned(),
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

        /// Getters.
        impl Entry {
            /// Get the type of the entry.
            pub fn entry_type(&self) -> &EntryType {
                &self.entry_type
            }

            /// Get the parents of the entry.
            pub fn parents(&self) -> &[Entry] {
                &self.parents
            }

            $(
                entry!(@get $(#[doc = $doc])* $s => $i : $t $(| $d)?);
            )*
        }

        /// Setters.
        impl Entry {
            /// Set the parents of the entry.
            pub fn set_parents(&mut self, parents: Vec<Entry>) {
                self.parents = parents;
            }


            $(
                entry!(@set $s => $i : $t);
            )*
        }

        /// The library deserialization also handles entries.
        ///
        /// Entries do not implement [`Deserialize`] because they have a data
        /// dependency on their key (stored in the parent map) and their
        /// children for default types.
        impl<'de> Deserialize<'de> for Library {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct MyVisitor;

                #[derive(Deserialize)]
                struct NakedEntry {
                    #[serde(rename = "type")]
                    entry_type: Option<EntryType>,
                    #[serde(default)]
                    #[serde(rename = "parent")]
                    parents: OneOrMany<NakedEntry>,
                    $(
                        $(#[serde $serde])*
                        #[serde(rename = $s)]
                        #[serde(default)]
                        $i: Option<$t>,
                    )*
                }

                impl NakedEntry {
                    /// Convert into a full entry using the child entry type
                    /// (if any) and the key.
                    fn into_entry<E>(
                        self,
                        key: &str,
                        child_entry_type: Option<EntryType>,
                    ) -> Result<Entry, E>
                        where E: serde::de::Error
                    {
                        let entry_type = self.entry_type
                            .or_else(|| child_entry_type.map(|e| e.default_parent()))
                            .ok_or_else(|| E::custom("no entry type"))?;

                        let parents: Result<Vec<_>, _> = self.parents
                            .into_iter()
                            .map(|p| p.into_entry(key, Some(entry_type)))
                            .collect();

                        Ok(Entry {
                            key: key.to_owned(),
                            entry_type,
                            parents: parents?,
                            $(
                                $i: self.$i,
                            )*
                        })
                    }
                }

                impl<'de> Visitor<'de> for MyVisitor {
                    type Value = Library;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter)
                        -> std::fmt::Result
                    {
                        formatter.write_str(
                            "a map between cite keys and entries"
                        )
                    }

                    fn visit_map<A>(self, mut map: A)
                        -> Result<Self::Value, A::Error>
                    where
                        A: serde::de::MapAccess<'de>,
                    {
                        let mut entries = Vec::with_capacity(
                            map.size_hint().unwrap_or(0).min(128)
                        );
                        while let Some(key) = map.next_key::<String>()? {
                            if entries.iter().any(|(k, _)| k == &key) {
                                return Err(serde::de::Error::custom(format!(
                                    "duplicate key {}",
                                    key
                                )));
                            }

                            let entry: NakedEntry = map.next_value()?;
                            entries.push((key, entry));
                        }

                        let entries: Result<IndexMap<_, _>, A::Error> =
                            entries.into_iter().map(|(k, v)| {
                                v.into_entry(&k, None).map(|e| (k, e))
                            }).collect();

                        Ok(Library(entries?))
                    }
                }

                deserializer.deserialize_map(MyVisitor)
            }
        }
    };

    (@match
        $s:literal => $i:ident,
        $naked:ident, $map:ident $(,)?
    ) => {
        $naked.$i = Some($map.next_value()?)
    };

    // All items with a serde attribute are expected to be collections.
    (@match
        $(#[serde $serde:tt])+
        $s:literal => $i:ident,
        $naked:ident, $map:ident $(,)?
    ) => {
        let one_or_many: OneOrMany = $map.next_value()?;
        $naked.$i = Some(one_or_many.into());
    };

    // Getter macro for deref types
    (@get $(#[$docs:meta])+ $s:literal => $i:ident : $t:ty | $d:ty $(,)?) => {
            $(#[$docs])+
            pub fn $i(&self) -> Option<&$d> {
                self.$i.as_deref()
            }
    };

    // Getter macro for regular types.
    (@get $(#[$docs:meta])+ $s:literal => $i:ident : $t:ty $(,)?) => {
        $(#[$docs])+
        pub fn $i(&self) -> Option<&$t> {
            self.$i.as_ref()
        }
    };

    // Setter for all types.
    (@set $s:literal => $i:ident : $t:ty $(,)?) => {
        paste! {
            #[doc = "Set the `" $s "` field."]
            pub fn [<set_ $i>](&mut self, $i: $t) {
                self.$i = Some($i);
            }
        }
    };
}

entry! {
    /// Title of the item.
    "title" => title: FormatString,
    /// Persons primarily responsible for creating the item.
    #[serde(serialize_with = "serialize_one_or_many_opt")]
    #[serde(deserialize_with = "deserialize_one_or_many_opt")]
    "author" => authors: Vec<Person> | [Person],
    /// Date at which the item was published.
    "date" => date: Date,
    /// Persons responsible for selecting and revising the content of the item.
    #[serde(serialize_with = "serialize_one_or_many_opt")]
    #[serde(deserialize_with = "deserialize_one_or_many_opt")]
    "editor" => editors: Vec<Person> | [Person],
    /// Persons involved in the production of the item that are not authors or editors.
    #[serde(serialize_with = "serialize_one_or_many_opt")]
    #[serde(deserialize_with = "deserialize_one_or_many_opt")]
    "affiliated" => affiliated: Vec<PersonsWithRoles> | [PersonsWithRoles],
    /// Publisher of the item.
    "publisher" => publisher: FormatString,
    /// Physical location at which the item was published or created.
    "location" => location: FormatString,
    /// Organization at/for which the item was created.
    "organization" => organization: FormatString,
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
    /// Any serial number or version describing the item that is not appropriate
    /// for the fields doi, edition, isbn or issn (may be assigned by the author
    /// of the item; especially useful for preprint archives).
    #[serde(alias = "serial")]
    "serial-number" => serial_number: SerialNumber,
    /// The language of the item.
    "language" => language: LanguageIdentifier,
    /// Name of the institution/collection where the item is kept.
    "archive" => archive: FormatString,
    /// Physical location of the institution/collection where the item is kept.
    "archive-location" => archive_location: FormatString,
    /// The call number of the item in the institution/collection.
    "call-number" => call_number: FormatString,
    /// Additional description to be appended in the bibliographic entry.
    "note" => note: FormatString,
}

impl Entry {
    /// Get and parse the `affiliated` field and only return persons of a given
    /// [role](PersonRole).
    pub(crate) fn affiliated_with_role(&self, role: PersonRole) -> Vec<&Person> {
        self.affiliated
            .iter()
            .flatten()
            .filter_map(
                |PersonsWithRoles { names, role: r }| {
                    if r == &role {
                        Some(names)
                    } else {
                        None
                    }
                },
            )
            .flatten()
            .collect()
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

    /// Apply a selector and return a bound parent entry or self.
    pub fn bound_select(&self, selector: &Selector, binding: &str) -> Option<&Entry> {
        selector.apply(self).and_then(|map| map.get(binding).copied())
    }

    /// Will recursively get a date off either the entry or any of its ancestors.
    pub fn date_any(&self) -> Option<&Date> {
        self.map(|e| e.date.as_ref())
    }

    /// Will recursively get an URL off either the entry or any of its ancestors.
    pub fn url_any(&self) -> Option<&QualifiedUrl> {
        self.map(|e| e.url.as_ref())
    }

    /// Retrieve a keyed serial number.
    pub fn keyed_serial_number(&self, key: &str) -> Option<&str> {
        self.serial_number
            .as_ref()
            .and_then(|s| s.0.get(key).map(|s| s.as_str()))
    }

    /// Set a keyed serial number.
    pub fn set_keyed_serial_number(&mut self, key: &str, value: String) {
        if let Some(serials) = &mut self.serial_number {
            serials.0.insert(key.to_owned(), value);
        } else {
            let mut map = BTreeMap::new();
            map.insert(key.to_owned(), value);
            self.serial_number = Some(SerialNumber(map));
        }
    }

    /// The Digital Object Identifier of the item.
    pub fn doi(&self) -> Option<&str> {
        self.keyed_serial_number("doi")
    }

    /// Set the `doi` field.
    pub fn set_doi(&mut self, doi: String) {
        self.set_keyed_serial_number("doi", doi);
    }

    /// International Standard Book Number (ISBN), prefer ISBN-13.
    pub fn isbn(&self) -> Option<&str> {
        self.keyed_serial_number("isbn")
    }

    /// Set the `isbn` field.
    pub fn set_isbn(&mut self, isbn: String) {
        self.set_keyed_serial_number("isbn", isbn);
    }

    /// International Standard Serial Number (ISSN).
    pub fn issn(&self) -> Option<&str> {
        self.keyed_serial_number("issn")
    }

    /// Set the `issn` field.
    pub fn set_issn(&mut self, issn: String) {
        self.set_keyed_serial_number("issn", issn);
    }

    /// PubMed Identifier (PMID).
    pub fn pmid(&self) -> Option<&str> {
        self.keyed_serial_number("pmid")
    }

    /// Set the `pmid` field.
    pub fn set_pmid(&mut self, pmid: String) {
        self.set_keyed_serial_number("pmid", pmid);
    }

    /// PubMed Central Identifier (PMCID).
    pub fn pmcid(&self) -> Option<&str> {
        self.keyed_serial_number("pmcid")
    }

    /// Set the `pmcid` field.
    pub fn set_pmcid(&mut self, pmcid: String) {
        self.set_keyed_serial_number("pmcid", pmcid);
    }

    /// ArXiv identifier.
    pub fn arxiv(&self) -> Option<&str> {
        self.keyed_serial_number("arxiv")
    }

    /// Set the `arxiv` field.
    pub fn set_arxiv(&mut self, arxiv: String) {
        self.set_keyed_serial_number("arxiv", arxiv);
    }

    /// Get the container of an entry like CSL defines it.
    pub(crate) fn get_container(&self) -> Option<&Self> {
        let retrieve_container = |possible: &[EntryType]| {
            for possibility in possible {
                if let Some(container) =
                    self.parents.iter().find(|e| e.entry_type == *possibility)
                {
                    return Some(container);
                }
            }

            None
        };

        match &self.entry_type {
            EntryType::Article => retrieve_container(&[
                EntryType::Book,
                EntryType::Periodical,
                EntryType::Newspaper,
                EntryType::Blog,
                EntryType::Reference,
                EntryType::Web,
            ]),
            EntryType::Chapter => retrieve_container(&[
                EntryType::Book,
                EntryType::Anthology,
                EntryType::Reference,
                EntryType::Report,
            ]),
            EntryType::Report => {
                retrieve_container(&[EntryType::Book, EntryType::Anthology])
            }
            EntryType::Web => retrieve_container(&[EntryType::Web]),
            EntryType::Scene => retrieve_container(&[
                EntryType::Audio,
                EntryType::Video,
                EntryType::Performance,
                EntryType::Artwork,
            ]),
            EntryType::Case => retrieve_container(&[
                EntryType::Book,
                EntryType::Anthology,
                EntryType::Reference,
                EntryType::Report,
            ]),
            EntryType::Post => {
                retrieve_container(&[EntryType::Thread, EntryType::Blog, EntryType::Web])
            }
            EntryType::Thread => {
                retrieve_container(&[EntryType::Thread, EntryType::Web, EntryType::Blog])
            }
            _ => None,
        }
    }

    /// Get the collection of an entry like CSL defines it.
    pub(crate) fn get_collection(&self) -> Option<&Self> {
        match &self.entry_type {
            EntryType::Anthology
            | EntryType::Newspaper
            | EntryType::Performance
            | EntryType::Periodical
            | EntryType::Proceedings
            | EntryType::Book
            | EntryType::Reference
            | EntryType::Exhibition => self.parents.iter().find(|e| {
                e.entry_type == self.entry_type || e.entry_type == EntryType::Anthology
            }),
            _ => self.parents.iter().find_map(|e| e.get_collection()),
        }
    }

    /// Search a parent by DFS.
    pub(crate) fn dfs_parent(&self, kind: EntryType) -> Option<&Self> {
        if self.entry_type == kind {
            return Some(self);
        }

        for parent in &self.parents {
            if let Some(entry) = parent.dfs_parent(kind) {
                return Some(entry);
            }
        }

        None
    }

    /// Get the original entry.
    pub(crate) fn get_original(&self) -> Option<&Self> {
        self.dfs_parent(EntryType::Original)
    }
}

#[cfg(feature = "biblatex")]
impl Entry {
    /// Adds a parent to the current entry. The parent
    /// list will be created if there is none.
    pub(crate) fn add_parent(&mut self, entry: Self) {
        self.parents.push(entry);
    }

    /// Adds affiliated persons. The list will be created if there is none.
    pub(crate) fn add_affiliated_persons(
        &mut self,
        new_persons: (Vec<Person>, PersonRole),
    ) {
        let obj = PersonsWithRoles { names: new_persons.0, role: new_persons.1 };
        if let Some(affiliated) = &mut self.affiliated {
            affiliated.push(obj);
        } else {
            self.affiliated = Some(vec![obj]);
        }
    }

    pub(crate) fn parents_mut(&mut self) -> &mut [Self] {
        &mut self.parents
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;
    use crate::io::from_yaml_str;

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = [$($key,)*];
            let selector = Selector::parse($select).unwrap();
            for entry in $entries.iter() {
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
        let contents = fs::read_to_string("tests/data/basic.yml").unwrap();
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
        let contents = fs::read_to_string("tests/data/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();

        select!(
            "a:article > (b:conference & c:(video|blog|web))",
            entries >> "wwdc-network",
            ["a", "b", "c"]
        );
    }
}
