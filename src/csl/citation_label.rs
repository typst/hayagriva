//! Alphanumeric labels for citations.

use std::fmt::Write;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    Entry,
    types::{EntryType, Person, PersonRole},
};

/// Citation labels in the form of numbers.
///
/// For example, the output could be Rass97 or MKG+21. \
/// Corresponds to LaTeX's `alphabetical` style.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Alphanumerical {
    /// Defines how many letters are allowed to describe an entry.
    pub letters: usize,
}

impl Default for Alphanumerical {
    fn default() -> Self {
        Self::new()
    }
}

impl Alphanumerical {
    /// Create a new instance of this [`CitationStyle`].
    pub fn new() -> Self {
        Self { letters: 3 }
    }

    fn creators(&self, entry: &Entry) -> String {
        let creators = get_creators(entry);

        match creators.len() {
            0 => {
                let pseudo_creator = if let Some(org) = entry.organization() {
                    org.to_string()
                } else if let Some(title) = entry.get_full().title() {
                    title.value.to_string()
                } else {
                    entry.key().chars().filter(|c| c.is_alphabetic()).collect::<String>()
                };

                pseudo_creator.graphemes(true).take(self.letters).collect()
            }
            1 => creators[0].name.graphemes(true).take(self.letters).collect(),
            2 | 3 => creators
                .iter()
                .filter_map(|person| person.name.graphemes(true).next())
                .collect(),
            _ => creators[0]
                .name
                .graphemes(true)
                .take(self.letters)
                .chain(std::iter::once("+"))
                .collect(),
        }
    }

    fn year(entry: &Entry) -> Option<String> {
        let year = entry
            .date_any()
            .or_else(|| entry.url_any().and_then(|u| u.visit_date.as_ref()))
            .map(|date| {
                let mut year = i32::abs(date.year % 100);
                if date.year <= 0 {
                    year += 1;
                }
                year
            });

        year.and_then(|y| {
            let mut num = String::with_capacity(2);
            write!(&mut num, "{y:02}").ok()?;
            Some(num)
        })
    }

    pub fn citation(self, entry: &Entry) -> String {
        let full_entry = entry.get_full();

        let creators = self.creators(full_entry);
        let mut res = creators.clone();
        let year_opt = Self::year(full_entry);
        if let Some(year) = year_opt.as_ref() {
            res += year;
        }

        res
    }
}

/// Get the creator of an entry.
fn get_creators(entry: &Entry) -> Vec<&Person> {
    if let Some(authors) = entry.authors() {
        authors.iter().collect()
    } else if let Some(eds) = entry.editors() {
        eds.iter().collect()
    } else if entry.entry_type == EntryType::Video {
        let tv_series = select!((Video["issue", "volume"]) > ("p":Video));
        if let Some(mut bindings) = tv_series.apply(entry) {
            let mut affs = entry.affiliated_with_role(PersonRole::Director);
            affs.extend(entry.affiliated_with_role(PersonRole::Writer));
            if !affs.is_empty() {
                affs
            } else {
                let parent = bindings.remove("p").unwrap();
                parent.affiliated_with_role(PersonRole::ExecutiveProducer)
            }
        } else {
            let dir = entry.affiliated_with_role(PersonRole::Director);
            if !dir.is_empty() {
                dir
            } else {
                entry.affiliated_with_role(PersonRole::ExecutiveProducer)
            }
        }
    } else {
        let compilers = entry.affiliated_with_role(PersonRole::Compiler);
        if !compilers.is_empty() {
            return compilers;
        }

        let translators = entry.affiliated_with_role(PersonRole::Translator);
        return translators;
    }
}
