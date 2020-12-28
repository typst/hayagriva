//! Author-Date citations as defined in chapter 15 of the 17th edition of the
//! Chicago Manual of Style.

use std::collections::{BTreeMap, HashMap};

use super::{
    and_list_opt, get_chunk_title, get_creators, web_creator, CommonChicagoConfig,
};
use crate::style::{
    alph_designator, AtomicCitation, Bracket, BracketMode, BracketPreference,
    CitationError, CitationFormatter, DisplayString,
};
use crate::types::EntryType::*;
use crate::types::Person;
use crate::Entry;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Uniqueness {
    None,
    Initials,
    Full,
}

/// Checks if the keys are in the database and returns them as reference
/// markers, since they are already unique.
pub struct AuthorYear<'s> {
    entries: BTreeMap<&'s str, &'s Entry>,
    authors: Vec<Person>,
    entry_authors: HashMap<&'s str, Vec<usize>>,
    unique: Vec<Uniqueness>,
    /// Common config options for all chicago styles.
    /// Primarily important for works without titles.
    pub common: CommonChicagoConfig,
    /// Number of authors (equal or greater) for which the author
    /// list is truncated.
    pub et_al_limit: u8,
}

impl<'s> AuthorYear<'s> {
    /// Create a new author year citation formatter.
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        let entries: BTreeMap<&'s str, &Entry> =
            entries.map(|e| (e.key.as_ref(), e)).collect();
        let mut i = 0;
        let mut authors = vec![];
        let entry_authors: HashMap<&'s str, Vec<usize>> = entries
            .iter()
            .map(|(k, e)| {
                (k.clone(), {
                    let creators = get_creators(e).0;
                    let mut indices = vec![];
                    for creator in creators.into_iter() {
                        indices.push(i);
                        i += 1;
                        authors.push(creator);
                    }
                    indices
                })
            })
            .collect();

        let mut unique = vec![];
        for author in &authors {
            let mut uniqueness = Uniqueness::Full;

            for other in &authors {
                if other != author && other.name == author.name {
                    if other.initials(None) == author.initials(None) {
                        uniqueness = Uniqueness::None;
                    } else if uniqueness != Uniqueness::None {
                        uniqueness = Uniqueness::Initials;
                    }
                }
            }

            unique.push(uniqueness);
        }
        Self {
            entries,
            entry_authors,
            authors,
            unique,
            common: CommonChicagoConfig::new(),
            et_al_limit: 4,
        }
    }

    fn get_authors(&self, key: &str) -> Option<Vec<&Person>> {
        self.entry_authors
            .get(key)
            .map(|indices| indices.iter().map(|&i| &self.authors[i]).collect())
    }
}

impl<'s> CitationFormatter<'s> for AuthorYear<'s> {
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        let mut items: Vec<DisplayString> = vec![];
        for atomic in citation.into_iter() {
            let entry = self
                .entries
                .get(atomic.key)
                .ok_or_else(|| CitationError::KeyNotFound(atomic.key.into()))?;

            let authors = self.get_authors(atomic.key).unwrap();

            let date = entry.date_any();
            let similars = self
                .entries
                .iter()
                .map(|(_, e)| e)
                .filter(|&e| {
                    e.date_any().map(|d| d.year) == date.map(|d| d.year)
                        && self.get_authors(&e.key).unwrap() == authors
                })
                .collect::<Vec<_>>();

            let mut s = if !authors.is_empty() {
                let mut last_full = false;
                let names = self.entry_authors[atomic.key]
                    .iter()
                    .map(|&index| {
                        let uniqueness = self.unique[index];
                        last_full = uniqueness == Uniqueness::None;
                        let author = &self.authors[index];
                        match uniqueness {
                            Uniqueness::Full => author.name.clone(),
                            Uniqueness::Initials => author.given_first(true),
                            Uniqueness::None => author.name_first(false, true),
                        }
                    })
                    .collect::<Vec<_>>();

                let et_al_auth = if names.len() >= self.et_al_limit as usize {
                    let local_authors = self.entry_authors[atomic.key]
                        .iter()
                        .map(|&index| &self.authors[index])
                        .collect::<Vec<_>>();
                    // 0: First author is different
                    let mut distinction_authors = 0;
                    for (&key, authors) in self.entry_authors.iter() {
                        if key == atomic.key {
                            continue;
                        }
                        let authors = authors
                            .into_iter()
                            .map(|&index| &self.authors[index])
                            .collect::<Vec<_>>();
                        let mut mismatch = authors.len();

                        for (i, author) in authors.iter().enumerate() {
                            if author != &local_authors[i] {
                                mismatch = i;
                                break;
                            }
                        }

                        if mismatch == authors.len() {
                            continue;
                        }

                        if mismatch > distinction_authors {
                            distinction_authors = mismatch;
                        }
                    }

                    distinction_authors
                } else {
                    0
                };

                let mut list =
                    and_list_opt(names, false, Some(self.et_al_limit.into()), et_al_auth);

                if last_full {
                    if list.chars().last() != Some('.') {
                        list.push('.');
                    }
                }

                list.into()
            } else if entry.title().is_some() {
                get_chunk_title(entry, true, true, &self.common)
            } else if let Some(creator) =
                web_creator(entry, false, self.common.et_al_limit)
            {
                creator.into()
            } else if matches!(
                entry.entry_type,
                Report | Patent | Legislation | Conference | Exhibition
            ) {
                if let Some(org) = entry.organization() {
                    org.into()
                } else {
                    DisplayString::new()
                }
            } else if let Some(np) = select!(* > ("p":Newspaper)).bound(entry, "p") {
                get_chunk_title(np, true, true, &self.common)
            } else {
                DisplayString::new()
            };

            let space = if let Some(date) = date {
                if !s.is_empty() {
                    s.push(' ');
                }
                s += &date.display_year();
                false
            } else {
                if !s.is_empty() {
                    if s.last() != Some(',') {
                        s.push(',')
                    }
                    s.push(' ');
                }
                s += "n.d.";
                true
            };

            if similars.len() > 1 {
                let pos = similars.iter().position(|&x| x == entry).unwrap();
                let designator = alph_designator(pos);

                if space {
                    s.push(' ');
                }

                s.push(designator);
            }

            if let Some(supplement) = atomic.supplement {
                if supplement.chars().last() != Some(';') {
                    s += ", ";
                }

                s += supplement;
            }

            items.push(s);
        }

        Ok(DisplayString::join(&items, "; "))
    }
}

impl<'s> BracketPreference for AuthorYear<'s> {
    fn default_brackets() -> Bracket {
        Bracket::Parentheses
    }

    fn default_bracket_mode() -> BracketMode {
        BracketMode::Wrapped
    }
}

#[cfg(test)]
mod tests {
    use crate::style::CitationFormatter;
    use crate::types::{Date, EntryType, Person, Title};
    use crate::{style::AtomicCitation, Entry};

    use super::AuthorYear;

    fn date_author_entry(key: &str, authors: Vec<Person>, year: i32) -> Entry {
        let mut e = Entry::new(key, EntryType::Article);
        e.set_authors(authors);
        e.set_date(Date::from_year(year));
        e
    }

    #[allow(non_snake_case)]
    fn A(given: &str, family: &str) -> Person {
        Person::from_strings(&[family, given]).unwrap()
    }

    #[allow(non_snake_case)]
    fn C(key: &str) -> AtomicCitation {
        AtomicCitation::new(key, None, None)
    }

    #[allow(non_snake_case)]
    fn Cs(entries: &[Entry]) -> Vec<AtomicCitation> {
        entries.iter().map(|e| C(e.key.as_ref())).collect::<Vec<_>>()
    }

    #[test]
    fn simple() {
        let es = vec![date_author_entry("key", vec![A("Martin", "Haug")], 2018)];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(formatter.format(citations).unwrap().value, "Haug 2018");
    }

    #[test]
    fn same_author_year() {
        let es = vec![
            date_author_entry("klaus1", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("klaus2", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("unklaus", vec![A("Haus", "Hinsky")], 2018),
            date_author_entry("klaus3", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("klaus4", vec![A("Klaus", "Kinsky")], 2019),
            date_author_entry("klaus5", vec![A("Klaus", "Kinsky")], 2019),
        ];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "Kinsky 2018a; Kinsky 2018b; Hinsky 2018; Kinsky 2018c; Kinsky 2019a; Kinsky 2019b"
        );
    }

    #[test]
    fn author_initials() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Rich", "Doe")], 2011),
        ];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "J. Doe 1967; R. Doe 2011"
        );
    }

    #[test]
    fn author_gn() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Janet", "Doe")], 2011),
        ];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "Doe, John. 1967; Doe, Janet. 2011"
        );
    }

    #[test]
    fn multi_author() {
        let es = vec![
            date_author_entry(
                "key",
                vec![A("Laurenz", "Mädje"), A("Martin", "Haug")],
                2020,
            ),
            date_author_entry(
                "key2",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Madeleine", "Béjart"),
                    A("Charles", "du Fresne"),
                ],
                1648,
            ),
        ];
        let mut formatter = AuthorYear::new(es.iter());
        formatter.et_al_limit = 3;
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "Mädje and Haug 2020; Poquelin et al. 1648"
        );
    }

    #[test]
    fn differentiate_et_al() {
        let es = vec![
            date_author_entry(
                "key",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Madeleine", "Béjart"),
                    A("Charles", "du Fresne"),
                ],
                1648,
            ),
            date_author_entry(
                "2",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Armande", "Béjart"),
                    A("Charles", "du Fresne"),
                ],
                1662,
            ),
        ];
        let mut formatter = AuthorYear::new(es.iter());
        formatter.et_al_limit = 3;
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "Poquelin, M. Béjart, et al. 1648; Poquelin, A. Béjart, et al. 1662"
        );
    }

    #[test]
    fn no_author() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_date(Date::from_year(1999));
        e.set_title(Title::new("Third International Report on Reporting"));
        let es = vec![e];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(
            formatter.format(citations).unwrap().value,
            "Third International Report on Reporting 1999"
        );
    }

    #[test]
    fn no_date() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_authors(vec![A("John", "Doe")]);
        let es = vec![e];
        let formatter = AuthorYear::new(es.iter());
        let citations = Cs(&es);
        assert_eq!(formatter.format(citations).unwrap().value, "Doe, n.d.");
    }
}
