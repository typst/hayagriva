use std::collections::HashSet;

use super::{
    and_list_opt, bibliography::Bibliography, get_chunk_title, get_creators, web_creator,
    ChicagoConfig, Mode,
};
use crate::style::{
    alph_designator, author_title_ord_custom, BibliographyOrdering, BibliographyStyle,
    Brackets, Citation, CitationStyle, Database, DisplayCitation, DisplayReference,
    DisplayString, Record,
};
use crate::types::EntryType::*;
use crate::types::Person;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Uniqueness {
    None,
    Initials,
    Full,
}

/// Citations and bibliographies following the Chicago _Author Date_ style.
///
/// See the 17th edition of the Chicago Manual of Style, Chapter 15, for details
/// on how Chicago advises you to format citations and bibliographies.
///
/// If you're unsure on which Chicago style to use, the manual generally
/// recommends [_Notes and Bibliography_](super::notes::ChicagoNotes) for the humanities and
/// social sciences whereas [_Author Date_](ChicagoAuthorDate) is recommended
/// for natural sciences, mathematics, and engineering.
pub struct ChicagoAuthorDate {
    /// Common config options for all chicago styles.
    /// Primarily important for works without titles.
    pub common: ChicagoConfig,
    /// Number of authors (equal or greater) for which the author
    /// list is truncated.
    pub et_al_limit: u8,
    bib_format: Bibliography,
    /// How the bibliography should be sorted.
    pub sort_bibliography: BibliographyOrdering,
}

impl Default for ChicagoAuthorDate {
    fn default() -> Self {
        Self::new(ChicagoConfig::default())
    }
}

impl ChicagoAuthorDate {
    /// Create a new author year citation formatter.
    pub fn new(common: ChicagoConfig) -> Self {
        Self {
            bib_format: Bibliography::new(Mode::AuthorDate, common.clone()),
            common,
            et_al_limit: 4,
            sort_bibliography: BibliographyOrdering::ByAuthor,
        }
    }

    fn uniqueness<'a>(author: &Person, db: &Database<'a>) -> Uniqueness {
        let total_authors: HashSet<_> = db
            .records()
            .flat_map(|e| get_creators(e.entry).0)
            .filter(|a| a != author)
            .collect();

        let mut unique = Uniqueness::Full;
        for other in total_authors {
            if other.name == author.name {
                if other.initials(None) == author.initials(None) {
                    return Uniqueness::None;
                } else {
                    unique = Uniqueness::Initials;
                }
            }
        }

        unique
    }
}

impl<'a> CitationStyle<'a> for ChicagoAuthorDate {
    fn citation(
        &mut self,
        db: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut items: Vec<DisplayString> = vec![];
        for atomic in parts {
            let entry = atomic.entry;

            let authors = get_creators(entry).0;

            let date = entry.date_any();
            let similars = db
                .records()
                .filter(|&r| {
                    r.entry.date_any().map(|d| d.year) == date.map(|d| d.year)
                        && get_creators(r.entry).0 == authors
                })
                .collect::<Vec<_>>();

            let mut s = if !authors.is_empty() {
                let mut last_full = false;
                let names = authors
                    .iter()
                    .map(|author| {
                        let uniqueness = ChicagoAuthorDate::uniqueness(author, db);
                        last_full = uniqueness == Uniqueness::None;
                        match uniqueness {
                            Uniqueness::Full => author.name.clone(),
                            Uniqueness::Initials => author.given_first(true),
                            Uniqueness::None => author.name_first(false, true),
                        }
                    })
                    .collect::<Vec<_>>();

                let et_al_auth = if names.len() >= self.et_al_limit as usize {
                    // 0: First author is different
                    let mut distinction_authors = 0;
                    for entry in db.records().map(|r| r.entry) {
                        if entry == atomic.entry {
                            continue;
                        }
                        let other_authors = get_creators(entry).0;
                        let mut mismatch = other_authors.len();

                        for (i, author) in other_authors.iter().enumerate() {
                            if author != &authors[i] {
                                mismatch = i;
                                break;
                            }
                        }

                        if mismatch == other_authors.len() {
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
                let pos = similars.iter().position(|&x| x.entry == entry).unwrap();
                let num = if let Some(disambiguation) = similars[pos].disambiguation {
                    disambiguation
                } else {
                    db.records
                        .iter_mut()
                        .find(|(_, r)| r.entry == entry)
                        .unwrap()
                        .1
                        .disambiguation = Some(pos);
                    pos
                };


                let designator = alph_designator(num);

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

        DisplayCitation::new(DisplayString::join(&items, "; "), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Round
    }

    fn wrapped(&self) -> bool {
        true
    }
}

impl<'a> BibliographyStyle<'a> for ChicagoAuthorDate {
    fn bibliography(&self, db: &Database<'a>) -> Vec<DisplayReference<'a>> {
        let mut items = vec![];

        for record in db.records() {
            let (bib, al) = self.bib_format.format(record.entry, record.disambiguation);
            items.push((
                DisplayReference {
                    display: bib,
                    entry: record.entry,
                    prefix: record.prefix.clone().map(Into::into),
                },
                al,
            ))
        }

        match self.sort_bibliography {
            BibliographyOrdering::ByPrefix => {
                items.sort_unstable_by(|(a, _), (b, _)| a.prefix.cmp(&b.prefix));
            }
            BibliographyOrdering::ByAuthor => {
                items.sort_unstable_by(|(a_ref, a_auths), (b_ref, b_auths)| {
                    author_title_ord_custom(
                        a_ref.entry,
                        b_ref.entry,
                        Some(a_auths),
                        Some(b_auths),
                    )
                });
            }
            _ => {}
        }

        items.into_iter().map(|(a, _)| a).collect()
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        let (bib, _) = self.bib_format.format(record.entry, record.disambiguation);
        DisplayReference {
            display: bib,
            entry: record.entry,
            prefix: record.prefix.clone().map(Into::into),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::style::Database;
    use crate::types::{Date, EntryType, Person, Title};
    use crate::{style::Citation, Entry};

    use super::ChicagoAuthorDate;

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
    fn C(entry: &Entry) -> Citation {
        Citation::new(entry, None)
    }

    #[allow(non_snake_case)]
    fn Cs(entries: &[Entry]) -> (Vec<Citation>, Database) {
        let cv = entries.iter().map(|e| C(e)).collect();

        let mut db = Database::new();

        for entry in entries {
            db.push(entry);
        }

        (cv, db)
    }

    #[test]
    fn simple() {
        let es = vec![date_author_entry("key", vec![A("Martin", "Haug")], 2018)];
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "Haug 2018"
        );
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
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "Kinsky 2018a; Kinsky 2018b; Hinsky 2018; Kinsky 2018c; Kinsky 2019a; Kinsky 2019b"
        );
    }

    #[test]
    fn author_initials() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Rich", "Doe")], 2011),
        ];
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "J. Doe 1967; R. Doe 2011"
        );
    }

    #[test]
    fn author_gn() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Janet", "Doe")], 2011),
        ];
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
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
        let mut formatter = ChicagoAuthorDate::default();
        formatter.et_al_limit = 3;
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
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
        let mut formatter = ChicagoAuthorDate::default();
        formatter.et_al_limit = 3;
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "Poquelin, M. Béjart, et al. 1648; Poquelin, A. Béjart, et al. 1662"
        );
    }

    #[test]
    fn no_author() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_date(Date::from_year(1999));
        e.set_title(Title::new("Third International Report on Reporting"));
        let es = vec![e];
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "Third International Report on Reporting 1999"
        );
    }

    #[test]
    fn no_date() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_authors(vec![A("John", "Doe")]);
        let es = vec![e];
        let mut formatter = ChicagoAuthorDate::default();
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut formatter, &citations).display.value,
            "Doe, n.d."
        );
    }
}
