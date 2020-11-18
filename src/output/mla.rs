//! Style for entries in the "Works Cited" listing as of the 8th edition
//! of the MLA Handbook.

use super::{name_list, BibliographyGenerator, DisplayString};
use crate::lang::TitleCase;
use crate::selectors::{Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{Person, PersonRole};
use crate::{attrs, sel, Entry};

/// Generates the "Works Cited" entries
pub struct MlaBibliographyGenerator<'s> {
    tc_formatter: TitleCase,
    prev_entry: Option<&'s Entry>,
}

impl<'s> MlaBibliographyGenerator<'s> {
    /// Create a new MLA Bibliography Generator with default values.
    pub fn new() -> Self {
        let mut tc_formatter = TitleCase::new();
        tc_formatter.always_capitalize_last_word = false;

        Self { tc_formatter, prev_entry: None }
    }

    /// Indicates whether a kind of work is its own container.
    fn own_container(entry: &Entry) -> bool {
        let kinds = [
            Book,
            Report,
            Thesis,
            Newspaper,
            Periodical,
            Proceedings,
            Book,
            Blog,
            Reference,
            Conference,
            Anthology,
            Thread,
            Exhibition,
        ];

        kinds.contains(&entry.entry_type)
            || sel!(Wc() => Neg(Wc())).apply(entry).is_some()
    }

    fn and_list(&self, names: Vec<String>) -> String {
        let name_len = names.len();
        let mut res = String::new();
        let threshold = 3;

        for (index, name) in names.into_iter().enumerate() {
            res += &name;

            if (index as i32) <= name_len as i32 - 2 {
                res += ", ";
            }
            if name_len >= threshold {
                break;
            }
            if (index as i32) == name_len as i32 - 2 {
                res += "and ";
            }
        }

        if name_len >= threshold {
            res += "et al."
        }

        res
    }

    fn get_main_contributors(&self, entry: &Entry) -> Option<Vec<Person>> {
        entry
            .get_authors_fallible()
            .map(|a| a.to_vec())
            .or_else(|| {
                entry
                    .get_affiliated_persons()
                    .ok()
                    .and_then(|a| if a.len() == 1 { Some(a[0].0.clone()) } else { None })
            })
            .or_else(|| entry.get_editors().ok().map(|a| a.to_vec()))
    }

    fn name_list(&self, persons: &[Person], tweet_entry: Option<&Entry>) -> Vec<String> {
        let mut names = vec![];

        for (i, author) in persons.iter().enumerate() {
            let alias = tweet_entry
                .and_then(|entry| entry.get_twitter_handle(i))
                .or_else(|| author.alias.clone());

            names.push(if let Some(alias) = alias {
                format!("{} ({})", alias, author.get_given_name_initials_first(false))
            } else {
                author.get_name_first(false)
            });
        }

        names
    }

    /// Prints the names of the main creators of the item.
    fn get_author(&self, entry: &Entry) -> String {
        let main_contribs = self.get_main_contributors(entry);
        let (mut res, previous) = if main_contribs.is_some() && Some(main_contribs)
            == self.prev_entry.map(|s| self.get_main_contributors(s))
        {
            ("---".to_string(), true)
        } else {
            (String::new(), false)
        };
        res += &if let Some(authors) = entry.get_authors_fallible() {
            if !previous && entry.entry_type == Tweet {
                self.and_list(self.name_list(authors, Some(entry)))
            } else if !previous {
                self.and_list(self.name_list(authors, None))
            } else {
                String::new()
            }
        } else if let Ok(affs) = entry.get_affiliated_persons() {
            let mut res = String::new();
            for (persons, role) in affs.iter() {
                let plural = persons.len() > 1;
                let desc = match role {
                    PersonRole::Translator if plural => "translators",
                    PersonRole::Translator => "translator",
                    PersonRole::Annotator if plural => "annotators",
                    PersonRole::Annotator => "annotator",
                    PersonRole::Commentator if plural => "commentators",
                    PersonRole::Commentator => "commentator",
                    PersonRole::Holder if plural => "holders",
                    PersonRole::Holder => "holder",
                    PersonRole::Compiler if plural => "compilers",
                    PersonRole::Compiler => "compiler",
                    PersonRole::Founder if plural => "founders",
                    PersonRole::Founder => "founder",
                    PersonRole::Collaborator if plural => "collaborators",
                    PersonRole::Collaborator => "collaborator",
                    PersonRole::Organizer if plural => "organizers",
                    PersonRole::Organizer => "organizer",
                    PersonRole::CastMember if plural => "performers",
                    PersonRole::CastMember => "performer",
                    PersonRole::Composer if plural => "composers",
                    PersonRole::Composer => "composer",
                    PersonRole::Producer if plural => "poducers",
                    PersonRole::Producer => "poducer",
                    PersonRole::ExecutiveProducer if plural => "executive producers",
                    PersonRole::ExecutiveProducer => "executive producer",
                    PersonRole::Writer if plural => "writers",
                    PersonRole::Writer => "writer",
                    PersonRole::Cinematography if plural => "cinematographers",
                    PersonRole::Cinematography => "cinematographer",
                    PersonRole::Director if plural => "directors",
                    PersonRole::Director => "director",
                    PersonRole::Illustrator if plural => "illustrators",
                    PersonRole::Illustrator => "illustrator",
                    _ => "",
                };

                if desc.is_empty() || persons.is_empty() {
                    continue;
                }

                if !res.is_empty() {
                    res += "; ";
                }

                if !previous {
                    res += &self.and_list(self.name_list(persons, None));
                }
                res += ", ";
                res += desc;
            }
            res
        } else if let Ok(eds) = entry.get_editors() {
            let plural = eds.len() > 1;
            let mut res = if !previous {
                self.and_list(self.name_list(eds, None))
            } else {
                String::new()
            };

            res += ", editor";
            if plural {
                res.push('s');
            }

            res
        } else {
            String::new()
        };

        if !res.is_empty() && res.chars().last().unwrap_or('a') != '.' {
            res.push('.');
        }
        res
    }
}

impl<'s> BibliographyGenerator<'s> for MlaBibliographyGenerator<'s> {
    fn get_reference(&mut self, entry: &'s Entry) -> DisplayString {
        let res = DisplayString::from_string(self.get_author(entry));
        self.prev_entry = Some(entry);
        res
    }
}
