use String;

use crate::Entry;
use crate::style::sorted_bibliography;
use crate::types::{Date, EntryType::*, Person, PersonRole};

use super::{
    BibliographyOrdering, BibliographyStyle, Database,
    DisplayReference, DisplayString, Record,
};

/// Bibliographies following ГОСТ guidance.
///
/// # Examples
/// - DeRidder J.L. The immediate prospects for the application of ontologies in digital libraries //
///   Knowledge Organization – 2007. – Т. 34, No. 4 . – P. 227 – 246 .
/// - Белоозеров В.Н., Федосимов В.И. Место макротезауруса в лингвистическом обеспечении сети органов
///   научно-технической информации // Проблемы информационных систем – 1986 . – N 1. – С. 6 – 10 .
/// - U.S. National Library of Medicine. Fact sheet: Unfied Medical Language System/National Institutes of Health,
///   2006 – 2013. – URL: http://www.nlm.nih.gov/pubs/factsheets/umls.html (дата обращения 2009-12-09).
///
/// # Reference
/// See ГОСТ 7.1, ГОСТ 7.80, ГОСТ 7.82
#[derive(Default)]
pub struct Gost;

const DOT_DASH_SEPARATOR: &str = ". - ";

fn join_with_and(list: &[Person]) -> String {
    if list.len() == 1 {
        list.first().unwrap().given_first(true)
    } else {
        let mut res = list[0..list.len() - 1]
            .iter()
            .map(|p| p.given_first(true))
            .collect::<Vec<String>>()
            .join(", ");

        res += " и ";
        res + &list.last().unwrap().given_first(true)
    }
}

fn to_two_digits_str(mut number: u8) -> String {
    number += 1;

    if number < 10 {
        String::from("0") + &number.to_string()
    } else {
        number.to_string()
    }
}

trait StringExt {
    fn add_separator(&mut self, separator: &str);
}

impl StringExt for String {
    fn add_separator(&mut self, separator: &str) {
        if self.is_empty() {
            return;
        }

        if separator.starts_with(self.chars().last().unwrap()) {
            *self += &separator[1..separator.len()]
        } else {
            *self += separator;
        }
    }
}

trait DateExt {
    fn as_numbers(&self) -> String;
}

impl DateExt for Date {
    fn as_numbers(&self) -> String {
        let mut res = String::new();

        if let Some(day) = self.day {
            res += &to_two_digits_str(day);
        }

        if let Some(month) = self.month {
            if !res.is_empty() {
                res += ".";
            }

            res += &to_two_digits_str(month);
        }

        if !res.is_empty() {
            res += ".";
        }

        res + &self.year.to_string()
    }
}

trait EntryExt {
    fn is_web(&self) -> bool;

    fn authors_count(&self) -> usize;

    fn get_first_author(&self) -> String;

    fn get_all_authors(&self) -> String;

    fn get_by_role(&self, role: PersonRole) -> Option<Vec<Person>>;

    fn get_affiliated_persons(&self) -> Option<String>;

    fn get_responsibility(&self) -> Option<String>;

    fn get_release_data(&self) -> Option<String>;

    fn get_location(&self) -> Option<String>;

    fn get_pages(&self) -> Option<String>;

    fn get_volumes(&self) -> Option<String>;

    fn get_url(&self) -> Option<String>;

    fn get_contributors(&self) -> Vec<Person>;
}

impl EntryExt for Entry {
    fn is_web(&self) -> bool {
        [Web, Blog, Tweet].contains(&self.entry_type)
    }

    fn authors_count(&self) -> usize {
        if let Some(authors) = self.authors() {
            authors.len()
        } else {
            0
        }
    }

    fn get_first_author(&self) -> String {
        self.authors().unwrap().first().unwrap().name_first(true, false)
    }

    fn get_all_authors(&self) -> String {
        self
            .authors()
            .unwrap()
            .iter()
            .map(|a| a.given_first(true))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn get_by_role(&self, role: PersonRole) -> Option<Vec<Person>>
    {
        if let Some(affiliated) = self.affiliated_persons() {
            affiliated
                .iter()
                .find(|(_, r)| *r == role)
                .map(|(p, _)| p.clone())
        } else {
            None
        }
    }

    fn get_affiliated_persons(&self) -> Option<String> {
        let mut res = String::new();

        if let Some(editors) = self.editors() {
            res += "ред. ";
            res += &join_with_and(&editors[..]);
        }

        if let Some(compilers) = self.get_by_role(PersonRole::Compiler) {
            res.add_separator("; ");
            res += "составит. ";
            res += &join_with_and(&compilers[..]);
        }

        if let Some(translators) = self.get_by_role(PersonRole::Translator) {
            res.add_separator("; ");
            res += "перевод. ";
            res += &join_with_and(&translators[..]);
        }

        if let Some(affiliated) = self.affiliated_persons() {
            let affiliated_joined = affiliated
                .iter()
                .filter(|(_, r)| {
                    match r {
                        PersonRole::Unknown(_) => true,
                        _ => false,
                    }
                })
                .map(|(persons, role)| {
                    if let PersonRole::Unknown(descr) = role {
                        descr.clone() + " " + &join_with_and(persons)
                    } else {
                        join_with_and(persons)
                    }
                })
                .collect::<Vec<String>>()
                .join(", ");

            if !affiliated_joined.is_empty() {
                res.add_separator("; ");
                res += &affiliated_joined;
            }
        }

        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }

    fn get_responsibility(&self) -> Option<String> {
        let authors_count = self.authors_count();
        let organization = self.organization();
        let affiliated = self.get_affiliated_persons();

        if authors_count == 0 && organization.is_none() && affiliated.is_none() {
            return None;
        }

        let mut res = String::new();

        if self.entry_type == Proceedings {
            if authors_count > 0 {
                res += &self.get_all_authors();
            }

            if let Some(organization) = organization {
                res.add_separator("; ");
                res += organization;
            }
        } else {
            if let Some(organization) = organization {
                res += organization;
            }

            if authors_count > 0 {
                res.add_separator("; ");
                res += &self.get_all_authors();
            }
        }

        if let Some(affiliated) = affiliated {
            res.add_separator("; ");
            res += &affiliated;
        }

        Some(res)
    }

    fn get_release_data(&self) -> Option<String> {
        if self.location().is_none() && self.publisher().is_none() && self.date().is_none() {
            return None;
        }

        let mut res = String::new();

        if let Some(location) = self.get_location() {
            res += &location;
        }

        if let Some(publisher) = self.publisher() {
            res.add_separator(": ");
            res += &publisher.value;
        }

        if let Some(date) = self.date() {
            res.add_separator(", ");
            res += &date.year.to_string();
        }

        Some(res)
    }

    fn get_location(&self) -> Option<String> {
        if let Some(location) = self.location() {
            let location_value = location.value.to_lowercase();

            if location_value == "москва" {
                Some(String::from("М."))
            } else if location_value == "питер" || location_value == "санкт-петербург" {
                Some(String::from("СПБ."))
            } else {
                Some(location.value.clone())
            }
        } else {
            None
        }
    }

    fn get_pages(&self) -> Option<String> {
        if self.page_range().is_none() {
            return None;
        }

        let mut reference = String::new();

        if let Some(page_range) = self.page_range() {
            if page_range.start == page_range.end {
                reference += &page_range.start.to_string();
                reference += " с.";
            } else {
                reference += "С. ";
                reference += &page_range.start.to_string();
                reference += "-";
                reference += &page_range.end.to_string();
            }
        }

        Some(reference)
    }

    fn get_volumes(&self) -> Option<String> {
        if self.volume().is_none() && self.volume_total().is_none() {
            return None;
        }

        let mut res = String::new();
        if let Some(volume_total) = self.volume_total() {
            res += "в ";
            res += &volume_total.to_string();
            res += " т.";
        }

        if let Some(volume) = self.volume() {
            if !res.is_empty() {
                res += " ";
            }

            res += "Т. ";
            res += &volume.start.to_string();
            res.add_separator(".");
        }

        Some(res)
    }

    fn get_url(&self) -> Option<String> {
        if let Some(url) = self.url() {
            let mut res = String::from("URL: ");
            res += url.value.as_str();

            if let Some(date) = url.visit_date {
                res += " (дата обращения: ";
                res += &date.as_numbers();
                res += ")";
            }

            Some(res)
        } else {
            None
        }
    }

    fn get_contributors(&self) -> Vec<Person> {
        let mut contributors = vec![];

        if let Some(authors) = self.authors() {
            contributors.extend(authors.iter().cloned());
        }

        if let Some(editors) = self.editors() {
            contributors.extend(editors.iter().cloned())
        }

        if let Some(affiliated_persons) = self.affiliated_persons() {
            for (persons, _) in affiliated_persons {
                contributors.extend(persons.iter().cloned())
            }
        }

        contributors
    }
}

impl Gost {
    fn get_single_record<'a>(&self, record: &Record<'a>) -> (DisplayReference<'a>, Vec<Person>) {
        let entry = record.entry;

        (
            DisplayReference::new(
                entry,
                record.prefix.clone().map(Into::into),
                DisplayString::from(self.get_entry_representation(entry)),
            ),
            entry.get_contributors()
        )
    }

    fn get_entry_representation(&self, entry: &Entry) -> String
    {
        let mut reference = String::new();

        if entry.authors_count() > 0 && entry.authors_count() < 4 {
            reference += &entry.get_first_author();
            reference.add_separator(". ");
        }

        reference += &entry.title().unwrap().canonical.value;

        if entry.is_web() {
            reference.add_separator(" ");
            reference += "[Электронный ресурс]";
        }

        if let Some(note) = entry.note() {
            reference.add_separator(" : ");
            reference += note;
        }

        if let Some(volumes) = entry.get_volumes() {
            reference.add_separator(": ");
            reference += &volumes;
        }

        if let Some(responsibility) = entry.get_responsibility() {
            reference.add_separator(" / ");
            reference += &responsibility;
        }

        if let Some(edition) = entry.edition() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += &edition.to_string();
        }

        if let Some(issue_data) = entry.get_release_data() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += &issue_data;
        }

        if let Some(issue) = entry.issue() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += &issue.to_string();
        }

        if let Some(parents) = entry.parents() {
            reference.add_separator(" // ");
            reference += &parents
                .iter()
                .map(|p| self.get_entry_representation(p))
                .collect::<Vec<String>>()
                .join("; ");
        }

        if let Some(pages) = entry.get_pages() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += &pages;
        }

        if let Some(isbn) = entry.isbn() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += "ISBN ";
            reference += isbn;
        }

        if let Some(issn) = entry.issn() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += "ISSN ";
            reference += issn;
        }

        if let Some(doi) = entry.doi() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += "DOI ";
            reference += doi;
        }

        if let Some(url) = entry.get_url() {
            reference.add_separator(DOT_DASH_SEPARATOR);
            reference += &url;
        }

        reference.add_separator(".");

        reference
    }
}

impl<'a> BibliographyStyle<'a> for Gost {
    fn bibliography(&self, db: &Database<'a>, ordering: BibliographyOrdering)
                    -> Vec<DisplayReference<'a>> {
        let mut items = vec![];

        for record in db.records() {
            items.push(self.get_single_record(record));
        }

        sorted_bibliography(items, ordering)
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        self.get_single_record(record).0
    }

    fn ordering(&self) -> BibliographyOrdering {
        BibliographyOrdering::ByInsertionOrder
    }
}
