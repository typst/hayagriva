use String;
use super::{
    alph_designator, delegate_titled_entry, format_range, name_list, name_list_straight,
    sorted_bibliography, BibliographyOrdering, BibliographyStyle, Database,
    DisplayReference, DisplayString, Formatting, Record,
};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::SentenceCase;
use crate::types::{EntryType::*, EntryType, FmtOptionExt, NumOrStr, Person, PersonRole};
use crate::Entry;
use crate::types::NumOrStr::Str;


/// Bibliographies following ГОСТ guidance.
///
/// # Examples
/// - DeRidder J.L. The immediate prospects for the application of ontologies in digital libraries //
///   Knowledge Organization – 2007. – Vol. 34, No. 4 . – P. 227 – 246 .
/// - Белоозеров В.Н., Федосимов В.И. Место макротезауруса в лингвистическом обеспечении сети органов
///   научно-технической информации // Проблемы информационных систем – 1986 . – N 1. – С. 6 – 10 .
/// - U.S. National Library of Medicine. Fact sheet: Unfied Medical Language System/National Institutes of Health,
///   2006 – 2013. – URL: http://www.nlm.nih.gov/pubs/factsheets/umls.html (дата обращения 2009-12-09).
///
/// # Reference
/// See ГОСТ 7.1, ГОСТ 7.80, ГОСТ 7.82
#[derive(Default)]
pub struct Gost;

impl Gost {
    fn get_single_record<'a>(&self, record: &Record<'a>) -> DisplayReference<'a> {
        let entry = delegate_titled_entry(record.entry);

        match entry.entry_type {
            Thesis => self.get_textbook_record(entry),
            Article => self.get_journal_record(entry),
            Newspaper => self.get_newspaper_record(entry),
            Conference => self.get_conference_record(entry),
            Book => self.get_book_record(entry),
            Web => self.get_web_record(entry),
            _ => DisplayReference::new(entry, None, DisplayString::new()),
        }
    }

    fn get_textbook_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        reference += &self.get_authors_and_title(entry, true);

        if let Some(note) = entry.note() {
            reference += " - ";
            reference += note;
        }

        reference += &self.get_location(entry);

        reference += ": ";

        if let Some(publisher) = entry.publisher() {
            reference += &publisher.value;
        }

        if let Some(date) = entry.date() {
            reference += ", ";
            reference += &date.year.to_string();
            reference += ".";
        }

        reference += &self.get_pages(entry);

        if let Some(isbn) = entry.isbn() {
            reference += " - ISBN ";
            reference += isbn;
        }

        reference += ".";

        DisplayReference::new(
            entry,
            None,
            reference,
        )
    }

    fn get_journal_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        reference += &self.get_authors_and_title(entry, true);

        if let Some(doi) = entry.doi() {
            reference += " - DOI ";
            reference += doi;
        }

        reference += " // ";

        if let Some(publisher) = entry.publisher() {
            reference += &publisher.value;
        }

        if let Some(date) = entry.date() {
            reference += " - ";
            reference += &date.year.to_string();
            reference += ".";
        }

        if let Some(issue) = entry.issue() {
            reference += " - ";
            reference += &issue.to_string();
        }

        reference += &self.get_pages(entry);

        if let Some(url) = entry.url() {
            reference += " - URL: ";
            reference += &url.value.to_string();

            if let Some(date) = url.visit_date {
                if let (Some(mut month), Some(mut day)) = (date.month, date.day) {
                    reference += " (дата обращения: ";
                    let date = self.to_two_digits_str(day) + "." + &self.to_two_digits_str(month) + "." + &date.year.to_string();
                    reference += &date;
                    reference += ")";
                }
            }
        }

        reference += ".";

        return DisplayReference::new(entry, None, reference);
    }

    fn get_newspaper_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        reference += &self.get_authors_and_title(entry, false);

        reference += " // ";

        if let Some(publisher) = entry.publisher() {
            reference += &publisher.value;
            reference += ".";
        }

        if let Some(date) = entry.date() {
            reference += " - ";
            reference += &date.year.to_string();
            reference += ".";

            if let (Some(month), Some(day)) = (date.month, date.day) {
                reference += " - ";
                reference += &self.to_two_digits_str(day);
                reference += " ";
                reference += self.month_to_abbr(month);
            }
        }

        if let Some(issue) = entry.issue() {
            reference += " (";
            reference += &issue.to_string();
            reference += ")."
        }

        reference += &self.get_pages(entry);
        reference += ".";

        return DisplayReference::new(entry, None, reference);
    }

    fn get_conference_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        reference += &self.get_authors_and_title(entry, true);

        reference += " // ";

        if let Some(note) = entry.note() {
            reference += note;
        }

        if let Some(location) = entry.location() {
            reference += ". - ";
            reference += &location.value;
        }

        if let Some(publisher) = entry.publisher() {
            reference += ": ";
            reference += &publisher.value;
        }

        if let Some(date) = entry.date() {
            reference += ", ";
            reference += &date.year.to_string();
            reference += ".";
        }

        reference += &self.get_pages(entry);
        reference += ".";

        return DisplayReference::new(entry, None, reference);
    }

    fn get_book_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        let authors = self.get_authors(entry, true);

        if authors.2 <= 3 {
            reference += &authors.0.unwrap();
            reference += " ";
        }

        if let Some(title) = entry.title() {
            reference += &title.canonical.value;
        }

        if authors.2 > 1 {
            reference += " / ";
            reference += &authors.1.unwrap();
        }

        if let Some(note) = entry.note() {
            reference += " - ";
            reference += note;
        }

        reference += &self.get_location(entry);
        reference += ": ";

        if let Some(publisher) = entry.publisher() {
            reference += &publisher.value;
        }

        if let Some(date) = entry.date() {
            reference += ", ";
            reference += &date.year.to_string();
            reference += ".";
        }

        reference += &self.get_pages(entry);

        if let Some(isbn) = entry.isbn() {
            reference += ". - ISBN ";
            reference += isbn;
        }

        reference += ".";

        DisplayReference::new(
            entry,
            None,
            reference,
        )
    }

    fn get_web_record<'a>(&self, entry: &'a Entry) -> DisplayReference<'a> {
        let mut reference = DisplayString::new();
        if let Some(title) = entry.title() {
            reference += &title.canonical.value;
        }

        if let Some(url) = entry.url() {
            reference += " - URL: ";
            reference += &url.value.to_string();

            if let Some(date) = url.visit_date {
                if let (Some(mut month), Some(mut day)) = (date.month, date.day) {
                    reference += " (дата обращения: ";
                    let date = self.to_two_digits_str(day) + "." + &self.to_two_digits_str(month) + "." + &date.year.to_string();
                    reference += &date;
                    reference += ")";
                }
            }
        }

        DisplayReference::new(
            entry,
            None,
            reference,
        )
    }


    fn get_authors_and_title(&self, entry: &Entry, initials: bool) -> String {
        let authors = self.get_authors(entry, initials);

        let mut reference = String::new();
        if authors.2 != 0 {
            reference += &authors.0.unwrap();
        }

        if let Some(title) = entry.title() {
            reference += " ";
            reference += &title.canonical.value;
        }

        if !initials || authors.2 > 1 {
            reference += " / ";
            reference += &authors.1.unwrap();
        }

        reference
    }

    fn get_authors(&self, entry: &Entry, initials: bool) -> (Option<String>, Option<String>, usize) {
        if let Some(authors) = entry.authors() {
            let mut authors_joined = String::new();
            for author in authors {
                authors_joined += &author.name_first(true, false);
                authors_joined += ", ";
            }

            authors_joined.pop();
            authors_joined.pop();

            let mut authors_given_first_joined = String::new();
            for author in authors {
                if initials {
                    authors_given_first_joined += &author.given_first(true);
                } else {
                    if let Some(given_name) = &author.given_name {
                        authors_given_first_joined += &given_name.split(" ").collect::<Vec<&str>>()[0];
                        authors_given_first_joined += " ";
                    }
                    authors_given_first_joined += &author.name;

                }
                authors_given_first_joined += ", ";
            }

            authors_given_first_joined.pop();
            authors_given_first_joined.pop();

            (Some(authors_joined), Some(authors_given_first_joined), authors.len())
        } else {
            (None, None, 0)
        }
    }

    fn get_pages(&self, entry: &Entry) -> String {
        let mut reference = String::new();

        if let Some(page_range) = entry.page_range() {
            reference += " - ";
            if page_range.start == page_range.end {
                reference += &page_range.start.to_string();
                reference += " с";
            } else {
                reference += "С. ";
                reference += &page_range.start.to_string();
                reference += "-";
                reference += &page_range.end.to_string();
            }
        }

        reference
    }

    fn get_location(&self, entry: &Entry) -> String {
        let mut reference = String::new();

        if let Some(location) = entry.location() {
            reference += ". - ";
            let location_value = location.value.to_lowercase();

            if location_value == "москва" {
                reference += "М.";
            } else if location_value == "питер" || location_value == "санкт-петербург" {
                reference += "СПБ."
            } else {
                reference += &location.value;
            }
        }

        reference
    }

    fn to_two_digits_str(&self, mut number: u8) -> String {
        number += 1;

        if number < 10 {
            String::from("0") + &number.to_string()
        } else {
            number.to_string()
        }
    }

    fn month_to_abbr(&self, month: u8) -> &str {
        match month {
            0 => "янв.",
            1 => "фев.",
            2 => "марта.",
            3 => "апр.",
            4 => "мая.",
            5 => "июня.",
            6 => "июля.",
            7 => "авг.",
            8 => "сент.",
            9 => "окт.",
            10 => "нояб.",
            11 => "дек.",
            _ => "incorrect",
        }
    }
}

impl<'a> BibliographyStyle<'a> for Gost {
    fn bibliography(&self, db: &Database<'a>, ordering: BibliographyOrdering) -> Vec<DisplayReference<'a>> {
        let mut items = vec![];

        for record in db.records() {
            items.push(self.get_single_record(record));
        }

        items
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        self.get_single_record(record)
    }

    fn ordering(&self) -> BibliographyOrdering {
        BibliographyOrdering::ByInsertionOrder
    }
}
