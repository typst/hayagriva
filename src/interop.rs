//! Provides conversion methods for BibLaTeX.

use super::types::{Date, EntryType, Person, FormattableString, NumOrStr, PersonRole, Title, QualifiedUrl};
use super::Entry;
use biblatex::{Chunks, ChunksExt, Date as TexDate, DateValue, Edition, EditorType, Entry as TexEntry, EntryType as TexKind, Person as TexPerson};
use std::convert::Into;
use url::Url;

macro_rules! tex_kinds {
    ($self:expr, $mv_attr:expr, [$({$kind:pat, $new_kind:expr, $top_level:expr, $expand_mv:expr}),* $(,)*] $(,)*) => {
        match $self.entry_type {
            $(
                $kind => {
                    let mut item = Entry::new(&$self.key, $new_kind);
                    let top_level: Option<EntryType> = $top_level;
                    if let Some(kind) = top_level {
                        let mut tl_parent = Entry::new(&$self.key, kind);
                        if $expand_mv && $mv_attr {
                            let parent = Entry::new(&$self.key, kind);
                            tl_parent.add_parent(parent);
                        }
                        item.add_parent(tl_parent);
                    } else if $expand_mv && $mv_attr {
                        let parent = Entry::new(&$self.key, $new_kind);
                        item.add_parent(parent);
                    }

                    (item, top_level.is_some(), $expand_mv && $mv_attr)
                }
            )*
        }
    };
}

impl Into<Person> for TexPerson {
    fn into(self) -> Person {
        let given_name = if self.given_name.is_empty() {
            None
        } else {
            Some(self.given_name)
        };
        let prefix = if self.prefix.is_empty() {
            None
        } else {
            Some(self.prefix)
        };
        let suffix = if self.suffix.is_empty() {
            None
        } else {
            Some(self.suffix)
        };
        Person {
            name: self.name,
            given_name,
            prefix,
            suffix,
            alias: None,
        }
    }
}

impl Into<Date> for TexDate {
    fn into(self) -> Date {
        match self.value {
            DateValue::At(x) | DateValue::After(x) | DateValue::Before(x) => {
                Date { year: x.year, month: x.month, day: x.day }
            }
            DateValue::Between(_, x) => Date { year: x.year, month: x.month, day: x.day },
        }
    }
}

impl Into<FormattableString> for Chunks {
    fn into(self) -> FormattableString {
        FormattableString::new(self.format_verbatim(), None, Some(self.format_sentence()), false)
    }
}

impl Into<NumOrStr> for Chunks {
    fn into(self) -> NumOrStr {
        let verb = self.format_verbatim();
        match verb.parse() {
            Ok(i) => NumOrStr::Number(i),
            Err(_) => NumOrStr::Str(verb),
        }
    }
}

impl Into<NumOrStr> for Edition {
    fn into(self) -> NumOrStr {
        match self {
            Edition::Int(i) => NumOrStr::Number(i),
            Edition::Chunks(c) => NumOrStr::Str(c.format_sentence()),
        }
    }
}

fn ed_role(role: &EditorType) -> Option<PersonRole> {
    match role {
        EditorType::Editor => None,
        EditorType::Compiler => Some(PersonRole::Compiler),
        EditorType::Founder => Some(PersonRole::Founder),
        EditorType::Continuator => None,
        EditorType::Redactor => None,
        EditorType::Reviser => None,
        EditorType::Collaborator => Some(PersonRole::Collaborator),
        EditorType::Organizer => Some(PersonRole::Organizer),
    }
}

fn book(item: &mut Entry, parent: bool) -> Option<&mut Entry> {
    if parent {
        item.get_parents_mut().and_then(|p| p.get_mut(0))
    } else {
        None
    }
}

fn mv(item: &mut Entry, parent: bool, mv_parent: bool) -> Option<&mut Entry> {
    if parent && mv_parent {
        item.get_parents_mut().and_then(|p| p.get_mut(0)).unwrap().get_parents_mut().and_then(|p| p.get_mut(0))
    } else if mv_parent {
        item.get_parents_mut().and_then(|p| p.get_mut(0))
    } else {
        None
    }
}

// fn eldest(item: &mut Entry, parent: bool, mv_parent: bool) -> &mut Entry {
//     let mv = mv(item, parent, mv_parent);
//     if mv.is_some() {
//         return mv.unwrap();
//     }

//     drop(mv);

//     let book = book(item, parent);
//     if book.is_some() {
//         return book.unwrap();
//     }

//     drop(book);
//     item
// }

impl Into<Entry> for TexEntry {
    fn into(self) -> Entry {
        let mv_attributes = self.main_title().is_some() && self.volume().is_some();
        let (mut item, parent, mv_parent) = tex_kinds!(self, mv_attributes, [
            { TexKind::Article, EntryType::Article, Some(EntryType::Periodical), false },
            { TexKind::Book, EntryType::Book, None, true },
            { TexKind::Booklet, EntryType::Misc, None, false },
            { TexKind::InBook, EntryType::Chapter, Some(EntryType::Book), true },
            { TexKind::InCollection, EntryType::Anthos, Some(EntryType::Anthology), true },
            { TexKind::InProceedings, EntryType::Article, Some(EntryType::Proceedings), true },
            { TexKind::Manual, EntryType::Reference, None, false },
            { TexKind::MastersThesis, EntryType::Thesis, None, false },
            { TexKind::PhdThesis, EntryType::Thesis, None, false },
            { TexKind::Thesis, EntryType::Thesis, None, false },
            { TexKind::Misc, EntryType::Misc, None, false },
            { TexKind::Proceedings, EntryType::Proceedings, None, true },
            { TexKind::Report, EntryType::Report, None, true },
            { TexKind::TechReport, EntryType::Report, None, true },
            { TexKind::Unpublished, EntryType::Manuscript, None, true },
            { TexKind::MvBook, EntryType::Book, None, false },
            { TexKind::BookInBook, EntryType::Book, Some(EntryType::Book), true },
            { TexKind::SuppBook, EntryType::Misc, Some(EntryType::Book), true },
            { TexKind::Periodical, EntryType::Periodical, None, true },
            { TexKind::SuppPeriodical, EntryType::Misc, Some(EntryType::Periodical), true },
            { TexKind::Collection, EntryType::Anthology, None, true },
            { TexKind::SuppCollection, EntryType::Misc, Some(EntryType::Anthology), true },
            { TexKind::Reference, EntryType::Reference, None, true },
            { TexKind::MvReference, EntryType::Reference, None, false },
            { TexKind::InReference, EntryType::Entry, Some(EntryType::Reference), true },
            { TexKind::MvProceedings, EntryType::Proceedings, None, false },
            { TexKind::MvCollection, EntryType::Anthology, None, false },
            { TexKind::Patent, EntryType::Patent, None, false },
            { TexKind::Online, EntryType::Web, None, false },
            { TexKind::Software, EntryType::Misc, None, false },
            { TexKind::Dataset, EntryType::Repository, None, false },
            { TexKind::Set, EntryType::Misc, None, false },
            { TexKind::XData, EntryType::Misc, None, false },
            { TexKind::Unknown(_), EntryType::Misc, None, false },
        ]);

        if let Some(a) = self.author().map(|a| a.into_iter().map(Into::into).collect()) {
            item.set_authors(a);
        }

        let mut eds: Vec<Person> = vec![];
        let mut collaborators = vec![];
        for (editors, role) in self.editors() {
            let ptype = ed_role(&role);
            match ptype {
                None => eds.extend(editors.into_iter().map(Into::into)),
                Some(role) => collaborators.push((editors.into_iter().map(Into::into).collect(), role)),
            }
        }

        if !eds.is_empty() {
            item.set_editors(eds);
        }
        if !collaborators.is_empty() {
            item.set_affiliated_persons(collaborators);
        }

        if let Some(a) = self.holder().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Holder));
        }

        if let Some(parent) = book(&mut item, parent) {
            if let Some(a) = self.book_author().map(|a| a.into_iter().map(Into::into).collect()) {
                parent.set_authors(a);
            }
        }

        if let Some(a) = self.annotator().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Annotator));
        }

        if let Some(a) = self.commentator().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Commentator));
        }

        if let Some(a) = self.translator().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Translator));
        }

        // TODO: self.orig_language into item.set_language()

        if let Some(a) = self.afterword().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Afterword));
        }

        if let Some(a) = self.foreword().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Foreword));
        }

        if let Some(a) = self.introduction().map(|a| a.into_iter().map(Into::into).collect()) {
            item.add_affiliated_persons((a, PersonRole::Introduction));
        }

        if let Some(title) = self.title().map(|a| Title::from_fs(a.to_vec().into())) {
            item.set_title(title);
        }

        // NOTE: Ignoring subtitle and titleaddon for now

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(title) = self.main_title().map(|a| Title::from_fs(a.to_vec().into())) {
                parent.set_title(title);
            }
        }

        if let Some(parent) = book(&mut item, parent) {
            if self.entry_type == TexKind::Article {
                if let Some(title) = self.journal_title().map(|a| Title::from_fs(a.to_vec().into())) {
                    parent.set_title(title);
                }
            } else {
                if let Some(title) = self.book_title().map(|a| Title::from_fs(a.to_vec().into())) {
                    parent.set_title(title);
                }
            }
        }

        if matches!(self.entry_type, TexKind::Proceedings | TexKind::MvProceedings | TexKind::InProceedings) {
            if self.event_date().is_some() || self.eventtitle().is_some() || self.venue().is_some() {
                let mut conference = Entry::new(&self.key, EntryType::Conference);

                if let Some(event_date) = self.event_date().map(|d| d.into()) {
                    conference.set_date(event_date);
                }
                if let Some(title) = self.eventtitle().map(|a| Title::from_fs(a.to_vec().into())) {
                    conference.set_title(title);
                }
                if let Some(venue) = self.venue().map(|d| d.to_vec().into()) {
                    conference.set_location(venue);
                }

                item.add_parent(conference);
            }
        }

        if let Some(date) = self.date().map(|d| d.into()) {
            item.set_date(date);
        }

        if let Some(edition) = self.edition().map(|d| d.into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_edition(edition);
            } else {
                item.set_edition(edition);
            }
        }

        if matches!(self.entry_type, TexKind::Article | TexKind::Proceedings) {
            if let Some(issue) = self.issue().map(|d| d.to_vec().into()) {
                if let Some(parent) = book(&mut item, parent) {
                    parent.set_issue(issue);
                } else {
                    item.set_issue(issue);
                }
            }
            if let Some(ititle) = self.issue_title().map(|a| Title::from_fs(a.to_vec().into())) {
                if let Some(parent) = book(&mut item, parent) {
                    parent.set_title(ititle);
                } else {
                    item.set_title(ititle);
                }
            }
        }

        if let Some(number) = self.number().map(|d| d.to_vec().into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_issue(number);
            } else {
                item.set_issue(number);
            }
        }

        if let Some(volume) = self.volume() {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_volume(volume..volume);
            } else {
                item.set_volume(volume..volume);
            }
        }

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(volumes) = self.volumes() {
                    parent.set_total_volumes(volumes);
            }
        }

        if let Some(version) = self.version() {
            item.set_serial_number(version.to_vec().format_verbatim());
        }

        if let Some(doi) = self.doi() {
            item.set_doi(doi);
        }

        if let Some(isbn) = self.isbn() {
            item.set_isbn(isbn.to_vec().format_verbatim());
        }

        if let Some(issn) = self.issn() {
            item.set_issn(issn.to_vec().format_verbatim());
        }

        if let Some(sn) = self.isan().or_else(|| self.ismn()).or_else(|| self.iswc()) {
            if item.get_serial_number().is_none() {
                item.set_serial_number(sn.to_vec().format_verbatim());
            }
        }

        if let Some(url) = self.url().and_then(|s| Url::parse(&s).ok()) {
            let date = self.url_date().map(|d| d.into());
            item.set_url(QualifiedUrl { value: url, visit_date: date });
        }

        if let Some(location) = self.location().map(|d| d.to_vec().into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_location(location);
            } else {
                item.set_location(location);
            }
        }

        if let Some(publisher) = self.publisher().map(|d| {
            let mut format = FormattableString::new_empty(false, true, false);
            let comma = FormattableString { value: ", ".into(), sentence_case: Some(", ".into()), title_case: None, verbatim: false };
            for (i, item) in d.into_iter().enumerate() {
                if i != 0 {
                    format.extend(comma.clone());
                }
                format.extend(item.into());
            }
            format
        }) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_publisher(publisher);
            } else {
                item.set_publisher(publisher);
            }
        }

        if let Some(organization) = self.organization().map(|d| {
            let mut format = String::new();
            let comma = ", ";
            for (i, item) in d.into_iter().enumerate() {
                if i != 0 {
                    format += comma;
                }
                format += &item.format_verbatim();
            }
            format
        }) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_organization(organization);
            } else {
                item.set_organization(organization);
            }
        } else if let Some(organization) = self.institution().map(|d| d.to_vec().format_verbatim()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_organization(organization);
            } else {
                item.set_organization(organization);
            }
        }

        if let Some(note) = self.how_published().map(|d| d.to_vec().format_verbatim()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_note(note);
            } else {
                item.set_note(note);
            }
        }

        if let Some(pages) = self.pages().and_then(|p| p.get(0).cloned()) {
            item.set_page_range((pages.start as i64)..(pages.end as i64));
        }

        if let Some(ptotal) = self.page_total().and_then(|c| c.to_vec().format_verbatim().parse().ok()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_total_pages(ptotal);
            } else {
                item.set_total_pages(ptotal);
            }
        }

        if let Some(note) = self.annotation().or_else(|| self.addendum()).map(|d| d.to_vec().format_verbatim()) {
            if item.get_note().is_none() {
                item.set_note(note);
            }
        }

        if let Some(series) = self.series().map(|d| d.to_vec().into()) {
            let title = Title::from_fs(series);
            let mut new = Entry::new(&self.key, item.entry_type);
            new.set_title(title);
            if let Some(parent) = mv(&mut item, parent, mv_parent) {
                new.entry_type = parent.entry_type;
                parent.add_parent(new);
            } else if let Some(parent) = book(&mut item, parent) {
                new.entry_type = parent.entry_type;
                parent.add_parent(new);
            } else {
                item.add_parent(new);
            }
        }

        if let Some(chapter) = self.chapter().or_else(|| self.part()).map(|c| c.to_vec().into()) {
            let mut new = Entry::new(&self.key, EntryType::Chapter);
            new.set_title(Title::from_fs(chapter));
            let temp = item;
            new.set_parents(vec![temp]);
            item = new;
        }

        item
    }
}
