//! Provides conversion methods for BibLaTeX.

use std::convert::TryFrom;

use biblatex as tex;
use tex::{
    Chunk, ChunksExt, DateValue, EditorType, PermissiveType, RetrievalError, Spanned,
    TypeError,
};
use url::Url;

use crate::lang::{Case, TitleCase};

use super::types::{
    Date, EntryType, FmtString, NumOrStr, Person, PersonRole, QualifiedUrl, Title,
};
use super::Entry;

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

impl From<&tex::Person> for Person {
    fn from(person: &tex::Person) -> Self {
        fn optional(part: &str) -> Option<String> {
            if !part.is_empty() {
                Some(part.to_string())
            } else {
                None
            }
        }

        Self {
            name: person.name.clone(),
            given_name: optional(&person.given_name),
            prefix: optional(&person.prefix),
            suffix: optional(&person.suffix),
            alias: None,
        }
    }
}

impl From<tex::Date> for Date {
    fn from(date: tex::Date) -> Self {
        match date.value {
            DateValue::At(x) | DateValue::After(x) | DateValue::Before(x) => {
                Date { year: x.year, month: x.month, day: x.day }
            }
            DateValue::Between(_, x) => Self { year: x.year, month: x.month, day: x.day },
        }
    }
}

impl From<&[Spanned<Chunk>]> for FmtString {
    fn from(chunks: &[Spanned<Chunk>]) -> Self {
        let mut fmt = Self::new(chunks.format_verbatim());
        let verbatim = chunks.iter().all(|chunk| match &chunk.v {
            Chunk::Normal(s) => s.chars().all(char::is_whitespace),
            _ => true,
        });

        fmt = fmt.verbatim(verbatim);
        if !verbatim {
            if chunks.iter().any(|chunk| matches!(chunk.v, Chunk::Verbatim(_))) {
                fmt = fmt.title_case(format_title_case(chunks));
            }
            fmt = fmt.sentence_case(chunks.format_sentence());
        }

        fmt
    }
}

fn format_title_case(chunks: &[Spanned<Chunk>]) -> String {
    let mut out = String::new();
    for chunk in chunks {
        match &chunk.v {
            Chunk::Normal(s) => out.push_str(
                &TitleCase {
                    trim_end: false,
                    trim_start: false,
                    always_capitalize_last_word: false,
                    ..Default::default()
                }
                .apply(s),
            ),
            Chunk::Verbatim(s) => out.push_str(s),
            Chunk::Math(s) => {
                out.push('$');
                out += s;
                out.push('$');
            }
        }
    }
    out
}

impl From<&[Spanned<Chunk>]> for NumOrStr {
    fn from(chunks: &[Spanned<Chunk>]) -> Self {
        let verb = chunks.format_verbatim();
        match verb.parse() {
            Ok(i) => NumOrStr::Number(i),
            Err(_) => NumOrStr::Str(verb),
        }
    }
}

impl From<&PermissiveType<i64>> for NumOrStr {
    fn from(edition: &PermissiveType<i64>) -> Self {
        match edition {
            PermissiveType::Typed(i) => Self::Number(*i),
            PermissiveType::Chunks(c) => Self::Str(c.format_verbatim()),
        }
    }
}

fn ed_role(role: EditorType) -> Option<PersonRole> {
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
        item.parents_mut().and_then(|p| p.get_mut(0))
    } else {
        None
    }
}

fn mv(item: &mut Entry, parent: bool, mv_parent: bool) -> Option<&mut Entry> {
    if parent && mv_parent {
        item.parents_mut()
            .and_then(|p| p.get_mut(0))
            .unwrap()
            .parents_mut()
            .and_then(|p| p.get_mut(0))
    } else if mv_parent {
        item.parents_mut().and_then(|p| p.get_mut(0))
    } else {
        None
    }
}

fn map_res<T>(result: Result<T, RetrievalError>) -> Result<Option<T>, TypeError> {
    match result {
        Ok(x) => Ok(Some(x)),
        Err(e) => match e {
            RetrievalError::Missing(_) => Ok(None),
            RetrievalError::TypeError(t) => Err(t),
        },
    }
}

impl TryFrom<&tex::Entry> for Entry {
    type Error = TypeError;

    fn try_from(entry: &tex::Entry) -> Result<Self, Self::Error> {
        let mv_attributes =
            !matches!(entry.main_title(), Err(RetrievalError::Missing(_)))
                && !matches!(entry.volume(), Err(RetrievalError::Missing(_)));
        let (mut item, parent, mv_parent) = tex_kinds!(entry, mv_attributes, [
            { tex::EntryType::Article, EntryType::Article, Some(EntryType::Periodical), false },
            { tex::EntryType::Book, EntryType::Book, None, true },
            { tex::EntryType::Booklet, EntryType::Misc, None, false },
            { tex::EntryType::InBook, EntryType::Chapter, Some(EntryType::Book), true },
            { tex::EntryType::InCollection, EntryType::Anthos, Some(EntryType::Anthology), true },
            { tex::EntryType::InProceedings, EntryType::Article, Some(EntryType::Proceedings), true },
            { tex::EntryType::Manual, EntryType::Reference, None, false },
            { tex::EntryType::MastersThesis, EntryType::Thesis, None, false },
            { tex::EntryType::PhdThesis, EntryType::Thesis, None, false },
            { tex::EntryType::Thesis, EntryType::Thesis, None, false },
            { tex::EntryType::Misc, EntryType::Misc, None, false },
            { tex::EntryType::Proceedings, EntryType::Proceedings, None, true },
            { tex::EntryType::Report, EntryType::Report, None, true },
            { tex::EntryType::TechReport, EntryType::Report, None, true },
            { tex::EntryType::Unpublished, EntryType::Manuscript, None, true },
            { tex::EntryType::MvBook, EntryType::Book, None, false },
            { tex::EntryType::BookInBook, EntryType::Book, Some(EntryType::Book), true },
            { tex::EntryType::SuppBook, EntryType::Misc, Some(EntryType::Book), true },
            { tex::EntryType::Periodical, EntryType::Periodical, None, true },
            { tex::EntryType::SuppPeriodical, EntryType::Misc, Some(EntryType::Periodical), true },
            { tex::EntryType::Collection, EntryType::Anthology, None, true },
            { tex::EntryType::SuppCollection, EntryType::Misc, Some(EntryType::Anthology), true },
            { tex::EntryType::Reference, EntryType::Reference, None, true },
            { tex::EntryType::MvReference, EntryType::Reference, None, false },
            { tex::EntryType::InReference, EntryType::Entry, Some(EntryType::Reference), true },
            { tex::EntryType::MvProceedings, EntryType::Proceedings, None, false },
            { tex::EntryType::MvCollection, EntryType::Anthology, None, false },
            { tex::EntryType::Patent, EntryType::Patent, None, false },
            { tex::EntryType::Online, EntryType::Web, None, false },
            { tex::EntryType::Software, EntryType::Misc, None, false },
            { tex::EntryType::Dataset, EntryType::Repository, None, false },
            { tex::EntryType::Set, EntryType::Misc, None, false },
            { tex::EntryType::XData, EntryType::Misc, None, false },
            { tex::EntryType::Unknown(_), EntryType::Misc, None, false },
        ]);

        if let Ok(a) = entry.author().map(|a| a.iter().map(Into::into).collect()) {
            item.set_authors(a);
        }

        let mut eds: Vec<Person> = vec![];
        let mut collaborators = vec![];
        for (editors, role) in entry.editors()? {
            let ptype = ed_role(role);
            match ptype {
                None => eds.extend(editors.iter().map(Into::into)),
                Some(role) => {
                    collaborators.push((editors.iter().map(Into::into).collect(), role))
                }
            }
        }

        if !eds.is_empty() {
            item.set_editors(eds);
        }
        if !collaborators.is_empty() {
            item.set_affiliated_persons(collaborators);
        }

        if let Some(a) =
            map_res(entry.holder())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Holder));
        }

        if let Some(parent) = book(&mut item, parent) {
            if let Some(a) =
                map_res(entry.book_author())?.map(|a| a.iter().map(Into::into).collect())
            {
                parent.set_authors(a);
            }
        }

        if let Some(a) =
            map_res(entry.annotator())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Annotator));
        }

        if let Some(a) =
            map_res(entry.commentator())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Commentator));
        }

        if let Some(a) =
            map_res(entry.translator())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Translator));
        }

        // TODO: entry.orig_language into item.set_language()

        if let Some(a) =
            map_res(entry.afterword())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Afterword));
        }

        if let Some(a) =
            map_res(entry.foreword())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Foreword));
        }

        if let Some(a) =
            map_res(entry.introduction())?.map(|a| a.iter().map(Into::into).collect())
        {
            item.add_affiliated_persons((a, PersonRole::Introduction));
        }

        if let Some(title) = map_res(entry.title())?.map(Title::new) {
            item.set_title(title);
        }

        // NOTE: Ignoring subtitle and titleaddon for now

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(title) = map_res(entry.main_title())?.map(Title::new) {
                parent.set_title(title);
            }
        }

        if let Some(parent) = book(&mut item, parent) {
            if entry.entry_type == tex::EntryType::Article {
                if let Some(title) = map_res(entry.journal_title())?.map(Title::new) {
                    parent.set_title(title);
                }
            } else if let Some(title) = map_res(entry.book_title())?.map(Title::new) {
                parent.set_title(title);
            }
        }

        if matches!(
            entry.entry_type,
            tex::EntryType::Proceedings
                | tex::EntryType::MvProceedings
                | tex::EntryType::InProceedings
        ) && (map_res(entry.event_date())?.is_some()
            || map_res(entry.eventtitle())?.is_some()
            || map_res(entry.venue())?.is_some())
        {
            let mut conference = Entry::new(&entry.key, EntryType::Conference);

            if let Some(event_date) = map_res(entry.event_date())?
                .and_then(|d| match d {
                    PermissiveType::Typed(d) => Some(d),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| d.into())
            {
                conference.set_date(event_date);
            }
            if let Some(title) = map_res(entry.eventtitle())?.map(Title::new) {
                conference.set_title(title);
            }
            if let Some(venue) = map_res(entry.venue())?.map(|d| d.into()) {
                conference.set_location(venue);
            }

            item.add_parent(conference);
        }

        if let Some(date) = map_res(entry.date())?
            .and_then(|d| match d {
                PermissiveType::Typed(d) => Some(d),
                PermissiveType::Chunks(_) => None,
            })
            .map(|d| d.into())
        {
            item.set_date(date);
        }

        if let Some(edition) = map_res(entry.edition())?.map(|d| (&d).into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_edition(edition);
            } else {
                item.set_edition(edition);
            }
        }

        if matches!(
            entry.entry_type,
            tex::EntryType::Article | tex::EntryType::Proceedings
        ) {
            if let Some(issue) = map_res(entry.issue())?.map(|d| d.into()) {
                if let Some(parent) = book(&mut item, parent) {
                    parent.set_issue(issue);
                } else {
                    item.set_issue(issue);
                }
            }
            if let Some(ititle) = map_res(entry.issue_title())?.map(Title::new) {
                if let Some(parent) = book(&mut item, parent) {
                    parent.set_title(ititle);
                } else {
                    item.set_title(ititle);
                }
            }
        }

        if let Some(number) = map_res(entry.number())?.map(|d| d.into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_issue(number);
            } else {
                item.set_issue(number);
            }
        }

        if let Some(PermissiveType::Typed(volume)) = map_res(entry.volume())? {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_volume(volume..volume);
            } else {
                item.set_volume(volume..volume);
            }
        }

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(volumes) = map_res(entry.volumes())? {
                parent.set_volume_total(volumes);
            }
        }

        if let Some(version) = map_res(entry.version())? {
            item.set_serial_number(version.format_verbatim());
        }

        if let Some(doi) = map_res(entry.doi())? {
            item.set_doi(doi);
        }

        if let Some(isbn) = map_res(entry.isbn())? {
            item.set_isbn(isbn.format_verbatim());
        }

        if let Some(issn) = map_res(entry.issn())? {
            item.set_issn(issn.format_verbatim());
        }

        if let Some(sn) = map_res(entry.isan())?
            .or_else(|| entry.ismn().ok())
            .or_else(|| entry.iswc().ok())
        {
            if item.serial_number().is_none() {
                item.set_serial_number(sn.format_verbatim());
            }
        }

        if let Some(url) = map_res(entry.url())?.and_then(|s| Url::parse(&s).ok()) {
            let date = map_res(entry.url_date())?
                .and_then(|d| match d {
                    PermissiveType::Typed(d) => Some(d),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| d.into());
            item.set_url(QualifiedUrl { value: url, visit_date: date });
        }

        if let Some(location) = map_res(entry.location())?.map(|d| d.into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_location(location);
            } else {
                item.set_location(location);
            }
        }

        if let Some(publisher) = map_res(entry.publisher())?.map(|d| {
            let mut format = FmtString::new_empty(false, true, false);
            let comma = FmtString {
                value: ", ".into(),
                sentence_case: Some(", ".into()),
                title_case: None,
                verbatim: false,
            };
            for (i, item) in d.iter().enumerate() {
                if i != 0 {
                    format.extend(comma.clone());
                }
                format.extend(item.as_slice().into());
            }
            format
        }) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_publisher(publisher);
            } else {
                item.set_publisher(publisher);
            }
        }

        if let Some(organization) = map_res(entry.organization())?.map(|d| {
            let mut format = String::new();
            let comma = ", ";
            for (i, item) in d.iter().enumerate() {
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
        } else if let Some(organization) =
            map_res(entry.institution())?.map(|d| d.format_verbatim())
        {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_organization(organization);
            } else {
                item.set_organization(organization);
            }
        }

        if let Some(note) = map_res(entry.how_published())?.map(|d| d.format_verbatim()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_note(note);
            } else {
                item.set_note(note);
            }
        }

        if let Some(pages) = map_res(entry.pages())?
            .and_then(|pages| match pages {
                PermissiveType::Typed(p) => Some(p),
                PermissiveType::Chunks(_) => None,
            })
            .and_then(|p| p.get(0).cloned())
        {
            item.set_page_range((pages.start as i64)..(pages.end as i64));
        }

        if let Some(ptotal) =
            map_res(entry.page_total())?.and_then(|c| c.format_verbatim().parse().ok())
        {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_page_total(ptotal);
            } else {
                item.set_page_total(ptotal);
            }
        }

        if let Some(note) = map_res(entry.annotation())?
            .or_else(|| entry.addendum().ok())
            .map(|d| d.format_verbatim())
        {
            if item.note().is_none() {
                item.set_note(note);
            }
        }

        if let Some(series) = map_res(entry.series())? {
            let title = Title::new(series);
            let mut new = Entry::new(&entry.key, item.entry_type);
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

        if let Some(chapter) =
            map_res(entry.chapter())?.or_else(|| map_res(entry.part()).ok().flatten())
        {
            let mut new = Entry::new(&entry.key, EntryType::Chapter);
            new.set_title(Title::new(chapter));
            let temp = item;
            new.set_parents(vec![temp]);
            item = new;
        }

        Ok(item)
    }
}
