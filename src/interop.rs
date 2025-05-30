//! Provides conversion methods for BibLaTeX.

use std::convert::TryFrom;

use biblatex as tex;
use tex::{
    Chunk, ChunksExt, DateValue, EditorType, PermissiveType, RetrievalError, Spanned,
    TypeError,
};

use url::Url;

use super::types::*;
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
        let approximate = date.uncertain || date.approximate;

        match date.value {
            DateValue::At(x) | DateValue::After(x) | DateValue::Before(x) => Date {
                year: x.year,
                month: x.month,
                day: x.day,
                approximate,
            },
            DateValue::Between(_, x) => Self {
                year: x.year,
                month: x.month,
                day: x.day,
                approximate,
            },
        }
    }
}

impl From<&[Spanned<Chunk>]> for ChunkedString {
    fn from(chunks: &[Spanned<Chunk>]) -> Self {
        let mut res = Self::new();
        for chunk in chunks {
            match &chunk.v {
                Chunk::Normal(s) => res.push_str(s, ChunkKind::Normal),
                Chunk::Verbatim(s) => res.push_str(s, ChunkKind::Verbatim),
                Chunk::Math(s) => res.push_str(s, ChunkKind::Math),
            }
        }
        res
    }
}

impl From<&[Spanned<Chunk>]> for FormatString {
    fn from(chunks: &[Spanned<Chunk>]) -> Self {
        Self { value: chunks.into(), short: None }
    }
}

impl From<&[Spanned<Chunk>]> for MaybeTyped<Numeric> {
    fn from(chunks: &[Spanned<Chunk>]) -> Self {
        let verb = chunks.format_verbatim();
        MaybeTyped::infallible_from_str(&verb)
    }
}

impl From<&PermissiveType<i64>> for MaybeTyped<Numeric> {
    fn from(edition: &PermissiveType<i64>) -> Self {
        match edition {
            PermissiveType::Typed(i) => Self::Typed(Numeric::new(*i as i32)),
            PermissiveType::Chunks(c) => Self::infallible_from_str(&c.format_verbatim()),
        }
    }
}

fn ed_role(role: EditorType, entry_type: &tex::EntryType) -> Option<PersonRole> {
    match role {
        EditorType::Editor => None,
        EditorType::Compiler => Some(PersonRole::Compiler),
        EditorType::Founder => Some(PersonRole::Founder),
        EditorType::Continuator => None,
        EditorType::Redactor => None,
        EditorType::Reviser => None,
        EditorType::Collaborator => Some(PersonRole::Collaborator),
        EditorType::Organizer => Some(PersonRole::Organizer),
        EditorType::Director => Some(PersonRole::Director),
        EditorType::Unknown(role) => {
            let other_entry_type = if let tex::EntryType::Unknown(t) = entry_type {
                Some(t.to_ascii_lowercase())
            } else {
                None
            };

            match (role.to_ascii_lowercase().as_str(), other_entry_type.as_deref()) {
                // See p. 26 of the biblatex-chicago manual and biblatex-apa
                ("producer", _) => Some(PersonRole::Producer),
                // The pervasive Zotero plugin zotero-better-biblatex produces this.
                ("scriptwriter", _) => Some(PersonRole::Writer),
                // The biblatex-apa style expects `writer` for videos.
                ("writer", Some("video")) => Some(PersonRole::Writer),
                // See p. 26 of the biblatex-chicago manual
                ("none", Some("video") | Some("music")) => Some(PersonRole::CastMember),
                _ => Some(PersonRole::Unknown(role)),
            }
        }
    }
}

fn book(item: &mut Entry, parent: bool) -> Option<&mut Entry> {
    if parent {
        item.parents_mut().get_mut(0)
    } else {
        None
    }
}

fn mv(item: &mut Entry, parent: bool, mv_parent: bool) -> Option<&mut Entry> {
    if parent && mv_parent {
        item.parents_mut().get_mut(0).unwrap().parents_mut().get_mut(0)
    } else if mv_parent {
        item.parents_mut().get_mut(0)
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
            let ptype = ed_role(role, &entry.entry_type);
            match ptype {
                None => eds.extend(editors.iter().map(Into::into)),
                Some(role) => collaborators.push(PersonsWithRoles::new(
                    editors.iter().map(Into::into).collect(),
                    role,
                )),
            }
        }

        if !eds.is_empty() {
            item.set_editors(eds);
        }
        if !collaborators.is_empty() {
            item.set_affiliated(collaborators);
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

        // Take the first language or the langid.
        let lang_res = entry.language();
        let langid_res = entry.langid().ok();
        // If we cannot parse the language, ignore it
        if let Some(l) = map_res(lang_res)?
            .as_ref()
            .and_then(|l| l.first())
            .or(langid_res.as_ref())
        {
            match l {
                PermissiveType::Typed(lang) => {
                    item.set_language((*lang).into());
                }
                PermissiveType::Chunks(spanneds) => {
                    let chunked: ChunkedString = (spanneds as &[Spanned<Chunk>]).into();
                    if let Ok(l) = chunked.to_str().parse() {
                        item.set_language(l);
                    }
                }
            }
        }

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

        if let Some(title) = map_res(entry.title())?.map(Into::into) {
            if let Some(short_title) = map_res(entry.short_title())?.map(Into::into) {
                item.set_title(FormatString {
                    value: title,
                    short: Some(Box::new(short_title)),
                });
            } else {
                item.set_title(FormatString { value: title, short: None });
            }
        }

        // NOTE: Ignoring subtitle and titleaddon for now

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(title) = map_res(entry.main_title())?.map(Into::into) {
                parent.set_title(title);
            }
        }

        if let Some(parent) = book(&mut item, parent) {
            if entry.entry_type == tex::EntryType::Article {
                if let Some(title) = map_res(entry.journal_title())?.map(Into::into) {
                    parent.set_title(title);
                }
            } else if let Some(title) = map_res(entry.book_title())?.map(Into::into) {
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
            if let Some(title) = map_res(entry.eventtitle())?.map(Into::into) {
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
            if let Some(ititle) = map_res(entry.issue_title())?.map(Into::into) {
                if let Some(parent) = book(&mut item, parent) {
                    parent.set_title(ititle);
                } else {
                    item.set_title(ititle);
                }
            }
        }

        // "number" is generally used in Biblatex for "The number of a journal
        // or the volume/number of a book in a series".  However, it is also
        // used for patent entries as "the number or record token of a patent
        // or patent request", and is also listed as an optional field for
        // report, manual, and dataset, where it fits the use for patent.
        // Hayagriva uses "issue" for the journal/book sense of biblatex's number,
        // and "serial-number" for the record number / token sense.
        if let Some(number) = map_res(entry.number())?.map(|d| d.into()) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_issue(number);
            } else {
                match item.entry_type {
                    EntryType::Report
                    | EntryType::Patent
                    | EntryType::Entry
                    | EntryType::Reference => {
                        item.set_keyed_serial_number("serial", number.to_string())
                    }
                    _ => item.set_issue(number),
                }
            }
        }

        if let Some(PermissiveType::Typed(volume)) = map_res(entry.volume())? {
            let val = Numeric::new(volume as i32).into();
            if let Some(parent) = book(&mut item, parent) {
                parent.set_volume(val);
            } else {
                item.set_volume(val);
            }
        }

        if let Some(parent) = mv(&mut item, parent, mv_parent) {
            if let Some(volumes) = map_res(entry.volumes())? {
                parent.set_volume_total(Numeric::new(volumes as i32));
            }
        }

        if let Some(version) = map_res(entry.version())? {
            item.set_keyed_serial_number("version", version.format_verbatim());
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

        if let Some(eprint) = map_res(entry.eprint())? {
            let eprint_type =
                map_res(entry.eprint_type().map(|c| c.format_verbatim().to_lowercase()))?;
            let eprint_type = eprint_type.as_deref();
            if eprint_type == Some("arxiv") {
                item.set_arxiv(eprint);
            } else if eprint_type == Some("pubmed") {
                item.set_pmid(eprint);
            }
        }

        if let Some(isan) = map_res(entry.isan())? {
            item.set_keyed_serial_number("isan", isan.format_verbatim());
        }

        if let Some(ismn) = map_res(entry.ismn())? {
            item.set_keyed_serial_number("ismn", ismn.format_verbatim());
        }

        if let Some(iswc) = map_res(entry.iswc())? {
            item.set_keyed_serial_number("iswc", iswc.format_verbatim());
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

        if let Some(publisher_name) =
            map_res(entry.publisher())?.map(|pubs| comma_list(&pubs))
        {
            let location = map_res(entry.location())?.map(|d| d.into());
            let publisher = Publisher::new(Some(publisher_name), location);
            if let Some(parent) = book(&mut item, parent) {
                parent.set_publisher(publisher);
            } else {
                item.set_publisher(publisher);
            }
        } else if let Some(location) = map_res(entry.location())?.map(|d| d.into()) {
            let publisher = Publisher::new(None, Some(location));
            if let Some(parent) = book(&mut item, parent) {
                parent.set_publisher(publisher);
            } else {
                item.set_publisher(publisher);
            }
        }

        if let Some(organization) =
            map_res(entry.organization())?.map(|orgs| comma_list(&orgs))
        {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_organization(organization);
            } else {
                item.set_organization(organization);
            }
        } else if let Some(organization) = map_res(entry.institution())?.map(Into::into) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_organization(organization);
            } else {
                item.set_organization(organization);
            }
        }

        if let Some(note) = map_res(entry.how_published())?.map(Into::into) {
            if let Some(parent) = book(&mut item, parent) {
                parent.set_note(note);
            } else {
                item.set_note(note);
            }
        }

        if let Some(pages) = map_res(entry.pages())? {
            item.set_page_range(match pages {
                PermissiveType::Typed(pages) => MaybeTyped::Typed(PageRanges::new(
                    pages
                        .into_iter()
                        .map(|p| {
                            if p.start == p.end {
                                PageRangesPart::SinglePage(Numeric::from(p.start))
                            } else {
                                PageRangesPart::Range(
                                    Numeric::from(p.start),
                                    Numeric::from(p.end),
                                )
                            }
                        })
                        .collect(),
                )),
                PermissiveType::Chunks(chunks) => {
                    MaybeTyped::infallible_from_str(&chunks.format_verbatim())
                }
            });
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

        if let Some(note) = map_res(entry.note())?.map(Into::into) {
            item.set_note(note);
        }

        if let Some(note) = map_res(entry.annotation())?
            .or_else(|| entry.addendum().ok())
            .map(Into::into)
        {
            if item.note.is_none() {
                item.set_note(note);
            }
        }

        if let Some(abstract_) = map_res(entry.abstract_())? {
            item.set_abstract_(abstract_.into())
        }

        // BibLaTeX describes "type" as "The type of a manual, patent, report, or thesis.
        // This field may also be useful for the custom types listed in § 2.1.3."
        // Hayagriva uses "genre" for 'Type, class, or subtype of the item (e.g.
        // "Doctoral dissertation" for a PhD thesis; "NIH Publication" for an NIH
        // technical report)'
        if let Some(type_) = map_res(entry.type_())? {
            item.set_genre(type_.into());
        }

        if let Some(series) = map_res(entry.series())? {
            let title: FormatString = series.into();
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
            new.set_title(chapter.into());
            let temp = item;
            new.parents.push(temp);
            item = new;
        }

        Ok(item)
    }
}

fn comma_list(items: &[Vec<Spanned<Chunk>>]) -> FormatString {
    let mut value = ChunkedString::new();
    for (i, entity) in items.iter().enumerate() {
        if i != 0 {
            value.push_str(", ", ChunkKind::Normal);
        }

        let chunked = ChunkedString::from(entity.as_slice());
        value.extend(chunked);
    }

    FormatString { value, short: None }
}

#[cfg(test)]
mod tests {
    use crate::types::PersonRole;

    #[test]
    fn test_pmid_from_biblatex() {
        let entries = crate::io::from_biblatex_str(
            r#"@article{test_article,
            title = {Title},
            volume = {3},
            url = {https://example.org},
            pages = {1--99},
            journaltitle = {Testing Journal},
            author = {Doe, Jane},
            date = {2024-12},
            eprint = {54678},
            eprinttype = {pubmed},
          }"#,
        )
        .unwrap();
        let entry = entries.get("test_article").unwrap();
        assert_eq!(Some("54678"), entry.keyed_serial_number("pmid"));
        assert_eq!(Some("54678"), entry.pmid());
    }

    #[test]
    /// See https://github.com/typst/hayagriva/issues/266
    fn issue_266() {
        let entries = crate::io::from_biblatex_str(
            r#"@video{wachowskiMatrix1999,
            type = {Action, Sci-Fi},
            entrysubtype = {film},
            title = {The {{Matrix}}},
            editor = {Wachowski, Lana and Wachowski, Lilly},
            editortype = {director},
            editora = {Wachowski, Lilly and Wachowski, Lana},
            editoratype = {scriptwriter},
            namea = {Reeves, Keanu and Fishburne, Laurence and Moss, Carrie-Anne},
            nameatype = {collaborator},
            date = {1999-03-31},
            publisher = {Warner Bros., Village Roadshow Pictures, Groucho Film Partnership},
            abstract = {When a beautiful stranger leads computer hacker Neo to a forbidding underworld, he discovers the shocking truth--the life he knows is the elaborate deception of an evil cyber-intelligence.},
            keywords = {artificial reality,dystopia,post apocalypse,simulated reality,war with machines},
            annotation = {IMDb ID: tt0133093\\
            event-location: United States, Australia}
            }"#,
        ).unwrap();

        let entry = entries.get("wachowskiMatrix1999").unwrap();
        assert_eq!(
            Some("Lilly"),
            entry
                .affiliated_with_role(PersonRole::Writer)
                .first()
                .unwrap()
                .given_name
                .as_deref()
        );

        serde_json::to_value(entry).unwrap();
    }
}
