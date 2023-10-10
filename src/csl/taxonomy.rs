use std::borrow::Cow;
use std::str::FromStr;

use crate::types::{
    ChunkedString, Date, EntryType, MaybeTyped, Numeric, Person, PersonRole, StringChunk,
};
use crate::Entry;
use citationberg::taxonomy::{
    DateVariable, Kind, NameVariable, NumberVariable, StandardVariable,
};
use citationberg::{taxonomy, LongShortForm};
use unic_langid::LanguageIdentifier;

pub(crate) fn resolve_number_variable(
    entry: &Entry,
    variable: NumberVariable,
) -> Option<MaybeTyped<Cow<Numeric>>> {
    match variable {
        NumberVariable::ChapterNumber => entry
            .bound_select(
                &select!(
                    (("e":Anthos) > ("p":Anthology)) |
                    (("e":*) > ("p":Reference)) |
                    (("e":Article) > ("p":Proceedings)) |
                    (("e":*) > ("p":Book))
                ),
                "e",
            )
            .and_then(Entry::volume)
            .map(MaybeTyped::to_cow),
        NumberVariable::CitationNumber => todo!("that's for us to do baby"),
        NumberVariable::CollectionNumber => entry
            .bound_select(
                &select!(
                    (* > ("p":(Book | Anthology | Proceedings)))
                ),
                "p",
            )
            .and_then(Entry::volume)
            .map(MaybeTyped::to_cow),
        NumberVariable::Edition => entry.map(|e| e.edition()).map(MaybeTyped::to_cow),
        NumberVariable::FirstReferenceNoteNumber => todo!("you guessed it, baybee"),
        NumberVariable::Issue => entry.map(|e| e.issue()).map(MaybeTyped::to_cow),
        NumberVariable::Locator => todo!("boy oh boy"),
        NumberVariable::Number => {
            return entry.serial_number().map(|s| {
                Numeric::from_str(s)
                    .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                    .unwrap_or_else(|_| MaybeTyped::String(s.to_owned()))
            })
        }
        NumberVariable::NumberOfPages => {
            entry.page_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
        }
        NumberVariable::NumberOfVolumes => {
            entry.volume_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
        }
        NumberVariable::Page => {
            entry.page_range().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
        }
        NumberVariable::PageFirst => entry
            .page_range()
            .and_then(|r| r.range())
            .map(|r| MaybeTyped::Typed(Cow::Owned(Numeric::from(r.start)))),
        NumberVariable::PartNumber => entry
            .bound_select(
                &select!(
                    (("e":*) > (Article | Blog | Book | Legislation))
                ),
                "e",
            )
            .and_then(Entry::volume)
            .map(MaybeTyped::to_cow),
        NumberVariable::PrintingNumber => None,
        NumberVariable::Section => None,
        NumberVariable::SupplementNumber => None,
        NumberVariable::Version => entry
            .bound_select(&select!(("e":Repository)), "e")
            .and_then(Entry::serial_number)
            .map(|s| {
                Numeric::from_str(s)
                    .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                    .unwrap_or_else(|_| MaybeTyped::String(s.to_owned()))
            }),
        NumberVariable::Volume => entry.volume().map(MaybeTyped::to_cow),
    }
}

// Number variables are standard variables.
pub(crate) fn resolve_standard_variable(
    entry: &Entry,
    form: LongShortForm,
    variable: StandardVariable,
) -> Option<Cow<ChunkedString>> {
    match variable {
        StandardVariable::Abstract => None,
        StandardVariable::Annote => None,
        StandardVariable::Archive => {
            entry.map(|e| e.archive()).map(|f| f.select(form)).map(Cow::Borrowed)
        }
        StandardVariable::ArchiveCollection => None,
        StandardVariable::ArchiveLocation => {
            entry.archive_location().map(|f| f.select(form)).map(Cow::Borrowed)
        }
        StandardVariable::ArchivePlace => None,
        StandardVariable::Authority => {
            entry.organization().map(|f| f.select(form)).map(Cow::Borrowed)
        }
        StandardVariable::CallNumber => {
            entry.call_number().map(|f| f.select(form)).map(Cow::Borrowed)
        }
        StandardVariable::CitationKey => {
            Some(Cow::Owned(StringChunk::verbatim(&entry.key).into()))
        }
        // The spec tells us that the CSL processor may assign this, we do not.
        StandardVariable::CitationLabel => None,
        // Get third-order title first, then second-order title.
        StandardVariable::CollectionTitle => entry
            .parents()
            .iter()
            .find_map(|p| p.map_parents(|e| e.title()))
            .or_else(|| entry.map_parents(|e| e.title()))
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::ContainerTitle => entry
            .map_parents(|e| e.title())
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::ContainerTitleShort => entry
            .map_parents(|e| e.title())
            .map(|f| f.select(LongShortForm::Short))
            .map(Cow::Borrowed),
        StandardVariable::Dimensions => entry
            .runtime()
            .map(|r| Cow::Owned(StringChunk::normal(r.to_string()).into())),
        StandardVariable::Division => None,
        StandardVariable::DOI => {
            entry.doi().map(|d| Cow::Owned(StringChunk::verbatim(d).into()))
        }
        StandardVariable::Event | StandardVariable::EventTitle => entry
            .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
            .and_then(Entry::title)
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::EventPlace => entry
            .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
            .and_then(Entry::location)
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::Genre => None,
        StandardVariable::ISBN => {
            entry.isbn().map(|d| Cow::Owned(StringChunk::verbatim(d).into()))
        }
        StandardVariable::ISSN => {
            entry.issn().map(|d| Cow::Owned(StringChunk::verbatim(d).into()))
        }
        StandardVariable::Jurisdiction => None,
        StandardVariable::Keyword => None,
        StandardVariable::Language => entry
            .map(|e| e.language())
            .map(|l| Cow::Owned(StringChunk::normal(csl_language(l)).into())),
        StandardVariable::License => None,
        StandardVariable::Medium => None,
        StandardVariable::Note => entry.note().map(|f| f.select(form)).map(Cow::Borrowed),
        // TODO: Find out how to express original publisher.
        StandardVariable::OriginalPublisher => None,
        StandardVariable::OriginalPublisherPlace => None,
        StandardVariable::OriginalTitle => None,
        StandardVariable::PartTitle => None,
        // TODO: Accomodate more serial numbers
        StandardVariable::PMCID => None,
        StandardVariable::PMID => None,
        StandardVariable::Publisher => entry
            .map(|e| e.publisher())
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::PublisherPlace => None,
        StandardVariable::References => None,
        StandardVariable::ReviewedGenre => None,
        StandardVariable::ReviewedTitle => entry
            .map_parents(|e| e.title())
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::Scale => None,
        StandardVariable::Source => entry
            .bound_select(&select!(* > ("p":Repository)), "p")
            .and_then(Entry::title)
            .map(|f| f.select(form))
            .map(Cow::Borrowed),
        StandardVariable::Status => None,
        StandardVariable::Title => {
            entry.title().map(|f| f.select(form)).map(Cow::Borrowed)
        }
        StandardVariable::TitleShort => None,
        StandardVariable::URL => entry
            .map(|e| e.url())
            .map(|d| Cow::Owned(StringChunk::verbatim(d.to_string()).into())),
        StandardVariable::VolumeTitle => {
            let selector = select!(
                (Anthos > ("p":Anthology)) |
                (Entry  > ("p":*)) |
                (* > ("p":Reference)) |
                (Article > ("p":Proceedings))
            );
            entry
                .bound_select(&selector, "p")
                .and_then(Entry::title)
                .map(|f| f.select(form))
                .map(Cow::Borrowed)
        }
        StandardVariable::YearSuffix => todo!("we actually have to generate this"),
    }
}

pub(crate) fn resolve_date_variable(
    entry: &Entry,
    variable: DateVariable,
) -> Option<&Date> {
    match variable {
        DateVariable::Accessed => entry.url_any().and_then(|u| u.visit_date.as_ref()),
        DateVariable::AvailableDate => None,
        DateVariable::EventDate => entry
            .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
            .and_then(Entry::date),
        DateVariable::Issued => entry.date_any(),
        // TODO match with the original publication stuff above
        DateVariable::OriginalDate => None,
        DateVariable::Submitted => None,
    }
}

pub(crate) fn resolve_name_variable(
    entry: &Entry,
    variable: taxonomy::NameVariable,
) -> Vec<&Person> {
    match variable {
        NameVariable::Author => entry.authors().map(|a| a.iter().collect()),
        NameVariable::Chair => entry
            .bound_select(
                &select!(
                    (* > ("p":(Proceedings | Conference)))
                ),
                "p",
            )
            .map(|e| e.affiliated_with_role(PersonRole::Director)),
        NameVariable::CollectionEditor => entry
            .bound_select(
                &select!(
                    (* > ("p":(Book | Anthology)))
                ),
                "p",
            )
            .and_then(|e| e.editors())
            .map(|a| a.iter().collect()),
        NameVariable::Compiler => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Compiler)))
        }
        NameVariable::Composer => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Composer)))
        }
        NameVariable::ContainerAuthor => {
            entry.map_parents(|e| e.authors()).map(|a| a.iter().collect())
        }
        NameVariable::Contributor => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Collaborator)))
        }
        NameVariable::Curator => entry
            .bound_select(
                &select!(
                    (* > ("p":Exhibition))
                ),
                "p",
            )
            .map(|e| e.affiliated_with_role(PersonRole::Organizer)),
        NameVariable::Director => entry
            .bound_select(
                &select!(
                    (* > ("p":(Audio | Video)))
                ),
                "p",
            )
            .map(|e| e.affiliated_with_role(PersonRole::Director)),
        NameVariable::Editor => entry.editors().map(|a| a.iter().collect()),
        NameVariable::EditorialDirector => None,
        NameVariable::EditorTranslator => {
            let translator = entry.affiliated_with_role(PersonRole::Translator);
            Some(
                entry
                    .editors()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|e| translator.contains(e))
                    .collect(),
            )
        }
        NameVariable::ExecutiveProducer => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::ExecutiveProducer)))
        }
        NameVariable::Guest => None,
        NameVariable::Host => None,
        NameVariable::Illustrator => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Illustrator)))
        }
        NameVariable::Interviewer => None,
        NameVariable::Narrator => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Narrator)))
        }
        NameVariable::Organizer => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Organizer)))
        }
        // TODO: Find out how to express original publisher.
        NameVariable::OriginalAuthor => None,
        NameVariable::Performer => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::CastMember)))
        }
        NameVariable::Producer => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Producer)))
        }
        NameVariable::Recipient => None,
        NameVariable::ReviewedAuthor => None,
        NameVariable::ScriptWriter => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Writer)))
        }
        NameVariable::SeriesCreator => entry
            .bound_select(
                &select!(
                    (* > ("p":(Audio | Video))) | ("p":(Audio | Video))
                ),
                "p",
            )
            .map(|e| e.affiliated_with_role(PersonRole::Founder)),
        NameVariable::Translator => {
            entry.map(|e| Some(e.affiliated_with_role(PersonRole::Translator)))
        }
    }
    .unwrap_or_default()
}

pub(crate) fn matches_entry_type(
    entry: &Entry,
    kind: citationberg::taxonomy::Kind,
) -> bool {
    // Each match arm contains mutually exclusive entry kinds.
    match kind {
        Kind::Article
        | Kind::ArticleMagazine
        | Kind::ArticleNewspaper
        | Kind::ArticleJournal
        | Kind::PaperConference
        | Kind::Report
        | Kind::Thesis
        | Kind::Manuscript => {
            if kind == Kind::ArticleMagazine {
                // TODO: Hayagriva does not differentiate between scientific and
                // non-scientific magazines. Could disambiguate via presence of
                // DOI or similar.
                return false;
            }

            let is_journal = select!(Article > Periodical).matches(entry);
            if kind == Kind::ArticleJournal {
                return is_journal;
            }

            let is_news = select!(Article > Newspaper).matches(entry);
            if kind == Kind::ArticleNewspaper {
                return is_news;
            }

            let is_conference = select!(Article > Proceedings).matches(entry);
            if kind == Kind::PaperConference {
                return is_conference;
            }

            let is_report = select!((* > Report) | Report).matches(entry);
            if kind == Kind::Report {
                return is_report;
            }

            let is_thesis = select!((* > Thesis) | Thesis).matches(entry);
            if kind == Kind::Thesis {
                return is_thesis;
            }

            let is_manuscript = entry.entry_type() == &EntryType::Manuscript;
            if kind == Kind::Manuscript {
                return is_manuscript;
            }

            entry.entry_type() == &EntryType::Article
                && !select!(* > Blog).matches(entry)
                && !(is_journal
                    || is_news
                    || is_conference
                    || is_report
                    || is_thesis
                    || is_manuscript)
        }
        Kind::Book | Kind::Classic | Kind::Periodical | Kind::Collection => {
            if !select!(Book | Anthology | Proceedings).matches(entry) {
                return false;
            }

            if kind == Kind::Classic {
                // TODO: Hayagriva does not support indicating something is a
                // classic.
                return false;
            }

            let is_periodical = select!((Book > Periodical) | Periodical).matches(entry);
            if kind == Kind::Periodical {
                return is_periodical;
            }

            let is_collection = entry.entry_type() == &EntryType::Anthology;
            if kind == Kind::Collection {
                return is_collection;
            }

            !(is_periodical || is_collection)
        }
        Kind::Chapter => {
            select!(Chapter > (Book | Anthology | Proceedings)).matches(entry)
        }
        Kind::Entry | Kind::EntryDictionary | Kind::EntryEncyclopedia => {
            if kind == Kind::EntryDictionary {
                // TODO: We do not differentiate between dictionaries and other
                // references.
                return false;
            }

            let is_encyclopedia = select!(* > Reference).matches(entry);
            if kind == Kind::EntryEncyclopedia {
                return is_encyclopedia;
            }

            entry.entry_type() == &EntryType::Entry && !is_encyclopedia
        }
        Kind::Event => entry.entry_type() == &EntryType::Exhibition,
        Kind::Hearing | Kind::Interview | Kind::Performance | Kind::Speech => false,
        Kind::Broadcast | Kind::MotionPicture | Kind::MusicalScore | Kind::Song => {
            let is_music_score =
                select!(Audio > (Book | Periodical | Reference | Misc | Blog | Web))
                    .matches(entry);
            if kind == Kind::MusicalScore {
                return is_music_score;
            }

            let is_motion_picture =
                entry.entry_type() == &EntryType::Video && entry.parents().is_empty();
            if kind == Kind::MotionPicture {
                return is_motion_picture;
            }

            let is_song =
                entry.entry_type() == &EntryType::Audio && entry.parents().is_empty();
            if kind == Kind::Song {
                return is_song;
            }

            matches!(entry.entry_type(), EntryType::Audio | EntryType::Video)
                && !(is_music_score || is_motion_picture || is_song)
        }
        Kind::Legislation | Kind::Bill => {
            if entry.entry_type() != &EntryType::Legislation {
                return false;
            }

            let is_published = entry.publisher().is_some();
            if kind == Kind::Bill {
                return !is_published;
            }

            is_published
        }
        Kind::LegalCase => entry.entry_type() == &EntryType::Case,
        Kind::Regulation | Kind::Standard | Kind::Treaty => return false,
        Kind::Patent => entry.entry_type() == &EntryType::Patent,
        Kind::Webpage | Kind::PostWeblog | Kind::Post => {
            let is_blogpost = select!(* > Blog).matches(entry);
            if kind == Kind::PostWeblog {
                return is_blogpost;
            }

            let is_post = select!(Post | (* > Thread)).matches(entry);
            if kind == Kind::Post {
                return is_post;
            }

            select!((Misc["url"]) | (* > (Web | Blog)) | Web | Blog | Thread)
                .matches(entry)
                && !(is_blogpost || is_post)
        }
        Kind::Dataset => false,
        Kind::Figure | Kind::Graphic | Kind::Map => {
            let is_figure = select!(Artwork > Article).matches(entry);
            if kind == Kind::Figure {
                return is_figure;
            }

            if kind == Kind::Map {
                return false;
            }

            entry.entry_type() == &EntryType::Artwork && !is_figure
        }
        Kind::Pamphlet => false,
        Kind::PersonalCommunication => false,
        Kind::Review | Kind::ReviewBook => false,
        Kind::Software => entry.entry_type() == &EntryType::Repository,
        Kind::Document => entry.entry_type() == &EntryType::Misc,
    }
}

pub(crate) fn csl_language(lang_id: &LanguageIdentifier) -> String {
    let mut buf = String::with_capacity(if lang_id.region.is_some() { 5 } else { 2 });
    buf.push_str(lang_id.language.as_str());
    if let Some(region) = lang_id.region {
        buf.push('-');
        buf.push_str(region.as_str());
    }
    buf
}
