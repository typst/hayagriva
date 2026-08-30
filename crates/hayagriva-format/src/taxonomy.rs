//! Translation from Hayagriva to CSL variables

use std::{borrow::Cow, str::FromStr};

use citationberg::LongShortForm;
use citationberg::taxonomy::{
    DateVariable, Kind, NameVariable, NumberVariable, PageVariable, StandardVariable,
};
use unic_langid::LanguageIdentifier;

use hayagriva_core::{
    ChunkedString, Date, EntryLike, EntryType, MaybeTyped, Numeric, PageRanges, Person,
    StringChunk,
};

use crate::citation_label::Alphanumerical;
use crate::types::*;

use super::Entry;

impl EntryLike for Entry {
    fn key(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.key())
    }

    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<MaybeTyped<Cow<'_, Numeric>>> {
        match variable {
            NumberVariable::ChapterNumber => self
                .chapter()
                .or_else(|| {
                    self.bound_select(
                        &select!(
                            (("e":Anthos) > ("p":Anthology)) |
                            (("e":*) > ("p":Reference)) |
                            (("e":Article) > ("p":Proceedings)) |
                            (("e":*) > ("p":Book))
                        ),
                        "e",
                    )
                    .and_then(Entry::volume)
                })
                .map(MaybeTyped::to_cow),
            NumberVariable::CitationNumber => panic!("processor must resolve this"),
            NumberVariable::CollectionNumber => {
                self.get_collection().and_then(Entry::volume).map(MaybeTyped::to_cow)
            }
            NumberVariable::Edition => self.map(|e| e.edition()).map(MaybeTyped::to_cow),
            NumberVariable::FirstReferenceNoteNumber => {
                panic!("processor must resolve this")
            }
            NumberVariable::Issue => self.map(|e| e.issue()).map(MaybeTyped::to_cow),
            NumberVariable::Locator => panic!("processor must resolve this"),
            NumberVariable::Number => self
                .serial_number()
                .and_then(|s| s.0.get("serial"))
                // Serial numbers should always be treated as untyped strings,
                // even if sometimes they are valid number ranges with affixes.
                .map(|s| MaybeTyped::String(s.to_owned()))
                .or_else(|| {
                    // User can specify either 'serial-number: 3' or
                    // 'chapter: 3' for chapter entries, with the same result.
                    self.bound_select(&select!(("e":Chapter)), "e")
                        .and_then(|s| s.chapter())
                        .map(MaybeTyped::to_cow)
                }),
            NumberVariable::NumberOfPages => {
                self.page_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
            }
            NumberVariable::NumberOfVolumes => {
                self.volume_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
            }
            NumberVariable::PageFirst => self
                .page_range()
                .and_then(MaybeTyped::as_typed)
                .and_then(PageRanges::first)
                .map(|r| MaybeTyped::Typed(Cow::Owned(r.clone()))),
            NumberVariable::PartNumber => self
                .bound_select(
                    &select!(
                        (("e":*) > (Article | Blog | Book | Legislation))
                    ),
                    "e",
                )
                .and_then(Entry::volume)
                .map(MaybeTyped::to_cow),
            NumberVariable::PrintingNumber => self
                .map(|e| e.serial_number())
                .and_then(|s| s.0.get("printing"))
                .map(|s| {
                    Numeric::from_str(s)
                        .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                        .unwrap_or_else(|_| MaybeTyped::String(s.to_owned()))
                }),
            NumberVariable::Section => None,
            NumberVariable::SupplementNumber => None,
            NumberVariable::Version => self
                .bound_select(&select!(("e":Repository)), "e")
                .and_then(Entry::serial_number)
                .and_then(|s| s.0.get("version"))
                .map(|s| {
                    Numeric::from_str(s)
                        .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                        .unwrap_or_else(|_| MaybeTyped::String(s.to_owned()))
                }),
            NumberVariable::Volume => self
                .get_container()
                .and_then(|e| e.volume())
                .or_else(|| self.volume())
                .map(MaybeTyped::to_cow),
        }
    }

    fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<MaybeTyped<PageRanges>> {
        match variable {
            PageVariable::Page => self.page_range().cloned(),
        }
    }

    // Number variables are standard variables.
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, ChunkedString>> {
        let entry = self;
        match variable {
            StandardVariable::Abstract => entry
                .map(|e| e.abstract_())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::Annote => {
                entry.map(|e| e.note()).map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::Archive => {
                entry.map(|e| e.archive()).map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::ArchiveCollection => None,
            StandardVariable::ArchiveLocation => {
                entry.archive_location().map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::ArchivePlace => None,
            StandardVariable::Authority => {
                // `Entry::organization` may also appear as `StandardVariable::Publisher`.
                // See the comment in that arm.
                entry.organization().map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::CallNumber => {
                entry.call_number().map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::CitationKey => {
                Some(Cow::Owned(StringChunk::verbatim(&entry.key).into()))
            }
            StandardVariable::CitationLabel => {
                Some(Cow::Owned(Alphanumerical::default().citation(entry).into()))
            }
            // Get third-order title first, then second-order title.
            StandardVariable::CollectionTitle => entry
                .get_collection()
                .and_then(|e| e.title())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::ContainerTitle => entry
                .get_container()
                .and_then(|e| e.title())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::ContainerTitleShort => entry
                .get_container()
                .and_then(|e| e.title())
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
            StandardVariable::Genre => {
                entry.map(|e| e.genre()).map(|f| f.select(form)).map(Cow::Borrowed)
            }
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
            StandardVariable::Note => {
                entry.note().map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::OriginalPublisher => entry
                .get_original()
                .and_then(|e| e.publisher())
                .and_then(Publisher::name)
                .map(|n| n.select(form))
                .map(Cow::Borrowed),
            StandardVariable::OriginalPublisherPlace => entry
                .get_original()
                .and_then(|e| e.publisher().and_then(|p| p.location()))
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::OriginalTitle => entry
                .get_original()
                .and_then(|e| e.title())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::PartTitle => None,
            StandardVariable::PMCID => {
                entry.pmcid().map(|d| Cow::Owned(StringChunk::verbatim(d).into()))
            }
            StandardVariable::PMID => {
                entry.pmid().map(|d| Cow::Owned(StringChunk::verbatim(d).into()))
            }
            StandardVariable::Publisher => entry
                .map(|e| e.publisher())
                .and_then(Publisher::name)
                // Fallback to `organization` if `publisher` is missing.
                //
                // Many CSL styles use `<text variable="publisher"/>` to display
                // the university where a thesis was written, because Zotero
                // exports the university to `publisher` in CSL-JSON.
                //
                // However, Zotero exports university to `institution` in BibLaTeX.
                // In BibLaTeX, `publisher` and `institution` are separate fields
                // (`school` is an alias for `institution`). We map them to
                // `Entry::publisher` and `Entry::organization` respectively.
                //
                // Therefore, this fallback is necessary to make BibLaTeX data
                // usable for such CSL styles.
                .or_else(|| entry.organization())
                .map(|n| n.select(form))
                .map(Cow::Borrowed),
            StandardVariable::PublisherPlace => entry
                .map(|e| e.publisher())
                .and_then(|p| p.location())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::References => None,
            StandardVariable::ReviewedGenre => None,
            StandardVariable::ReviewedTitle => None,
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
            StandardVariable::TitleShort => entry
                .title()
                .map(|f| f.select(LongShortForm::Short))
                .map(Cow::Borrowed),
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

                self.bound_select(&selector, "p")
                    .and_then(Entry::title)
                    .map(|f| f.select(form))
                    .map(Cow::Borrowed)
            }
            StandardVariable::YearSuffix => panic!("processor must resolve this"),
        }
    }

    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>> {
        Some(Cow::Borrowed(match variable {
            DateVariable::Accessed => self.url_any().and_then(|u| u.visit_date.as_ref()),
            DateVariable::AvailableDate => None,
            DateVariable::EventDate => self
                .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
                .and_then(Entry::date),
            DateVariable::Issued => self.date_any(),
            DateVariable::OriginalDate => self.get_original().and_then(|e| e.date()),
            DateVariable::Submitted => None,
        }?))
    }

    fn resolve_name_variable(&self, variable: NameVariable) -> Vec<Cow<'_, Person>> {
        match variable {
            NameVariable::Author => self.authors().map(|a| a.iter().collect()),
            NameVariable::Chair => self
                .bound_select(
                    &select!(
                        (* > ("p":(Proceedings | Conference)))
                    ),
                    "p",
                )
                .map(|e| e.affiliated_with_role(PersonRole::Director)),
            NameVariable::CollectionEditor => self
                .get_collection()
                .and_then(|e| e.editors())
                .map(|a| a.iter().collect()),
            NameVariable::Compiler => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Compiler)))
            }
            NameVariable::Composer => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Composer)))
            }
            NameVariable::ContainerAuthor => self
                .get_container()
                .and_then(|e| e.authors())
                .map(|a| a.iter().collect()),
            NameVariable::Contributor => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Collaborator)))
            }
            NameVariable::Curator => self
                .bound_select(
                    &select!(
                        (* > ("p":Exhibition))
                    ),
                    "p",
                )
                .map(|e| e.affiliated_with_role(PersonRole::Organizer)),
            NameVariable::Director => self
                .bound_select(
                    &select!(
                        (* > ("p":(Audio | Video))) | ("p":(Audio | Video))
                    ),
                    "p",
                )
                .map(|e| e.affiliated_with_role(PersonRole::Director)),
            NameVariable::Editor => {
                self.editors().map(|a| a.iter().collect()).or_else(|| {
                    self.get_container()
                        .and_then(|e| e.editors())
                        .map(|a| a.iter().collect())
                })
            }
            NameVariable::EditorialDirector => None,
            NameVariable::EditorTranslator => {
                let translator = self.affiliated_with_role(PersonRole::Translator);
                Some(
                    self.editors()
                        .unwrap_or_default()
                        .iter()
                        .filter(|e| translator.contains(e))
                        .collect(),
                )
            }
            NameVariable::ExecutiveProducer => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::ExecutiveProducer)))
            }
            NameVariable::Guest => None,
            NameVariable::Host => None,
            NameVariable::Illustrator => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Illustrator)))
            }
            NameVariable::Interviewer => None,
            NameVariable::Narrator => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Narrator)))
            }
            NameVariable::Organizer => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Organizer)))
            }
            NameVariable::OriginalAuthor => self
                .get_original()
                .and_then(|e| e.authors())
                .map(|a| a.iter().collect()),
            NameVariable::Performer => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::CastMember)))
            }
            NameVariable::Producer => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Producer)))
            }
            NameVariable::Recipient => None,
            NameVariable::ReviewedAuthor => None,
            NameVariable::ScriptWriter => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Writer)))
            }
            NameVariable::SeriesCreator => self
                .bound_select(
                    &select!(
                        (* > ("p":(Audio | Video))) | ("p":(Audio | Video))
                    ),
                    "p",
                )
                .map(|e| e.affiliated_with_role(PersonRole::Founder)),
            NameVariable::Translator => {
                self.map(|e| Some(e.affiliated_with_role(PersonRole::Translator)))
            }
        }
        .unwrap_or_default()
        .into_iter()
        .map(Cow::Borrowed)
        .collect()
    }

    fn matches_entry_type(&self, kind: Kind) -> bool {
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

                let is_journal = select!(Article > Periodical).matches(self);
                if kind == Kind::ArticleJournal {
                    return is_journal;
                }

                let is_news = select!(Article > Newspaper).matches(self);
                if kind == Kind::ArticleNewspaper {
                    return is_news;
                }

                let is_conference = select!(Article > Proceedings).matches(self);
                if kind == Kind::PaperConference {
                    return is_conference;
                }

                let is_report = select!((* > Report) | Report).matches(self);
                if kind == Kind::Report {
                    return is_report;
                }

                let is_thesis = select!((* > Thesis) | Thesis).matches(self);
                if kind == Kind::Thesis {
                    return is_thesis;
                }

                let is_manuscript = self.entry_type() == &EntryType::Manuscript;
                if kind == Kind::Manuscript {
                    return is_manuscript;
                }

                self.entry_type() == &EntryType::Article
                    && !select!(* > Blog).matches(self)
                    && !(is_journal
                        || is_news
                        || is_conference
                        || is_report
                        || is_thesis
                        || is_manuscript)
            }
            Kind::Book | Kind::Classic | Kind::Periodical | Kind::Collection => {
                if !select!(Book | Anthology | Proceedings).matches(self) {
                    return false;
                }

                if kind == Kind::Classic {
                    // TODO: Hayagriva does not support indicating something is a
                    // classic.
                    return false;
                }

                let is_periodical =
                    select!((Book > Periodical) | Periodical).matches(self);
                if kind == Kind::Periodical {
                    return is_periodical;
                }

                let is_collection = self.entry_type() == &EntryType::Anthology;
                if kind == Kind::Collection {
                    return is_collection;
                }

                !(is_periodical || is_collection)
            }
            Kind::Chapter => select!(
                (Anthos > Anthology) | (Chapter > (Book | Anthology | Proceedings))
            )
            .matches(self),
            Kind::Entry | Kind::EntryDictionary | Kind::EntryEncyclopedia => {
                if kind == Kind::EntryDictionary {
                    // TODO: We do not differentiate between dictionaries and other
                    // references.
                    return false;
                }

                let is_encyclopedia = select!(* > Reference).matches(self);
                if kind == Kind::EntryEncyclopedia {
                    return is_encyclopedia;
                }

                self.entry_type() == &EntryType::Entry && !is_encyclopedia
            }
            Kind::Event => self.entry_type() == &EntryType::Exhibition,
            Kind::Hearing | Kind::Interview | Kind::Performance | Kind::Speech => false,
            Kind::Broadcast | Kind::MotionPicture | Kind::MusicalScore | Kind::Song => {
                let is_music_score =
                    select!(Audio > (Book | Periodical | Reference | Misc | Blog | Web))
                        .matches(self);
                if kind == Kind::MusicalScore {
                    return is_music_score;
                }

                let is_motion_picture =
                    self.entry_type() == &EntryType::Video && self.parents().is_empty();
                if kind == Kind::MotionPicture {
                    return is_motion_picture;
                }

                let is_song =
                    self.entry_type() == &EntryType::Audio && self.parents().is_empty();
                if kind == Kind::Song {
                    return is_song;
                }

                matches!(self.entry_type(), EntryType::Audio | EntryType::Video)
                    && !(is_music_score || is_motion_picture || is_song)
            }
            Kind::Legislation | Kind::Bill => {
                if self.entry_type() != &EntryType::Legislation {
                    return false;
                }

                let is_published = self.publisher().is_some();
                if kind == Kind::Bill {
                    return !is_published;
                }

                is_published
            }
            Kind::LegalCase => self.entry_type() == &EntryType::Case,
            Kind::Regulation | Kind::Standard | Kind::Treaty => false,
            Kind::Patent => self.entry_type() == &EntryType::Patent,
            Kind::Webpage | Kind::PostWeblog | Kind::Post => {
                let is_blogpost = select!(* > Blog).matches(self);
                if kind == Kind::PostWeblog {
                    return is_blogpost;
                }

                let is_post = select!(Post | (* > Thread)).matches(self);
                if kind == Kind::Post {
                    return is_post;
                }

                select!((Misc["url"]) | (* > (Web | Blog)) | Web | Blog | Thread)
                    .matches(self)
                    && !(is_blogpost || is_post)
            }
            Kind::Dataset => false,
            Kind::Figure | Kind::Graphic | Kind::Map => {
                let is_figure = select!(Artwork > Article).matches(self);
                if kind == Kind::Figure {
                    return is_figure;
                }

                if kind == Kind::Map {
                    return false;
                }

                self.entry_type() == &EntryType::Artwork && !is_figure
            }
            Kind::Pamphlet => false,
            Kind::PersonalCommunication => false,
            Kind::Review | Kind::ReviewBook => false,
            Kind::Software => self.entry_type() == &EntryType::Repository,
            Kind::Document => self.entry_type() == &EntryType::Misc,
        }
    }

    fn is_english(&self) -> Option<bool> {
        self.language().map(|l| l.language.as_str() == "en")
    }
}

pub(super) fn csl_language(lang_id: &LanguageIdentifier) -> String {
    let mut buf = String::with_capacity(if lang_id.region.is_some() { 5 } else { 2 });
    buf.push_str(lang_id.language.as_str());
    if let Some(region) = lang_id.region {
        buf.push('-');
        buf.push_str(region.as_str());
    }
    buf
}

#[cfg(all(test, feature = "biblatex"))]
mod tests {
    use super::*;
    use crate::io::{from_biblatex_str, from_yaml_str};

    #[test]
    fn zotero_university_interop() {
        let entry = from_biblatex_str(
        r#"
        @thesis{zotero-export-better-biblatex,
            type = {Unpublished master’s thesis},
            title = {Fathers’ Participation in Family Work: {{Consequences}} for Fathers’ Stress and Father-Child Relations},
            author = {Almeida, David M.},
            namea = {Galambos, Nancy L.},
            nameatype = {collaborator},
            date = {1990-11},
            institution = {University of Victoria},
            location = {Victoria, British Columbia, Canada},
            langid = {american}
        }
        "#,
    ).unwrap().into_iter().next().unwrap();
        assert_eq!(entry.publisher().unwrap().name(), None);
        assert_eq!(
            entry.publisher().unwrap().location().unwrap(),
            &"Victoria, British Columbia, Canada".parse().unwrap()
        );
        assert_eq!(
            entry.organization().unwrap(),
            &"University of Victoria".parse().unwrap()
        );

        assert_eq!(
            entry
                .resolve_standard_variable(
                    Default::default(),
                    StandardVariable::Publisher
                )
                .unwrap()
                .to_str(),
            "University of Victoria"
        );
        assert_eq!(
            entry
                .resolve_standard_variable(
                    Default::default(),
                    StandardVariable::PublisherPlace
                )
                .unwrap()
                .to_str(),
            "Victoria, British Columbia, Canada"
        );
    }

    #[test]
    fn verbatim_serial_number() {
        let entry = from_yaml_str(
            // https://github.com/typst/hayagriva/issues/506
            r#"
            report:
              type: report
              serial-number: USDL-26-0599
              volume: XY-1-5
            "#,
        )
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
        // Hyphens in serial numbers should not be converted to en dashes.
        assert_eq!(
            entry
                .resolve_number_variable(NumberVariable::Number)
                .unwrap()
                .to_str(),
            "USDL-26-0599"
        );
        // But hyphens in number ranges can be converted.
        assert_eq!(
            entry
                .resolve_number_variable(NumberVariable::Volume)
                .unwrap()
                .to_str(),
            "XY-1–5"
        );
    }
}
