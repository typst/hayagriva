use std::borrow::Cow;
use std::cmp;
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

#[cfg(feature = "csl-json")]
use citationberg::json as csl_json;

use super::{DisambiguateState, InstanceContext, LocatorPayload};

pub trait EntryLike {
    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<MaybeTyped<Cow<'_, Numeric>>>;
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, ChunkedString>>;
    fn resolve_name_variable(&self, variable: NameVariable) -> Vec<Cow<'_, Person>>;
    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>>;
    fn matches_entry_type(&self, kind: taxonomy::Kind) -> bool;
    fn is_english(&self) -> Option<bool>;
    fn key(&self) -> Cow<'_, str>;
}

impl<'a, T: EntryLike> InstanceContext<'a, T> {
    pub(super) fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<NumberVariableResult<'a>> {
        match variable {
            NumberVariable::CitationNumber => {
                Some(NumberVariableResult::Regular(MaybeTyped::Typed(Cow::Owned(
                    Numeric::from(self.cite_props.speculative.citation_number as u32 + 1),
                ))))
            }
            NumberVariable::FirstReferenceNoteNumber => {
                self.cite_props.certain.first_note_number.map(|n| {
                    NumberVariableResult::Regular(MaybeTyped::Typed(Cow::Owned(
                        Numeric::from(n as u32),
                    )))
                })
            }
            NumberVariable::Locator => match self.cite_props.speculative.locator?.1 {
                LocatorPayload::Str(l) => Some(NumberVariableResult::from_regular(
                    Numeric::from_str(l)
                        .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                        .unwrap_or_else(|_| MaybeTyped::String(l.to_owned())),
                )),
                LocatorPayload::Transparent => Some(NumberVariableResult::Transparent(
                    self.cite_props.certain.initial_idx,
                )),
            },
            _ => self
                .entry
                .resolve_number_variable(variable)
                .map(NumberVariableResult::from_regular),
        }
    }

    // Number variables are standard variables.
    pub(super) fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'a, ChunkedString>> {
        match variable {
            StandardVariable::YearSuffix => {
                if let DisambiguateState::YearSuffix(s) =
                    self.cite_props.speculative.disambiguation
                {
                    Some(Cow::Owned(StringChunk::normal(letter(s)).into()))
                } else {
                    None
                }
            }
            _ => self.entry.resolve_standard_variable(form, variable),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum NumberVariableResult<'a> {
    Regular(MaybeTyped<Cow<'a, Numeric>>),
    Transparent(usize),
}

impl<'a> NumberVariableResult<'a> {
    pub(super) fn from_regular(regular: MaybeTyped<Cow<'a, Numeric>>) -> Self {
        Self::Regular(regular)
    }

    pub(super) fn csl_cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Self::Regular(a), Self::Regular(b)) => a.csl_cmp(b),
            (Self::Regular(_), Self::Transparent(_)) => cmp::Ordering::Less,
            (Self::Transparent(_), Self::Regular(_)) => cmp::Ordering::Greater,
            (Self::Transparent(a), Self::Transparent(b)) => a.cmp(b),
        }
    }
}

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
            NumberVariable::Number => {
                return self.serial_number().and_then(|s| s.0.get("serial")).map(|s| {
                    Numeric::from_str(s)
                        .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                        .unwrap_or_else(|_| MaybeTyped::String(s.to_owned()))
                })
            }
            NumberVariable::NumberOfPages => {
                self.page_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
            }
            NumberVariable::NumberOfVolumes => {
                self.volume_total().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
            }
            NumberVariable::Page => {
                self.page_range().map(|n| MaybeTyped::Typed(Cow::Borrowed(n)))
            }
            NumberVariable::PageFirst => self
                .page_range()
                .and_then(|r| r.range())
                .map(|r| MaybeTyped::Typed(Cow::Owned(Numeric::from(r.start)))),
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
            NumberVariable::Volume => self.volume().map(MaybeTyped::to_cow),
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
            StandardVariable::CitationLabel => entry
                .map(|e| e.authors())
                .and_then(|a| a.first())
                .map(|n| n.name.chars().take(3).collect::<String>())
                .or_else(|| {
                    entry.title().map(|t| {
                        t.select(LongShortForm::default())
                            .to_string()
                            .chars()
                            .take(3)
                            .collect::<String>()
                    })
                })
                .map(|s| {
                    let s = if let Some(date) = entry.date() {
                        format!("{}{:02}", s, date.year % 100)
                    } else {
                        s
                    };
                    Cow::Owned(StringChunk::verbatim(s).into())
                }),
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
            StandardVariable::Note => {
                entry.note().map(|f| f.select(form)).map(Cow::Borrowed)
            }
            StandardVariable::OriginalPublisher => entry
                .get_original()
                .and_then(|e| e.publisher())
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::OriginalPublisherPlace => entry
                .get_original()
                .and_then(|e| e.publisher().and_then(|_| e.location()))
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
                .map(|f| f.select(form))
                .map(Cow::Borrowed),
            StandardVariable::PublisherPlace => entry
                .map(|e| if e.publisher().is_some() { Some(e) } else { None })
                .and_then(|e| e.location())
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
            StandardVariable::Title => entry
                .title()
                .map(|f| f.select(LongShortForm::Short))
                .map(Cow::Borrowed),
            StandardVariable::TitleShort => {
                entry.title().map(|f| f.select(form)).map(Cow::Borrowed)
            }
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

    fn resolve_name_variable(
        &self,
        variable: taxonomy::NameVariable,
    ) -> Vec<Cow<'_, Person>> {
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
                        (* > ("p":(Audio | Video)))
                    ),
                    "p",
                )
                .map(|e| e.affiliated_with_role(PersonRole::Director)),
            NameVariable::Editor => self.editors().map(|a| a.iter().collect()),
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

    fn matches_entry_type(&self, kind: citationberg::taxonomy::Kind) -> bool {
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
            Kind::Chapter => {
                select!(Chapter > (Book | Anthology | Proceedings)).matches(self)
            }
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

#[cfg(feature = "csl-json")]
impl EntryLike for citationberg::json::Item {
    fn resolve_standard_variable(
        &self,
        _: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, ChunkedString>> {
        match self.0.get(&variable.to_string())? {
            csl_json::Value::String(s) => {
                Some(Cow::Owned(StringChunk::normal(s.clone()).into()))
            }
            csl_json::Value::Number(n) => {
                Some(Cow::Owned(StringChunk::normal(n.to_string()).into()))
            }
            _ => None,
        }
    }

    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>> {
        match self.0.get(&variable.to_string())? {
            csl_json::Value::Date(d) => {
                let Ok(d) = csl_json::FixedDateRange::try_from(d.clone()) else {
                    return None;
                };
                if d.end.is_some() {
                    panic!("ranges are not supported")
                }
                let d = d.start;
                Some(Cow::Owned(Date {
                    year: d.year as i32,
                    month: d.month,
                    day: d.day,
                    approximate: false,
                }))
            }
            _ => None,
        }
    }

    fn resolve_name_variable(&self, variable: NameVariable) -> Vec<Cow<'_, Person>> {
        match self.0.get(&variable.to_string()) {
            Some(csl_json::Value::Names(names)) => names
                .iter()
                .map(|name| {
                    Cow::Owned(match name {
                        csl_json::NameValue::Literal(l) => Person {
                            name: l.literal.clone(),
                            prefix: None,
                            suffix: None,
                            given_name: None,
                            alias: None,
                        },
                        csl_json::NameValue::Item(csl_json::NameItem {
                            family,
                            given,
                            non_dropping_particle: None,
                            dropping_particle: None,
                            suffix,
                        }) => {
                            let mut parts = vec![family.as_str()];
                            if let Some(given) = given {
                                parts.push(given.as_str());
                            }
                            let mut p = Person::from_strings(parts).unwrap();
                            if let Some(suffix) = suffix {
                                p.suffix = Some(suffix.as_str().to_owned());
                            }

                            p
                        }
                        csl_json::NameValue::Item(csl_json::NameItem {
                            family,
                            given,
                            non_dropping_particle,
                            dropping_particle,
                            suffix,
                        }) => Person {
                            name: if let Some(non_drop) = non_dropping_particle {
                                format!("{} {}", non_drop, family)
                            } else {
                                family.clone()
                            },
                            prefix: dropping_particle.clone(),
                            suffix: suffix.clone(),
                            given_name: given.clone(),
                            alias: None,
                        },
                    })
                })
                .collect(),
            _ => vec![],
        }
    }

    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<MaybeTyped<Cow<'_, Numeric>>> {
        match self.0.get(&variable.to_string())? {
            csl_json::Value::Number(n) => {
                Some(MaybeTyped::Typed(Cow::Owned(Numeric::from(*n as u32))))
            }
            csl_json::Value::String(s) => {
                let res = MaybeTyped::<Numeric>::infallible_from_str(s);
                Some(match res {
                    MaybeTyped::String(s) => MaybeTyped::String(s),
                    MaybeTyped::Typed(n) => MaybeTyped::Typed(Cow::Owned(n)),
                })
            }
            _ => None,
        }
    }

    fn matches_entry_type(&self, kind: taxonomy::Kind) -> bool {
        let Some(actual) = self.0.get("type") else {
            return false;
        };
        let Some(string) = actual.to_str() else {
            return false;
        };
        Kind::from_str(&string) == Ok(kind)
    }

    fn is_english(&self) -> Option<bool> {
        self.0
            .get("language")
            .and_then(|l| l.to_str())
            .map(|l| l.starts_with("en"))
    }

    fn key(&self) -> Cow<'_, str> {
        self.id().unwrap_or_default()
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

// A function that takes a usize value and returns a String
fn letter(val: u8) -> String {
    let mut result = String::with_capacity(1);
    let mut current = val;

    loop {
        let remainder = current % 26;
        let c = (remainder + b'a') as char;
        result.insert(0, c);

        if current < 26 {
            break;
        }

        current = (current - 26) / 26;
    }

    result
}
