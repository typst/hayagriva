use std::{borrow::Cow, str::FromStr};

use biblatex::{Chunk, ChunksExt, EditorType, EntryType, PermissiveType, Spanned};
use citationberg::{
    LongShortForm,
    taxonomy::{
        DateVariable, Kind, NameVariable, NumberVariable, PageVariable, StandardVariable,
    },
};
use hayagriva_core::{
    ChunkKind, ChunkedString, Date, EntryLike, FormatString, MaybeTyped, Numeric,
    PageRanges, PageRangesPart, StringChunk, biblatex_conversion as conversion,
    csl_language,
};
use unic_langid::LanguageIdentifier;

pub struct Entry(pub biblatex::Entry);

impl EntryLike for Entry {
    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<hayagriva_core::MaybeTyped<Cow<'_, hayagriva_core::Numeric>>> {
        match variable {
            NumberVariable::ChapterNumber => self
                .0
                .chapter()
                .ok()
                .map(chunks_to_mby_numeric)
                .map(MaybeTyped::to_cow_owned),
            NumberVariable::CitationNumber => panic!("processor must resolve this"),
            NumberVariable::CollectionNumber => None,
            NumberVariable::Edition => self
                .0
                .edition()
                .ok()
                .as_ref()
                .map(permissive_to_mby_numeric)
                .map(MaybeTyped::to_cow_owned),
            NumberVariable::FirstReferenceNoteNumber => {
                panic!("processor must resolve this")
            }
            NumberVariable::Issue => self
                .0
                .issue()
                .ok()
                .map(chunks_to_mby_numeric)
                .map(MaybeTyped::to_cow_owned),
            NumberVariable::Locator => panic!("processor must resolve this"),
            NumberVariable::Number => {
                if matches!(self.0.entry_type, EntryType::Report | EntryType::Reference) {
                    self.0
                        .number()
                        .ok()
                        .map(chunks_to_mby_numeric)
                        .map(MaybeTyped::to_cow_owned)
                } else {
                    None
                }
            }
            NumberVariable::NumberOfPages => self
                .0
                .page_total()
                .ok()
                .and_then(|c| c.format_verbatim().parse::<Numeric>().ok())
                .map(|n| MaybeTyped::Typed(Cow::Owned(n))),
            NumberVariable::NumberOfVolumes => self
                .0
                .volumes()
                .ok()
                .map(|v| MaybeTyped::Typed(Cow::Owned(Numeric::new(v as i32)))),
            NumberVariable::PageFirst => self
                .resolve_page_variable(PageVariable::Page)
                .as_ref()
                .and_then(MaybeTyped::as_typed)
                .and_then(PageRanges::first)
                .map(|r| MaybeTyped::Typed(Cow::Owned(r.clone()))),
            NumberVariable::PartNumber => None,
            NumberVariable::PrintingNumber => None,
            NumberVariable::Section => None,
            NumberVariable::SupplementNumber => None,
            NumberVariable::Version => self.0.version().ok().map(|v| {
                let version = v.format_verbatim();
                Numeric::from_str(&version)
                    .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                    .unwrap_or_else(|_| MaybeTyped::String(version.to_owned()))
            }),
            NumberVariable::Volume => self
                .0
                .volume()
                .ok()
                .map(|v| permissive_to_mby_numeric(&v).to_cow_owned()),
        }
    }

    fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<hayagriva_core::MaybeTyped<hayagriva_core::PageRanges>> {
        match variable {
            PageVariable::Page => match self.0.pages() {
                Ok(PermissiveType::Typed(pages)) => {
                    Some(MaybeTyped::Typed(PageRanges::new(
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
                    )))
                }
                Ok(PermissiveType::Chunks(c)) => {
                    Some(MaybeTyped::infallible_from_str(&c.format_verbatim()))
                }
                _ => None,
            },
        }
    }

    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, hayagriva_core::ChunkedString>> {
        match variable {
            StandardVariable::Abstract => self
                .0
                .abstract_()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::Annote => self
                .0
                .note()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::Archive => None,
            StandardVariable::ArchiveCollection => None,
            StandardVariable::ArchiveLocation => None,
            StandardVariable::ArchivePlace => None,
            StandardVariable::Authority => self
                .0
                .organization()
                .ok()
                .map(|v| comma_list(&v))
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::CallNumber => None,
            StandardVariable::CitationKey => {
                Some(Cow::Owned(StringChunk::verbatim(&self.0.key).into()))
            }
            StandardVariable::CitationLabel => None,
            StandardVariable::CollectionTitle => None,
            StandardVariable::ContainerTitle => None,
            StandardVariable::ContainerTitleShort => None,
            StandardVariable::Dimensions => None,
            StandardVariable::Division => None,
            StandardVariable::DOI => self
                .0
                .doi()
                .ok()
                .map(|d| Cow::Owned(StringChunk::verbatim(&d).into())),
            StandardVariable::Event => None,
            StandardVariable::EventTitle => None,
            StandardVariable::EventPlace => None,
            StandardVariable::Genre => None,
            StandardVariable::ISBN => self
                .0
                .isbn()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::ISSN => self
                .0
                .issn()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::Jurisdiction => None,
            StandardVariable::Keyword => self
                .0
                .keywords()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::Language => self
                .0
                .language()
                .ok()
                .as_ref()
                .and_then(|l| l.first())
                .and_then(|l| {
                    if let PermissiveType::Typed(lang) = l {
                        let id: LanguageIdentifier = (*lang).into();
                        Some(Cow::Owned(StringChunk::normal(csl_language(&id)).into()))
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    self.0.langid().ok().and_then(|l| {
                        if let PermissiveType::Typed(lang) = l {
                            let id: LanguageIdentifier = lang.into();
                            Some(Cow::Owned(
                                StringChunk::normal(csl_language(&id)).into(),
                            ))
                        } else {
                            None
                        }
                    })
                }),
            StandardVariable::License => None,
            StandardVariable::Medium => None,
            StandardVariable::Note => self
                .0
                .note()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::OriginalPublisher => None,
            StandardVariable::OriginalPublisherPlace => None,
            StandardVariable::OriginalTitle => None,
            StandardVariable::PartTitle => None,
            StandardVariable::PMCID => None,
            StandardVariable::PMID => None,
            StandardVariable::Publisher => self
                .0
                .publisher()
                .ok()
                .map(|p| comma_list(&p))
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::PublisherPlace => self
                .0
                .location()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::References => None,
            StandardVariable::ReviewedGenre => None,
            StandardVariable::ReviewedTitle => None,
            StandardVariable::Scale => None,
            StandardVariable::Source => None,
            StandardVariable::Status => self
                .0
                .how_published()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::Title => self
                .0
                .title()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::TitleShort => self
                .0
                .short_title()
                .ok()
                .map(chunks_to_fmt_str)
                .map(|f| f.select(form).clone())
                .map(Cow::Owned),
            StandardVariable::URL => self
                .0
                .url()
                .ok()
                .map(|url| Cow::Owned(StringChunk::verbatim(&url).into())),
            StandardVariable::VolumeTitle => None,
            StandardVariable::YearSuffix => panic!("processor must resolve this"),
        }
    }

    fn resolve_name_variable(
        &self,
        variable: NameVariable,
    ) -> Vec<Cow<'_, hayagriva_core::Person>> {
        match variable {
            NameVariable::Author => self.0.author().ok(),
            NameVariable::Chair => None,
            NameVariable::CollectionEditor => None,
            NameVariable::Compiler => self.editors_with_role(EditorType::Collaborator),
            NameVariable::Composer => None,
            NameVariable::ContainerAuthor => self.0.book_author().ok(),
            NameVariable::Contributor => None,
            NameVariable::Curator => None,
            NameVariable::Director => {
                if is_non_standard_type(&self.0.entry_type, "video") {
                    self.editors_with_role(EditorType::Director)
                } else {
                    self.0.author().ok()
                }
            }
            NameVariable::Editor => self.editors_with_role(EditorType::Editor),
            NameVariable::EditorialDirector => None,
            NameVariable::EditorTranslator => None,
            NameVariable::ExecutiveProducer => None,
            NameVariable::Guest => None,
            NameVariable::Host => None,
            NameVariable::Illustrator => None,
            NameVariable::Interviewer => None,
            NameVariable::Narrator => None,
            NameVariable::Organizer => self.editors_with_role(EditorType::Organizer),
            NameVariable::OriginalAuthor => None,
            NameVariable::Performer => None,
            NameVariable::Producer => None,
            NameVariable::Recipient => None,
            NameVariable::ReviewedAuthor => None,
            NameVariable::ScriptWriter => None,
            NameVariable::SeriesCreator => None,
            NameVariable::Translator => self.0.translator().ok(),
        }
        .unwrap_or_default()
        .into_iter()
        .map(|p| Cow::Owned(conversion::person(&p)))
        .collect()
    }

    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>> {
        match variable {
            DateVariable::Accessed => self
                .0
                .url_date()
                .ok()
                .and_then(|d| match d {
                    PermissiveType::Typed(t) => Some(t),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| Cow::Owned(conversion::date(d))),
            DateVariable::AvailableDate => None,
            DateVariable::EventDate => self
                .0
                .event_date()
                .ok()
                .and_then(|d| match d {
                    PermissiveType::Typed(t) => Some(t),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| Cow::Owned(conversion::date(d))),
            DateVariable::Issued => self
                .0
                .date()
                .ok()
                .and_then(|d| match d {
                    PermissiveType::Typed(t) => Some(t),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| Cow::Owned(conversion::date(d))),
            DateVariable::OriginalDate => self
                .0
                .orig_date()
                .ok()
                .and_then(|d| match d {
                    PermissiveType::Typed(t) => Some(t),
                    PermissiveType::Chunks(_) => None,
                })
                .map(|d| Cow::Owned(conversion::date(d))),
            DateVariable::Submitted => None,
        }
    }

    fn matches_entry_type(&self, kind: Kind) -> bool {
        let ty = &self.0.entry_type;
        match kind {
            Kind::Article
            | Kind::ArticleJournal
            | Kind::ArticleMagazine
            | Kind::ArticleNewspaper => ty == &EntryType::Article,
            Kind::Bill => {
                is_non_standard_type(ty, "legislation") && self.0.publisher().is_err()
            }
            Kind::Book => ty == &EntryType::Book,
            Kind::Broadcast => false,
            Kind::Chapter => ty == &EntryType::InCollection,
            Kind::Classic => false,
            Kind::Collection => ty == &EntryType::Collection,
            Kind::Dataset => ty == &EntryType::Dataset,
            Kind::Document => ty == &EntryType::Misc,
            Kind::Entry | Kind::EntryDictionary | Kind::EntryEncyclopedia => {
                ty == &EntryType::InReference
            }
            Kind::Event => false,
            Kind::Figure | Kind::Graphic => is_non_standard_type(ty, "image"),
            Kind::Hearing => false,
            Kind::Interview => ty == &EntryType::Misc,
            Kind::LegalCase => is_non_standard_type(ty, "jurisdiction"),
            Kind::Legislation => is_non_standard_type(ty, "legislation"),
            Kind::Manuscript => ty == &EntryType::Unpublished,
            Kind::Map => false,
            Kind::MotionPicture => is_non_standard_type(ty, "movie"),
            Kind::MusicalScore => is_non_standard_type(ty, "audio"),
            Kind::Pamphlet => ty == &EntryType::Booklet,
            Kind::PaperConference => ty == &EntryType::InProceedings,
            Kind::Patent => ty == &EntryType::Patent,
            Kind::Performance => false,
            Kind::Periodical => ty == &EntryType::Periodical,
            Kind::PersonalCommunication => is_non_standard_type(ty, "letter"),
            Kind::Post | Kind::PostWeblog => ty == &EntryType::Online,
            Kind::Regulation => false,
            Kind::Report => ty == &EntryType::Report,
            Kind::Review | Kind::ReviewBook => is_non_standard_type(ty, "review"),
            Kind::Software => ty == &EntryType::Software,
            Kind::Song => is_non_standard_type(ty, "music"),
            Kind::Speech => false,
            Kind::Standard => false,
            Kind::Thesis => ty == &EntryType::Thesis,
            Kind::Treaty => is_non_standard_type(ty, "legal"),
            Kind::Webpage => ty == &EntryType::Online,
        }
    }

    fn is_english(&self) -> Option<bool> {
        if let Ok(PermissiveType::Typed(id)) = self.0.langid() {
            let id: LanguageIdentifier = id.into();
            Some(id.language == "en")
        } else {
            None
        }
    }

    fn key(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.0.key)
    }
}

impl Entry {
    fn editors_with_role(&self, role: EditorType) -> Option<Vec<biblatex::Person>> {
        let mut result = vec![];
        if let Ok(eds_with_roles) = self.0.editors() {
            for (eds, r) in eds_with_roles.iter() {
                if r == &role {
                    for ed in eds {
                        result.push(ed.clone())
                    }
                }
            }
        }
        Some(result)
    }
}

fn is_non_standard_type(ty: &EntryType, label: &str) -> bool {
    if let EntryType::Unknown(s) = ty
        && s.to_lowercase() == label
    {
        true
    } else {
        false
    }
}

fn permissive_to_mby_numeric(
    edition_or_volume: &PermissiveType<i64>,
) -> MaybeTyped<Numeric> {
    match edition_or_volume {
        PermissiveType::Typed(i) => MaybeTyped::Typed(Numeric::new(*i as i32)),
        PermissiveType::Chunks(c) => {
            MaybeTyped::infallible_from_str(&c.format_verbatim())
        }
    }
}

fn chunks_to_mby_numeric(chunks: &[Spanned<Chunk>]) -> MaybeTyped<Numeric> {
    let verb = chunks.format_verbatim();
    MaybeTyped::infallible_from_str(&verb)
}

fn chunks_to_fmt_str(chunks: &[Spanned<Chunk>]) -> FormatString {
    FormatString { value: chunks_to_chunked_str(chunks), short: None }
}

fn chunks_to_chunked_str(chunks: &[Spanned<Chunk>]) -> ChunkedString {
    let mut res = ChunkedString::new();
    for chunk in chunks {
        match &chunk.v {
            Chunk::Normal(s) => res.push_str(s, ChunkKind::Normal),
            Chunk::Verbatim(s) => res.push_str(s, ChunkKind::Verbatim),
            Chunk::Math(s) => res.push_str(s, ChunkKind::Math),
        }
    }
    res
}

fn comma_list(items: &[Vec<Spanned<Chunk>>]) -> FormatString {
    let mut value = ChunkedString::new();
    for (i, entity) in items.iter().enumerate() {
        if i != 0 {
            value.push_str(", ", ChunkKind::Normal);
        }

        let chunked = chunks_to_chunked_str(entity.as_slice());
        value.extend(chunked);
    }

    FormatString { value, short: None }
}
