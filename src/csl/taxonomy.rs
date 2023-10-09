use std::convert::TryFrom;
use std::str::FromStr;

use crate::types::{Date, FormatString, MaybeTyped, Numeric, Person};
use crate::Entry;
use citationberg::taxonomy::{NumberVariable, StandardVariable};
use citationberg::{taxonomy, LongShortForm};

pub(crate) fn resolve_number_variable(
    entry: &Entry,
    variable: NumberVariable,
) -> Option<MaybeTyped<Numeric>> {
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
            .and_then(Entry::volume),
        NumberVariable::CitationNumber => todo!("that's for us to do baby"),
        NumberVariable::CollectionNumber => entry
            .bound_select(
                &select!(
                    (* > ("p":(Book | Anthology | Proceedings)))
                ),
                "p",
            )
            .and_then(Entry::volume),
        NumberVariable::Edition => entry.map(|e| e.edition()),
        NumberVariable::FirstReferenceNoteNumber => todo!("you guessed it, baybee"),
        NumberVariable::Issue => entry.map(|e| e.issue()),
        NumberVariable::Locator => todo!("boy oh boy"),
        NumberVariable::Number => {
            return entry.serial_number().and_then(|s| MaybeTyped::from_str(s).ok())
        }
        NumberVariable::NumberOfPages => entry.page_total(),
        NumberVariable::NumberOfVolumes => entry.volume_total(),
        NumberVariable::Page => entry.page_range(),
        NumberVariable::PageFirst => {
            entry.page_range().and_then(|r| r.range()).map(|r| r.start)
        }
        NumberVariable::PartNumber => entry
            .bound_select(
                &select!(
                    (("e":*) > (Article | Blog | Book | Legislation))
                ),
                "e",
            )
            .and_then(Entry::volume),
        NumberVariable::PrintingNumber => None,
        NumberVariable::Section => None,
        NumberVariable::SupplementNumber => None,
        NumberVariable::Version => entry
            .bound_select(&select!(("e":Repository)), "e")
            .and_then(Entry::serial_number)
            .and_then(|s| MaybeTyped::from_str(s).ok()),
        NumberVariable::Volume => entry.volume(),
    };

    todo!()
}

// Number variables are standard variables.
pub(crate) fn resolve_standard_variable(
    entry: &Entry,
    form: LongShortForm,
    variable: StandardVariable,
) -> Option<&str> {
    match variable {
        StandardVariable::Abstract => None,
        StandardVariable::Annote => None,
        StandardVariable::Archive => entry.map(|e| e.archive()),
        StandardVariable::ArchiveCollection => None,
        StandardVariable::ArchiveLocation => entry.archive_location(),
        StandardVariable::ArchivePlace => None,
        StandardVariable::Authority => entry.organization(),
        StandardVariable::CallNumber => None,
        StandardVariable::CitationKey => return Some(entry.key.as_str()),
        // The spec tells us that the CSL processor may assign this, we do not.
        StandardVariable::CitationLabel => None,
        // Get third-order title first, then second-order title.
        StandardVariable::CollectionTitle => entry
            .parents()
            .iter()
            .find_map(|p| p.map_parents(|e| e.title()))
            .or_else(|| entry.map_parents(|e| e.title())),
        StandardVariable::ContainerTitle => entry.map_parents(|e| e.title()),
        StandardVariable::ContainerTitleShort => None,
        StandardVariable::Dimensions => entry.runtime(),
        StandardVariable::Division => None,
        StandardVariable::DOI => entry.doi(),
        StandardVariable::Event | StandardVariable::EventTitle => entry
            .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
            .and_then(Entry::title),
        StandardVariable::EventPlace => entry
            .bound_select(&select!(* > ("p":(Exhibition | Conference | Misc))), "p")
            .and_then(Entry::location),
        StandardVariable::Genre => None,
        StandardVariable::ISBN => entry.isbn(),
        StandardVariable::ISSN => entry.issn(),
        StandardVariable::Jurisdiction => None,
        StandardVariable::Keyword => None,
        StandardVariable::Language => entry.map(|e| e.language),
        StandardVariable::License => None,
        StandardVariable::Medium => None,
        StandardVariable::Note => entry.note(),
        StandardVariable::OriginalPublisher => None,
        StandardVariable::OriginalPublisherPlace => None,
        StandardVariable::OriginalTitle => None,
        StandardVariable::PartTitle => None,
        StandardVariable::PMCID => None,
        StandardVariable::PMID => None,
        StandardVariable::Publisher => entry.map(|e| e.publisher),
        StandardVariable::PublisherPlace => None,
        StandardVariable::References => None,
        StandardVariable::ReviewedGenre => None,
        StandardVariable::ReviewedTitle => entry.map_parents(|e| e.title()),
        StandardVariable::Scale => None,
        StandardVariable::Source => entry
            .bound_select(&select!(* > ("p":Repository)), "p")
            .and_then(Entry::title),
        StandardVariable::Status => None,
        StandardVariable::Title => entry.title(),
        StandardVariable::TitleShort => None,
        StandardVariable::URL => entry.map(|e| e.url),
        StandardVariable::VolumeTitle => {
            let selector = select!(
                (Anthos > ("p":Anthology)) |
                (Entry  > ("p":*)) |
                (* > ("p":Reference)) |
                (Article > ("p":Proceedings))
            );
            entry.bound_select(&selector, "p").and_then(Entry::title)
        }
        StandardVariable::YearSuffix => todo!("we actually have to generate this"),
    }
    .and_then(|v| <&FormatString>::try_from(v).ok())
    .map(|v: &FormatString| &v.value.to_str())
}

pub(crate) fn resolve_date_variable(
    entry: &Entry,
    variable: taxonomy::DateVariable,
) -> Option<Date> {
    todo!()
}

pub(crate) fn resolve_name_variable(
    entry: &Entry,
    variable: taxonomy::NameVariable,
) -> Option<Vec<&Person>> {
    todo!()
}

pub(crate) fn matches_entry_type(
    entry: &Entry,
    kind: citationberg::taxonomy::Kind,
) -> bool {
    todo!()
}
