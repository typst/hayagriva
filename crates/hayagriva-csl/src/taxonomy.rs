use std::borrow::Cow;
use std::cmp;
use std::str::FromStr;

use citationberg::LongShortForm;
use citationberg::taxonomy::{NumberVariable, PageVariable, StandardVariable};

use hayagriva_core::types::{ChunkedString, MaybeTyped, Numeric, StringChunk};
use hayagriva_core::{EntryLike, PageRanges};

use super::{DisambiguateState, InstanceContext, LocatorPayload};

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
            NumberVariable::Locator => {
                match &self.cite_props.speculative.locator.as_ref()?.1 {
                    &LocatorPayload::Str(l) => Some(NumberVariableResult::from_regular(
                        Numeric::from_str(l)
                            .map(|n| MaybeTyped::Typed(Cow::Owned(n)))
                            .unwrap_or_else(|_| MaybeTyped::String(l.to_owned())),
                    )),
                    LocatorPayload::Transparent(_) => {
                        Some(NumberVariableResult::Transparent(
                            self.cite_props.certain.initial_idx,
                        ))
                    }
                }
            }
            _ => self
                .entry
                .resolve_number_variable(variable)
                .map(NumberVariableResult::from_regular),
        }
    }

    pub(super) fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<PageVariableResult> {
        self.entry.resolve_page_variable(variable)
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

pub(super) type PageVariableResult = MaybeTyped<PageRanges>;

pub(super) enum NumberOrPageVariableResult<'a> {
    Number(NumberVariableResult<'a>),
    Page(PageVariableResult),
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
