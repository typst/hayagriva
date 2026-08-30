//! Hayagriva's core.
//!
//! This crate contains data types, traits, and functions used for the
//! different formats and rendering crates of Hayagriva.

use std::borrow::Cow;

use citationberg::LongShortForm;
use citationberg::taxonomy;
use citationberg::taxonomy::{
    DateVariable, NameVariable, NumberVariable, PageVariable, StandardVariable,
};

pub use types::*;

pub mod lang;
pub mod types;
#[doc(hidden)]
pub mod util;

/// Defines an entry for CSL rendering. Implementing this trait enables
/// interaction between an input format like Hayagriva or CSL-JSON and the CSL
/// rendering implementation.
pub trait EntryLike {
    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<MaybeTyped<Cow<'_, Numeric>>>;
    fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<MaybeTyped<PageRanges>>;
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, ChunkedString>>;
    fn resolve_name_variable(&self, variable: NameVariable) -> Vec<Cow<'_, Person>>;
    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>>;
    /// `true` if the entry can be interpreted as the given `kind`. As most
    /// formats do not have a perfect correspondance, this check is usually
    /// "best-effort" and optimistic.
    fn matches_entry_type(&self, kind: taxonomy::Kind) -> bool;
    /// Returns `None` if the entry's language is unknown, `Some(true)` if it
    /// is known to be English, and `Some(false)` otherwise.
    fn is_english(&self) -> Option<bool>;
    /// The entry's key in its containing library.
    fn key(&self) -> Cow<'_, str>;
}
