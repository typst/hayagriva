#[cfg(feature = "biblatex")]
pub mod biblatex_conversion;
pub mod lang;
pub mod test_util;
pub mod types;
pub mod util;

use std::borrow::Cow;

use citationberg::{
    LongShortForm,
    taxonomy::{
        self, DateVariable, NameVariable, NumberVariable, PageVariable, StandardVariable,
    },
};
pub use types::*;

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
    fn matches_entry_type(&self, kind: taxonomy::Kind) -> bool;
    fn is_english(&self) -> Option<bool>;
    fn key(&self) -> Cow<'_, str>;
}
