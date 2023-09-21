use std::borrow::Cow;
use std::convert::TryFrom;

use crate::types::{Date, Person};
use crate::Entry;
use citationberg::{taxonomy, LongShortForm};

pub struct Numeric {
    value: NumericValue,
    prefix: Option<String>,
    suffix: Option<String>,
}

impl TryFrom<String> for Numeric {
    type Error = &'static str;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl ToString for Numeric {
    fn to_string(&self) -> String {
        todo!()
    }
}

enum NumericValue {
    Number(i32),
    Set(Vec<i32>),
    Range(i32, i32),
}

pub enum MaybeTyped<T> {
    Typed(T),
    String(String),
}

impl<T: ToString> MaybeTyped<T> {
    pub fn to_str(&self) -> Cow<str> {
        match self {
            MaybeTyped::Typed(t) => Cow::Owned(t.to_string()),
            MaybeTyped::String(s) => Cow::Borrowed(s),
        }
    }
}

pub(crate) fn resolve_number_variable(
    entry: &Entry,
    variable: taxonomy::NumberVariable,
) -> Option<MaybeTyped<Numeric>> {
    todo!()
}

// Number variables are standard variables.
pub(crate) fn resolve_standard_variable(
    entry: &Entry,
    form: LongShortForm,
    variable: taxonomy::StandardVariable,
) -> Option<&str> {
    todo!()
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
) -> Option<Vec<Person>> {
    todo!()
}
