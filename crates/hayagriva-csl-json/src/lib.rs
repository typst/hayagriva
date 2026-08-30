//! Hayagriva CSL-JSON handling.
//!
//! This crate contains a wrapper around [citationberg]'s [csl_json::Item] and
//! handles the resolution of CSL variables from [Item].

use std::{borrow::Cow, str::FromStr};

use citationberg::taxonomy::{
    DateVariable, Kind, NameVariable, NumberVariable, PageVariable, StandardVariable,
};
use citationberg::{LongShortForm, json as csl_json};
use serde::{Deserialize, Serialize};

use hayagriva_core::{
    ChunkedString, Date, EntryLike, MaybeTyped, Numeric, PageRanges, Person, StringChunk,
};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(transparent)]
/// A CSL-JSON item. It is a wrapper around a [csl_json::Item].
pub struct Item(pub csl_json::Item);

fn resolve_csl_json_standard_variable(
    item: &Item,
    variable: StandardVariable,
) -> Option<Cow<'_, ChunkedString>> {
    match item.0.0.get(&variable.to_string())? {
        csl_json::Value::String(s) => {
            Some(Cow::Owned(StringChunk::normal(s.clone()).into()))
        }
        csl_json::Value::Number(n) => {
            Some(Cow::Owned(StringChunk::normal(n.to_string()).into()))
        }
        _ => None,
    }
}

impl EntryLike for Item {
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: StandardVariable,
    ) -> Option<Cow<'_, ChunkedString>> {
        match variable {
            StandardVariable::Title => match form {
                LongShortForm::Short => {
                    // Per citeproc tests, a 'title-short' without 'title' is
                    // valid and should be used when the short form is
                    // selected.
                    resolve_csl_json_standard_variable(self, StandardVariable::TitleShort)
                        .or_else(|| {
                            resolve_csl_json_standard_variable(
                                self,
                                StandardVariable::Title,
                            )
                        })
                }
                LongShortForm::Long => {
                    resolve_csl_json_standard_variable(self, StandardVariable::Title)
                }
            },
            _ => resolve_csl_json_standard_variable(self, variable),
        }
    }

    fn resolve_date_variable(&self, variable: DateVariable) -> Option<Cow<'_, Date>> {
        match self.0.0.get(&variable.to_string())? {
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
                    approximate: d.circa,
                    season: d.season,
                }))
            }
            _ => None,
        }
    }

    fn resolve_name_variable(&self, variable: NameVariable) -> Vec<Cow<'_, Person>> {
        match self.0.0.get(&variable.to_string()) {
            Some(csl_json::Value::Names(names)) => names
                .iter()
                .filter_map(|name| {
                    Some(Cow::Owned(match name {
                        csl_json::NameValue::Literal(l) => Person {
                            name: l.literal.clone(),
                            prefix: None,
                            suffix: None,
                            given_name: None,
                            alias: None,
                            comma_suffix: false,
                        },
                        csl_json::NameValue::Item(csl_json::NameItem {
                            family,
                            given,
                            non_dropping_particle: None,
                            dropping_particle: None,
                            suffix,
                            comma_suffix,
                        }) => {
                            let mut parts = vec![family.as_str()];
                            if let Some(given) = given {
                                parts.push(given.as_str());
                            }
                            let mut p = Person::from_strings(parts).ok()?;
                            if let Some(suffix) = suffix {
                                p.suffix = Some(suffix.as_str().to_owned());
                            }
                            if comma_suffix.unwrap_or_default() {
                                p.comma_suffix = true;
                            }

                            p
                        }
                        csl_json::NameValue::Item(csl_json::NameItem {
                            family,
                            given,
                            non_dropping_particle,
                            dropping_particle,
                            suffix,
                            comma_suffix,
                        }) => Person {
                            name: if let Some(non_drop) = non_dropping_particle {
                                format!("{non_drop} {family}")
                            } else {
                                family.clone()
                            },
                            prefix: dropping_particle.clone(),
                            suffix: suffix.clone(),
                            given_name: given.clone(),
                            alias: None,
                            comma_suffix: comma_suffix.unwrap_or_default(),
                        },
                    }))
                })
                .collect(),
            _ => vec![],
        }
    }

    fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<MaybeTyped<PageRanges>> {
        match variable {
            PageVariable::Page => match self.0.0.get("page")? {
                &csl_json::Value::Number(n) => {
                    // Page ranges use i32 internally, so we check whether the
                    // number is in range.
                    Some(match i32::try_from(n) {
                        Ok(n) => MaybeTyped::Typed(PageRanges::from(n)),
                        // If the number is not in range, we degrade to a
                        // string, which disables some CSL features.
                        Err(_) => MaybeTyped::String(n.to_string()),
                    })
                }
                csl_json::Value::String(s) => {
                    let res = MaybeTyped::<PageRanges>::infallible_from_str(s);
                    Some(match res {
                        MaybeTyped::String(s) => MaybeTyped::String(s),
                        MaybeTyped::Typed(r) => MaybeTyped::Typed(r),
                    })
                }
                _ => None,
            },
        }
    }

    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<MaybeTyped<Cow<'_, Numeric>>> {
        if matches!(variable, NumberVariable::PageFirst)
            && let Some(MaybeTyped::Typed(n)) =
                self.resolve_page_variable(PageVariable::Page)
        {
            return n.first().map(|r| MaybeTyped::Typed(Cow::Owned(r.clone())));
        }
        match self.0.0.get(&variable.to_string())? {
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

    fn matches_entry_type(&self, kind: Kind) -> bool {
        let Some(actual) = self.0.0.get("type") else {
            return false;
        };
        let Some(string) = actual.to_str() else {
            return false;
        };
        Kind::from_str(&string) == Ok(kind)
    }

    fn is_english(&self) -> Option<bool> {
        self.0
            .0
            .get("language")
            .and_then(|l| l.to_str())
            .map(|l| l.starts_with("en"))
    }

    fn key(&self) -> Cow<'_, str> {
        self.0.id().unwrap_or_default()
    }
}
