use chrono::{Datelike, NaiveDate};
use lazy_static::lazy_static;
use regex::Regex;
use std::cmp::{Ordering, PartialOrd};
use std::ops::Range;
use std::ops::{Add, Sub};
use strum_macros::EnumString;
use thiserror::Error;
use url::Url;

#[rustfmt::skip]
lazy_static! {
    // Range regex (like `5 -- 7`).
    static ref RANGE_REGEX: Regex = Regex::new(r"^(?P<s>(\+|-)?\s*\d+)(\s*-+\s*(?P<e>(\+|-)?\s*\d+))?").unwrap();

    // Duration regexes.
    static ref DURATION_REGEX: Regex = Regex::new(r"^(((?P<d>\d+)\s*:\s*)?(?P<h>\d{2,})\s*:\s*)?(?P<m>\d{2,})\s*:\s*(?P<s>\d{2})(\s*,\s*(?P<ms>\d+))?").unwrap();
    static ref DURATION_RANGE_REGEX: Regex = Regex::new(r"^(?P<s>((\d+\s*:\s*)?\d{2,}\s*:\s*)?\d{2,}\s*:\s*\d{2}(\s*,\s*\d+)?)(\s*-+\s*(?P<e>((\d+\s*:\s*)?\d{2,}\s*:\s*)?\d{2,}\s*:\s*\d{2}(\s*,\s*\d+)?))?").unwrap();

    // Definite (i.e. non-range) date regexes.
    static ref MONTH_REGEX: Regex = Regex::new(r"^(?P<y>(\+|-)?\s*\d{4})\s*-\s*(?P<m>\d{2})").unwrap();
    static ref YEAR_REGEX: Regex = Regex::new(r"^(?P<y>(\+|-)?\s*\d{4})").unwrap();
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "lowercase")]
pub(crate) enum PersonRole {
    Translator,
    Afterword,
    Foreword,
    Introduction,
    Annotator,
    Commentator,
    Holder,
    Compiler,
    Founder,
    Collaborator,
    Organizer,
    CastMember,
    Composer,
    Producer,
    Writer,
    Cinematography,
    Director,

    #[strum(disabled)]
    Unknown(String),
}

#[derive(Debug)]
pub struct Person {
    pub name: String,
    pub given_name: Option<String>,
    pub prefix: Option<String>,
    pub suffix: Option<String>,
    pub alias: Option<String>,
}

#[derive(Clone, Debug, Error)]
pub enum PersonError {
    #[error("too many parts")]
    TooManyParts,
    #[error("part list is empty")]
    Empty,
}

impl Person {
    pub fn from_strings(parts: &[&str]) -> Result<Self, PersonError> {
        if parts.is_empty() {
            return Err(PersonError::Empty);
        } else if parts.len() > 3 {
            return Err(PersonError::TooManyParts);
        }

        let parts: Vec<&str> = parts.into_iter().map(|s| s.trim()).collect();

        let last_pre = parts[0];
        let given_name = if parts.len() > 1 {
            Some(parts.last().unwrap().to_string())
        } else {
            None
        };

        let suffix = if parts.len() > 2 {
            Some(parts[1].to_string())
        } else {
            None
        };

        let mut word_start = true;
        let mut last_lower_case_end: i32 = -1;
        let mut is_lowercase = false;
        let mut last_word_start = 0;
        let mut has_seen_uppercase_words = false;

        for (index, c) in last_pre.chars().enumerate() {
            if c.is_whitespace() {
                word_start = true;
                continue;
            }

            if word_start {
                last_word_start = index;

                if c.is_lowercase() {
                    is_lowercase = true;
                } else {
                    is_lowercase = false;
                    has_seen_uppercase_words = true;
                }
            }

            if is_lowercase {
                last_lower_case_end = index as i32;
            }

            word_start = false;
        }

        let mut name = String::new();
        let mut prefix = String::new();
        for (index, c) in last_pre.chars().enumerate() {
            if (index as i32 <= last_lower_case_end && has_seen_uppercase_words)
                || (!has_seen_uppercase_words && index < last_word_start)
            {
                prefix.push(c);
            } else if has_seen_uppercase_words || index >= last_word_start {
                name.push(c);
            }
        }

        let prefix = if prefix.is_empty() { None } else { Some(prefix) };

        Ok(Person {
            name,
            given_name,
            prefix,
            suffix,
            alias: None,
        })
    }
}

#[derive(Debug)]
pub(crate) enum NumOrStr {
    Number(i64),
    Str(String),
}

#[derive(Debug)]
pub struct Date {
    year: i32,
    month: Option<u8>,
    day: Option<u8>,
}

#[derive(Clone, Debug, Error)]
pub enum DateError {
    #[error("date format unknown")]
    UnknownFormat,
    #[error("month not in interval 1-12")]
    MonthOutOfBounds,
}

impl Date {
    /// Parse a date atom from a string.
    pub fn from_str(source: &str) -> Result<Self, DateError> {
        let mut source = source.to_string();
        source.retain(|f| !f.is_whitespace());

        let full_date = source.parse::<NaiveDate>();

        if let Ok(ndate) = full_date {
            Ok(Self {
                year: ndate.year(),
                month: Some(ndate.month0() as u8),
                day: Some(ndate.day0() as u8),
            })
        } else if let Some(captures) = MONTH_REGEX.captures(&source) {
            let month = (captures.name("m").unwrap()).as_str().parse::<u8>().unwrap() - 1;
            if month > 11 {
                Err(DateError::MonthOutOfBounds)
            } else {
                Ok(Self {
                    year: (captures.name("y").unwrap()).as_str().parse().unwrap(),
                    month: Some(month),
                    day: None,
                })
            }
        } else if let Some(captures) = YEAR_REGEX.captures(&source) {
            Ok(Self {
                year: (captures.name("y").unwrap()).as_str().parse().unwrap(),
                month: None,
                day: None,
            })
        } else {
            Err(DateError::UnknownFormat)
        }
    }

    pub fn from_year(year: i32) -> Self {
        Self { year, month: None, day: None }
    }
}

#[derive(Debug)]
pub(crate) struct FormattableString {
    value: String,
    title_case: Option<String>,
    sentence_case: Option<String>,
    verbatim: bool,
}

impl FormattableString {
    pub fn new(
        value: String,
        title_case: Option<String>,
        sentence_case: Option<String>,
        verbatim: bool,
    ) -> Self {
        Self {
            value,
            title_case,
            sentence_case,
            verbatim,
        }
    }

    pub fn new_shorthand(value: String) -> Self {
        Self {
            value,
            title_case: None,
            sentence_case: None,
            verbatim: false,
        }
    }

    pub fn new_verbatim(value: String, verbatim: bool) -> Self {
        Self {
            value,
            verbatim,
            title_case: None,
            sentence_case: None,
        }
    }
}

#[derive(Debug)]
pub struct QualifiedUrl {
    pub value: Url,
    pub visit_date: Option<Date>,
}

pub fn get_range(source: &str) -> Option<Range<i64>> {
    RANGE_REGEX.captures(source).map(|caps| {
        let start: i64 = str::parse(caps.name("s").expect("start is mandatory").as_str())
            .expect("Only queried for digits");
        let end: i64 = caps
            .name("e")
            .map(|v| str::parse(v.as_str()).expect("Only queried for digits"))
            .unwrap_or(start);

        start .. end
    })
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Duration {
    days: u32,
    hours: u32,
    minutes: u32,
    seconds: u8,
    milliseconds: f64,
}

#[derive(Clone, Error, Debug)]
pub enum DurationError {
    #[error("string does not match duration regex")]
    NoMatch,
    #[error("out of bounds value when greater order value is specified")]
    TooLarge,
}

impl Duration {
    fn as_ms(&self) -> f64 {
        self.milliseconds
            + self.seconds as f64 * 1000.0
            + self.minutes as f64 * 6000.0
            + self.hours as f64 * 36000.0
            + self.days as f64 * 864000.0
    }

    fn from_ms(mut ms: f64) -> Self {
        let days = (ms / 864000.0) as u32;
        ms -= days as f64 * 864000.0;

        let hours = (ms / 36000.0) as u32;
        ms -= hours as f64 * 36000.0;

        let minutes = (ms / 6000.0) as u32;
        ms -= minutes as f64 * 6000.0;

        let seconds = (ms / 1000.0) as u8;
        ms -= seconds as f64 * 1000.0;

        Self {
            days,
            hours,
            minutes,
            seconds,
            milliseconds: ms,
        }
    }

    pub fn from_str(source: &str) -> Result<Self, DurationError> {
        let capt =
            DURATION_REGEX.captures(source.trim()).ok_or(DurationError::NoMatch)?;

        let seconds: u8 = capt.name("s").unwrap().as_str().parse().unwrap();
        let mut minutes: u32 = capt.name("m").unwrap().as_str().parse().unwrap();

        if seconds > 59 {
            return Err(DurationError::TooLarge);
        }

        let ms: Option<f64> = capt.name("ms").and_then(|m| m.as_str().parse().ok());
        let hours: Option<u32> = capt.name("h").and_then(|m| m.as_str().parse().ok());
        let days: Option<u32> = capt.name("d").and_then(|m| m.as_str().parse().ok());

        if hours.is_some() && minutes > 59 {
            return Err(DurationError::TooLarge);
        }

        let mut hours = hours.unwrap_or_else(|| {
            let res = minutes / 60;
            minutes = minutes % 60;
            res
        });

        if days.is_some() && hours > 23 {
            return Err(DurationError::TooLarge);
        }

        let days = days.unwrap_or_else(|| {
            let res = hours / 24;
            hours = hours % 24;
            res
        });

        Ok(Self {
            days,
            hours,
            minutes,
            seconds,
            milliseconds: ms.unwrap_or(0.0),
        })
    }

    pub fn range_from_str(source: &str) -> Result<std::ops::Range<Self>, DurationError> {
        let caps = DURATION_RANGE_REGEX
            .captures(source.trim())
            .ok_or(DurationError::NoMatch)?;

        let start = Self::from_str(caps.name("s").expect("start is mandatory").as_str())?;
        let end = caps
            .name("e")
            .map(|e| Self::from_str(e.as_str()))
            .unwrap_or_else(|| Ok(start.clone()))?;

        Ok(start .. end)
    }
}

impl PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let ord = self
            .days
            .cmp(&other.days)
            .then_with(|| self.hours.cmp(&other.hours))
            .then_with(|| self.minutes.cmp(&other.minutes))
            .then_with(|| self.seconds.cmp(&other.seconds));

        if ord == Ordering::Equal {
            self.milliseconds.partial_cmp(&other.milliseconds)
        } else {
            Some(ord)
        }
    }
}

impl Sub for Duration {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Duration::from_ms(self.as_ms() - other.as_ms())
    }
}

impl Add for Duration {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Duration::from_ms(self.as_ms() + other.as_ms())
    }
}
