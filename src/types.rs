use std::cmp::{Ordering, PartialOrd};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::Range;
use std::ops::{Add, Sub};

use super::{Entry, EntryAccessError, FieldTypes};

use chrono::{Datelike, NaiveDate};
use lazy_static::lazy_static;
use regex::Regex;
use strum_macros::EnumString;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;
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

/// Describes which kind of work a database entry refers to.
#[derive(Copy, Clone, Debug, EnumString, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum EntryType {
    /// A short text, possibly of journalistic or scientific nature,
    /// appearing in some greater publication.
    Article,
    /// A section of a greater containing work.
    Chapter,
    /// A short segment of media pertaining to some subject matter.
    /// Could appear in a work of reference or a data set.
    Entry,
    /// Text published within an Anthology.
    InAnthology,
    /// A document compiled by authors that may be affiliated to an organisation.
    /// Presents information for a specific audience or purpose.
    Report,
    /// Scholarly work delivered to fulfill degree requirements at a higher
    /// education institution.
    Thesis,
    /// Piece of content that can be found on the internet and is native
    /// to the medium, like an animation, a web app, or a form of content not
    /// found elsewhere. Do not use this entry type when
    /// referencing a textual blog article, instead use an `Article` with
    /// a `Blog` parent.
    WebItem,
    /// A part of a show or another type of performed media, typically all
    /// taking place in the same location.
    Scene,
    /// A form of artistic / creative expression.
    Artwork,
    /// A technical document deposited at a government agency that describes
    /// an invention in order to legally limit the rights of reproduction
    /// to the inventors.
    Patent,
    /// Reference to a legal case that was or is to be heared at a court of law.
    Case,
    /// The issue of a newspaper that was published on a given day.
    NewspaperIssue,
    /// Legal document or draft there of that is, is to be, or was to be
    /// enacted into binding law.
    Legislation,
    /// Written document that is submitted as a candidate for publication.
    Manuscript,
    /// A post on a micro-blogging platform like Twitter.
    Tweet,
    /// Items that do not match any of the other Entry type composites.
    Misc,
    /// A publication that periodically publishes issues with unique content.
    /// This includes scientific journals and news magazines.
    Periodical,
    /// The official published record of the subjects
    /// of a professional conference.
    Proceedings,
    /// Long-form work published pysically as a set of bound sheets.
    Book,
    /// Set of self-published articles on a website.
    Blog,
    /// A work of reference. This could be a manual or a dictionary.
    Reference,
    /// Professional conference. This Entry type implies that the item
    /// referenced has been an event at the conference itself. If you instead
    /// want to reference a paper published in the published proceedings
    /// of the conference, use an `Article` with a `Proceedings` parent.
    Conference,
    /// Collection of different texts pertaining to a single topic.
    Anthology,
    /// Publicly visible storage of the source code for a particular software
    /// and its modifications over time.
    Repository,
    /// Written discussion on the internet triggered by an original post.
    /// Could be on a forum, social network, or Q&A site.
    Thread,
    /// Motion picture of any form, possibly with accompanying audio.
    Video,
    /// Recorded audible sound of any kind.
    Audio,
    /// A curated set of artworks.
    Exhibition,
}

/// Allows to formalize requirements for a single `EntryType`.
#[derive(Clone, Debug)]
pub(crate) enum EntryTypeModality {
    /// `EntryType` must match exactly.
    Specific(EntryType),
    /// `EntryType` must be contained within this list.
    Alternate(Vec<EntryType>),
    /// `EntryType` must _not_ be contained within this list.
    Disallowed(Vec<EntryType>),
    /// Any `EntryType is allowed`.
    Any,
}

impl EntryType {
    /// Checks if the `EntryTypeModality` constraint is satisfied.
    pub(crate) fn check(&self, constraint: EntryTypeModality) -> bool {
        match constraint {
            EntryTypeModality::Specific(tp) => &tp == self,
            EntryTypeModality::Alternate(tps) => {
                let mut found = false;
                for tp in &tps {
                    if tp == self {
                        found = true;
                        break;
                    }
                }

                found
            }
            EntryTypeModality::Disallowed(tps) => {
                !self.check(EntryTypeModality::Alternate(tps))
            }
            EntryTypeModality::Any => true,
        }
    }

    pub(crate) fn default_parent(&self) -> Self {
        match self {
            Self::Article => Self::Periodical,
            // Self::ConferencePaper => Self::Proceedings,
            Self::Chapter => Self::Book,
            Self::Entry => Self::Reference,
            Self::InAnthology => Self::Anthology,
            Self::WebItem => Self::WebItem,
            Self::Scene => Self::Video,
            Self::Artwork => Self::Exhibition,
            Self::Legislation => Self::Anthology,
            Self::Tweet => Self::Tweet,
            Self::Video => Self::Video,
            _ => Self::Misc,
        }
    }
}

/// Specifies the types of an entry, including its parents.
#[derive(Clone, Debug)]
pub(crate) struct EntryTypeSpec {
    /// The top-level type has to satisfy these conditions.
    pub(crate) here: EntryTypeModality,
    /// For each entry in the list, there must be an according parent.
    pub(crate) parents: Vec<EntryTypeSpec>,
}

impl EntryTypeSpec {
    pub fn new(here: EntryTypeModality, parents: Vec<EntryTypeSpec>) -> Self {
        Self { here, parents }
    }

    pub fn with_single(here: EntryType) -> Self {
        Self {
            here: EntryTypeModality::Specific(here),
            parents: vec![],
        }
    }

    pub fn with_parents(here: EntryType, parents: Vec<EntryTypeSpec>) -> Self {
        Self {
            here: EntryTypeModality::Specific(here),
            parents,
        }
    }

    pub fn single_parent(here: EntryTypeModality, parent: EntryTypeModality) -> Self {
        Self {
            here,
            parents: vec![Self::new(parent, vec![])],
        }
    }
}

#[derive(Clone, Debug, EnumString, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum PersonRole {
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
    ExecutiveProducer,
    Writer,
    Cinematography,
    Director,
    Illustrator,

    #[strum(disabled)]
    Unknown(String),
}

#[derive(Clone, Debug)]
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
        if prefix.is_some() {
            name = name.trim_start().to_string();
        }

        Ok(Person {
            name,
            given_name,
            prefix,
            suffix,
            alias: None,
        })
    }

    /// Formats the given name into initials, `"Judith Beatrice"`
    /// would yield `"J. B."` if the `delimiter` argument is set to
    /// `Some(".")`, `"Klaus-Peter"` would become `"K-P"` without a delimiter.
    pub fn get_initials(&self, delimiter: Option<&str>) -> Option<String> {
        if let Some(gn) = &self.given_name {
            let mut collect = true;
            let mut letters = vec![];
            let mut seps = vec![];

            for (_, gr) in gn.grapheme_indices(true) {
                if let Some(c) = gr.chars().next() {
                    if c.is_whitespace() || c == '-' {
                        collect = true;
                        seps.push(c);
                        continue;
                    }
                }

                if collect {
                    letters.push(gr);
                    collect = false;
                }
            }

            let mut res = String::new();
            for (i, e) in letters.into_iter().enumerate() {
                if i != 0 {
                    res.push(seps[i - 1]);
                }
                res += e;
                if let Some(delimiter) = delimiter {
                    res += delimiter;
                }
            }

            Some(res)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub enum NumOrStr {
    Number(i64),
    Str(String),
}

impl Display for NumOrStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Number(i) => write!(f, "{}", i),
            Self::Str(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Date {
    pub year: i32,
    pub month: Option<u8>,
    pub day: Option<u8>,
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

#[derive(Clone, Debug)]
pub struct FormattableString {
    pub(crate) value: String,
    pub(crate) title_case: Option<String>,
    pub(crate) sentence_case: Option<String>,
    pub(crate) verbatim: bool,
}

#[derive(Clone, Debug)]
pub struct FormattedString {
    pub value: String,
    pub title_case: String,
    pub sentence_case: String,
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

#[derive(Clone, Debug)]
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

macro_rules! try_from_fieldtypes {
    ($variant:ident, $target:ty $(,)*) => {
        impl TryFrom<FieldTypes> for $target {
            type Error = EntryAccessError;

            fn try_from(value: FieldTypes) -> Result<Self, Self::Error> {
                match value {
                    FieldTypes::$variant(f) => Ok(f),
                    _ => Err(EntryAccessError::WrongType),
                }
            }
        }

        impl From<$target> for FieldTypes {
            fn from(value: $target) -> Self {
                FieldTypes::$variant(value)
            }
        }
    };
}

try_from_fieldtypes!(FormattableString, FormattableString);
try_from_fieldtypes!(FormattedString, FormattedString);
try_from_fieldtypes!(Text, String);
try_from_fieldtypes!(Integer, i64);
try_from_fieldtypes!(Date, Date);
try_from_fieldtypes!(Persons, Vec<Person>);
try_from_fieldtypes!(PersonsWithRoles, Vec<(Vec<Person>, PersonRole)>);
try_from_fieldtypes!(IntegerOrText, NumOrStr);
try_from_fieldtypes!(Range, std::ops::Range<i64>);
try_from_fieldtypes!(Duration, Duration);
try_from_fieldtypes!(TimeRange, std::ops::Range<Duration>);
try_from_fieldtypes!(Url, QualifiedUrl);
try_from_fieldtypes!(Language, unic_langid::LanguageIdentifier);
try_from_fieldtypes!(Entries, Vec<Entry>);

#[cfg(test)]
mod tests {
    use super::Person;

    #[test]
    fn person_initials() {
        let p = Person::from_strings(&vec!["Dissmer", "Courtney Deliah"]).unwrap();
        assert_eq!("C. D.", p.get_initials(Some(".")).unwrap());
        let p = Person::from_strings(&vec!["Günther", "Hans-Joseph"]).unwrap();
        assert_eq!("H-J", p.get_initials(None).unwrap());
    }
}
