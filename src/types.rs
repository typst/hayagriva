//! Base types for the bibliography items and their content.

use std::cmp::{Ordering, PartialOrd};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::Range;
use std::ops::{Add, Sub};

use super::{Entry, FieldType};

use chrono::{Datelike, NaiveDate};
use lazy_static::lazy_static;
use regex::Regex;
use strum_macros::EnumString;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;
use url::{Host, Url};

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
    Anthos,
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
    Web,
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
    Newspaper,
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

impl Entry {
    /// Extract the twitter handle for the nth author from their alias.
    /// Will make sure the handle starts with `@`.
    ///
    /// If the `user_index` is 0, the function will try to extract
    /// the handle from the URL.
    pub(crate) fn get_twitter_handle(&self, user_index: usize) -> Option<String> {
        if self.entry_type != EntryType::Tweet {
            return None;
        }

        let authors = self.get_authors();

        if user_index > 0 && user_index >= authors.len() {
            return None;
        }

        if let Some(alias) = &authors[user_index].alias {
            return if alias.chars().next() == Some('@') {
                Some(alias.clone())
            } else {
                Some(format!("@{}", alias))
            };
        }

        if user_index == 0 {
            if let Some(url) = self.get_url().map(|u| &u.value) {
                if url.host() != Some(Host::Domain("twitter.com")) {
                    return None;
                }

                if let Some(handle) = url.path_segments().and_then(|mut c| c.next()) {
                    return Some(format!("@{}", handle));
                }
            }
        }

        None
    }
}

impl EntryType {
    pub(crate) fn default_parent(&self) -> Self {
        match self {
            Self::Article => Self::Periodical,
            // Self::ConferencePaper => Self::Proceedings,
            Self::Chapter => Self::Book,
            Self::Entry => Self::Reference,
            Self::Anthos => Self::Anthology,
            Self::Web => Self::Web,
            Self::Scene => Self::Video,
            Self::Artwork => Self::Exhibition,
            Self::Legislation => Self::Anthology,
            Self::Tweet => Self::Tweet,
            Self::Video => Self::Video,
            _ => Self::Misc,
        }
    }
}

/// Specifies the role a group of persons had in the creation to the
/// cited item.
#[derive(Clone, Debug, EnumString, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum PersonRole {
    /// Translated the work from a foreign language to the cited edition.
    Translator,
    /// Authored an afterword.
    Afterword,
    /// Authored an foreword.
    Foreword,
    /// Authored an introduction.
    Introduction,
    /// Provided value-adding annotations.
    Annotator,
    /// Commented the work.
    Commentator,
    /// Holds a patent or similar.
    Holder,
    /// Compiled the works in an [Anthology](EntryType::Anthology).
    Compiler,
    /// Founded the publication.
    Founder,
    /// Collaborated on the cited item.
    Collaborator,
    /// Organized the creation of the cited item.
    Organizer,
    /// Performed in the cited item.
    CastMember,
    /// Composed all or parts of the cited item's musical / audible components.
    Composer,
    /// Produced the cited item.
    Producer,
    /// Lead Producer for the cited item.
    ExecutiveProducer,
    /// Did the writing for the cited item.
    Writer,
    /// Shot film/video for the cited item.
    Cinematography,
    /// Directed the cited item.
    Director,
    /// Illustrated the cited item.
    Illustrator,
    /// Provided narration or voice-over for the cited item.
    Narrator,

    /// Various other roles described by the contained string.
    #[strum(disabled)]
    Unknown(String),
}

/// Holds the name of a person.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Person {
    /// The family name.
    pub name: String,
    /// The given name / forename.
    pub given_name: Option<String>,
    /// A prefix of the family name such as 'van' or 'de'.
    pub prefix: Option<String>,
    /// A suffix of the family name such as 'Jr.' or 'IV'.
    pub suffix: Option<String>,
    /// Another name (often user name) the person might be known under.
    pub alias: Option<String>,
}

/// Error that may occur when parsing a slice of strings as a name.
#[derive(Clone, Debug, Error)]
pub enum PersonError {
    /// The name has too many parts to be appropriately parsed.
    #[error("too many parts")]
    TooManyParts,
    /// The name is empty.
    #[error("part list is empty")]
    Empty,
}

impl Person {
    /// This function expects a `parts` &str slice of the length between one
    /// and three. The first part will be interpreted as the <prefix> <Name>,
    /// the second part as the given name and the third part as the suffix.
    ///
    /// The prefix and name are seperated just like in BiBTeX, as described
    /// [Nicolas Markey describes in "Tame the BeaST"][taming], p. 24. The
    /// gist is that the given name will start at the first word with a capital
    /// letter, if there are any such words.
    ///
    /// The call site of this function in the library obtains the slice by
    /// calling `split(",")` on a string like `"Des Egdens, Britta"`.
    ///
    /// [taming]: https://ftp.rrze.uni-erlangen.de/ctan/info/bibtex/tamethebeast/ttb_en.pdf

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

    /// Get the name with the family name fist, the initials
    /// afterwards, seperated by a comma.
    pub fn get_name_first(&self, initials: bool, prefix_given_name: bool) -> String {
        let mut res = if !prefix_given_name {
            if let Some(prefix) = &self.prefix {
                format!("{} {}", prefix, self.name)
            } else {
                self.name.clone()
            }
        } else {
            self.name.clone()
        };

        if initials {
            if let Some(initials) = self.get_initials(Some(".")) {
                res += ", ";
                res += &initials;
            }
        } else if let Some(given_name) = self.given_name.clone() {
            res += ", ";
            res += &given_name;
        }

        if prefix_given_name {
            if let Some(prefix) = &self.prefix {
                if self.given_name.is_some() {
                    res.push(' ');
                }

                res += prefix;
            }
        }


        if let Some(suffix) = &self.suffix {
            res += ", ";
            res += suffix;
        }
        res
    }

    /// Get the name with the initials fist, the family name
    /// afterwards, seperated by a comma.
    pub fn get_given_name_initials_first(&self, initials: bool) -> String {
        let mut res = if initials {
            if let Some(initials) = self.get_initials(Some(".")) {
                format!("{} ", initials)
            } else {
                String::new()
            }
        } else if let Some(given_name) = self.given_name.clone() {
            format!("{} ", given_name)
        } else {
            String::new()
        };

        if let Some(prefix) = &self.prefix {
            res += prefix;
            res += " ";
        }

        res += &self.name;

        if let Some(suffix) = &self.suffix {
            res += " ";
            res += suffix;
        }

        res
    }
}

/// A value that could be either a number or a string.
#[derive(Clone, Debug, PartialEq)]
pub enum NumOrStr {
    /// It's a number!
    Number(i64),
    /// It's a string!
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

impl Into<String> for NumOrStr {
    fn into(self) -> String {
        self.to_string()
    }
}

/// A date that can be as coarse as a year and as fine-grained as a day.
#[derive(Clone, Debug, PartialEq)]
pub struct Date {
    /// The year (1 B.C.E. is represented as 0 and so forth).
    pub year: i32,
    /// The optional month (0-11).
    pub month: Option<u8>,
    /// The optional day (0-30).
    pub day: Option<u8>,
}

/// This error can occur when trying to get a date from a string.
#[derive(Clone, Debug, Error)]
pub enum DateError {
    /// The string does not conform to the date interval.
    #[error("date format unknown")]
    UnknownFormat,
    /// The month is out of bounds.
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

    /// Get a date from an integer.
    pub fn from_year(year: i32) -> Self {
        Self { year, month: None, day: None }
    }

    /// Prints the year as a human-readable gregorian year.
    /// Non-positive values will be marked with a "BCE" postfix.
    pub fn display_year(&self) -> String {
        self.display_year_opt(true, false, false, false)
    }

    /// Prints the year as a human-readable gregorian year with controllable
    /// pre- and postfixes denominating the year's positivity.
    ///
    /// ## Arguments
    /// - `secular`   Switches between "BC" and "BCE"
    /// - `periods`   Determines wheter to use punctuation in the abbreviations
    /// - `designate_positive`    Show a denomination for positive years
    /// - `ad_prefix` Use the "AD" designation for positive years in a prefix
    ///               position. Will be ignored if `designate_positive` is negative.
    pub fn display_year_opt(
        &self,
        secular: bool,
        periods: bool,
        designate_positive: bool,
        ad_prefix: bool,
    ) -> String {
        let np_postfix = match (secular, periods) {
            (true, false) => "BCE",
            (true, true) => "B.C.E.",
            (false, false) => "BC",
            (false, true) => "B.C.",
        };

        let positive_dn = match (periods, ad_prefix) {
            (true, false) => "C.E.",
            (false, false) => "CE",
            (true, true) => "AD",
            (false, true) => "A.D.",
        };

        if self.year > 0 {
            if designate_positive && ad_prefix {
                format!("{} {}", positive_dn, self.year)
            } else if designate_positive && !ad_prefix {
                format!("{} {}", self.year, positive_dn)
            } else {
                self.year.to_string()
            }
        } else {
            format!("{} {}", -(self.year as i64 - 1), np_postfix)
        }
    }
}

/// A string with a value and possibly user-defined overrides for various
/// formattings.
#[derive(Clone, Debug, PartialEq)]
pub struct FormattableString {
    /// Canonical string value.
    pub(crate) value: String,
    /// User-defined title case override.
    pub(crate) title_case: Option<String>,
    /// User-defined sentence case override.
    pub(crate) sentence_case: Option<String>,
    /// If true, the user opts out of all automatic formatting for this string.
    pub(crate) verbatim: bool,
}

/// A collection of formattable Strings consisting of a title, a translated title, and a shorthand.
#[derive(Clone, Debug, PartialEq)]
pub struct Title {
    /// Canonical title.
    pub value: FormattableString,
    /// Optional title shorthand.
    pub shorthand: Option<FormattableString>,
    /// Optional title translation.
    pub translated: Option<FormattableString>,
}

impl Title {
    /// Create a new Title.
    pub fn new(value: FormattableString, shorthand: Option<FormattableString>, translated: Option<FormattableString>) -> Self {
        Self {
            value,
            shorthand,
            translated,
        }
    }

    /// Create a new title using a [`FormattableString`].
    pub fn from_fs(value: FormattableString) -> Self {
        Self {
            value,
            shorthand: None,
            translated: None,
        }
    }

    /// Create a new title from a string reference.
    pub fn from_str(s: &str) -> Self {
        Self::from_fs(FormattableString::new_shorthand(s.into()))
    }
}

/// Just like a [Title], but with formatting applied.
#[derive(Clone, Debug, PartialEq)]
pub struct FormattedTitle {
    /// Canonical title.
    pub value: FormattedString,
    /// Optional title shorthand.
    pub shorthand: Option<FormattedString>,
    /// Optional title translation.
    pub translated: Option<FormattedString>,
}

/// A string with a canonical value and title and sentence formatted variants.
#[derive(Clone, Debug, PartialEq)]
pub struct FormattedString {
    /// Canonical string value.
    pub value: String,
    /// String formatted as title case.
    pub title_case: String,
    /// String formatted as sentence case.
    pub sentence_case: String,
}

impl FormattableString {
    /// Creates a new formattable string.
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

    /// Creates a new formattable string from just a string.
    pub fn new_shorthand(value: String) -> Self {
        Self {
            value,
            title_case: None,
            sentence_case: None,
            verbatim: false,
        }
    }

    /// Creates a new formattable string that opts out of automatic
    /// formatting.
    pub fn new_verbatim(value: String, verbatim: bool) -> Self {
        Self {
            value,
            verbatim,
            title_case: None,
            sentence_case: None,
        }
    }
}

/// An URL, possibly with a last visited date.
#[derive(Clone, Debug, PartialEq)]
pub struct QualifiedUrl {
    /// The [Url].
    pub value: Url,
    /// The possible last visited Date.
    pub visit_date: Option<Date>,
}

/// Parses an integer range from a string reference.
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

/// A duration.
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct Duration {
    days: u32,
    hours: u32,
    minutes: u32,
    seconds: u8,
    milliseconds: f64,
}

/// Errors that can occur when parsing a string to a duration
#[derive(Clone, Error, Debug)]
pub enum DurationError {
    /// The string is malformed.
    #[error("string does not match duration regex")]
    NoMatch,
    /// The value is out of bounds when another, subsequent value is present (i.e. `01:61:48`).
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

    /// Tries to create a duration from a string.
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

    /// Tries to get a duration range from a string.
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

/// Could not cast to the desired field type.
#[derive(Error, Debug)]
#[error("Got wrong type `{0}`.")]
pub struct EntryTypeCastError(FieldType);

macro_rules! try_from_fieldtype {
    ($variant:ident, $target:ty $(,)*) => {
        try_from_fieldtype!(noref $variant, $target);

        impl<'s> TryFrom<&'s FieldType> for &'s $target {
            type Error = EntryTypeCastError;

            fn try_from(value: &'s FieldType) -> Result<Self, Self::Error> {
                match value {
                    FieldType::$variant(f) => Ok(f),
                    _ => Err(EntryTypeCastError(value.clone())),
                }
            }
        }
    };

    (noref $variant:ident, $target:ty $(,)*) => {
        impl TryFrom<FieldType> for $target {
            type Error = EntryTypeCastError;

            fn try_from(value: FieldType) -> Result<Self, Self::Error> {
                match value {
                    FieldType::$variant(f) => Ok(f),
                    _ => Err(EntryTypeCastError(value)),
                }
            }
        }

        impl From<$target> for FieldType {
            fn from(value: $target) -> Self {
                FieldType::$variant(value)
            }
        }
    };

    ($variant:ident, $target:ty, $ref_target:ty $(,)*) => {
        try_from_fieldtype!(noref $variant, $target);

        impl<'s> TryFrom<&'s FieldType> for &'s $ref_target {
            type Error = EntryTypeCastError;

            fn try_from(value: &'s FieldType) -> Result<Self, Self::Error> {
                match value {
                    FieldType::$variant(f) => Ok(f),
                    _ => Err(EntryTypeCastError(value.clone())),
                }
            }
        }
    }
}

try_from_fieldtype!(Title, Title);
try_from_fieldtype!(FormattableString, FormattableString);
try_from_fieldtype!(FormattedString, FormattedString);
try_from_fieldtype!(Text, String, str);
try_from_fieldtype!(Integer, i64);
try_from_fieldtype!(Date, Date);
try_from_fieldtype!(Persons, Vec<Person>, [Person]);
try_from_fieldtype!(
    PersonsWithRoles,
    Vec<(Vec<Person>, PersonRole)>,
    [(Vec<Person>, PersonRole)]
);
try_from_fieldtype!(IntegerOrText, NumOrStr);
try_from_fieldtype!(Range, std::ops::Range<i64>);
try_from_fieldtype!(Duration, Duration);
try_from_fieldtype!(TimeRange, std::ops::Range<Duration>);
try_from_fieldtype!(Url, QualifiedUrl);
try_from_fieldtype!(Language, unic_langid::LanguageIdentifier);
try_from_fieldtype!(Entries, Vec<Entry>, [Entry]);

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
