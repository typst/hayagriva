//! Base types for the bibliography items and their content.

use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt::Write;
use std::fmt::{self, Display};
use std::str::FromStr;

use strum::{Display, EnumString};
use thiserror::Error;
use unic_langid::LanguageIdentifier;
use unscanny::Scanner;
use url::Url;
use yaml_rust::Yaml;

pub use persons::*;
pub use strings::*;
pub use time::*;

mod persons;
mod strings;
mod time;

/// Contains information about the context in which a YAML value is parsed.
pub struct ParseContext<'a> {
    /// Entry types of the parents. Can be empty.
    pub path: Vec<EntryType>,
    /// The key of the currently parsed entry.
    pub key: &'a str,
    /// The keys of the currently parsed dictionary.
    pub dict_keys: Vec<&'static str>,
}

impl<'a> ParseContext<'a> {
    /// Create a new parse context.
    pub fn new(key: &'a str) -> Self {
        Self { path: Vec::new(), dict_keys: Vec::new(), key }
    }

    pub(crate) fn default_type(&self) -> Option<EntryType> {
        self.path.last().map(EntryType::default_parent)
    }

    pub(crate) fn pop_dict_key(&mut self) {
        self.dict_keys.pop();
    }
}

/// All bibliographic values implement this trait.
pub trait HayagrivaValue {
    /// Convert a YAML value to the type.
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized;

    /// Convert the type to a YAML value.
    fn to_yaml(&self) -> yaml_rust::Yaml;

    /// A human-readable explanation of the type.
    fn explain() -> &'static str;

    /// Create an error for when the type was expected but not found.
    fn expected_error() -> DeserializationError {
        DeserializationError::Expected(Self::explain())
    }
}

pub(crate) trait YamlDictExt {
    fn get_with_str(
        &self,
        key: &'static str,
        ctx: &mut ParseContext<'_>,
    ) -> Result<&yaml_rust::Yaml, DeserializationError>;
    fn insert_with_str(&mut self, key: &str, value: yaml_rust::Yaml);
}

impl YamlDictExt for yaml_rust::yaml::Hash {
    fn get_with_str(
        &self,
        key: &'static str,
        ctx: &mut ParseContext<'_>,
    ) -> Result<&yaml_rust::Yaml, DeserializationError> {
        let res = self
            .get(&Yaml::String(key.to_string()))
            .ok_or(DeserializationError::ExpectedKey(key))?;

        ctx.dict_keys.push(key);
        Ok(res)
    }

    fn insert_with_str(&mut self, key: &str, value: yaml_rust::Yaml) {
        self.insert(Yaml::String(key.to_string()), value);
    }
}

pub(crate) trait YamlExt {
    fn as_deserialized_str(&self) -> Result<&str, DeserializationError>;
}

impl YamlExt for Yaml {
    fn as_deserialized_str(&self) -> Result<&str, DeserializationError> {
        match self {
            Yaml::String(s) => Ok(s),
            _ => Err(DeserializationError::Expected("string")),
        }
    }
}

/// Describes which kind of work a database entry refers to.
#[derive(Copy, Clone, Display, Debug, EnumString, PartialEq, Eq)]
#[non_exhaustive]
#[strum(serialize_all = "lowercase")]
pub enum EntryType {
    /// A short text, possibly of journalistic or scientific nature,
    /// appearing in some greater publication.
    Article,
    /// A section of a greater containing work.
    Chapter,
    /// A short segment of media on some subject matter.
    /// Could appear in a work of reference or a data set
    Entry,
    /// Text published within an Anthology.
    Anthos,
    /// A document compiled by authors that may be affiliated to an organization.
    /// Presents information for a specific audience or purpose.
    Report,
    /// Scholarly work delivered to fulfill degree requirements at a higher
    /// education institution.
    Thesis,
    /// Piece of content that can be found on the internet and is native to the
    /// medium, like an animation, a web app, or a form of content not found
    /// elsewhere. Do not use this entry type when referencing a textual blog
    /// article, instead use an `Article` with a `Blog` parent.
    Web,
    /// A part of a show or another type of performed media, typically all
    /// taking place in the same location.
    Scene,
    /// A form of artistic/creative expression.
    Artwork,
    /// A technical document deposited at a government agency that describes an
    /// invention to legally limit the rights of reproduction to the inventors.
    Patent,
    /// Reference to a legal case that was or is to be heard at a court of law.
    Case,
    /// The issue of a newspaper that was published on a given day.
    Newspaper,
    /// Legal document or draft there of that is, is to be, or was to be
    /// enacted into binding law.
    Legislation,
    /// Legal document or draft thereof that is, is to be, or was to be enacted
    /// into binding law.
    Manuscript,
    /// A post on a micro-blogging platform like Twitter.
    Tweet,
    /// Items that do not match any of the other Entry type composites.
    Misc,
    /// A publication that periodically publishes issues with unique content.
    /// This includes scientific journals and news magazines.
    Periodical,
    /// The official published record of the events at a professional
    /// conference.
    Proceedings,
    /// Long-form work published pysically as a set of bound sheets.
    Book,
    /// Set of self-published articles on a website.
    Blog,
    /// A work of reference. This could be a manual or a dictionary.
    Reference,
    /// Professional conference. This Entry type implies that the item
    /// referenced has been an event at the conference itself. If you instead
    /// want to reference a paper published in the published proceedings of the
    /// conference, use an `Article` with a `Proceedings` parent.
    Conference,
    /// Collection of different texts on a single topic/theme.
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

impl HayagrivaValue for EntryType {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        _: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        Self::from_str(&yaml.as_deserialized_str()?.to_ascii_lowercase())
            .map_err(|_| Self::expected_error())
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        Yaml::String(self.to_string())
    }

    fn explain() -> &'static str {
        "entry type"
    }
}

impl EntryType {
    pub(crate) fn default_parent(&self) -> Self {
        match self {
            Self::Article => Self::Periodical,
            Self::Chapter => Self::Book,
            Self::Entry => Self::Reference,
            Self::Anthos => Self::Anthology,
            Self::Web => Self::Web,
            Self::Scene => Self::Video,
            Self::Artwork => Self::Exhibition,
            Self::Legislation => Self::Anthology,
            Self::Tweet => Self::Tweet,
            Self::Video => Self::Video,
            Self::Audio => Self::Audio,
            _ => Self::Misc,
        }
    }
}

/// This error can occur when converting from YAML to a type.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum DeserializationError {
    #[error("malformed date")]
    Date(#[from] time::DateError),
    #[error("malformed duration")]
    Duration(#[from] time::DurationError),
    #[error("malformed person")]
    Person(#[from] persons::PersonError),
    #[error("malformed numeric value")]
    Numeric(#[from] NumericError),
    #[error("malformed URL")]
    Url(#[from] url::ParseError),
    #[error("malformed format string")]
    FormatString(#[from] strings::ChunkedStrParseError),
    #[error("expected {0}")]
    Expected(&'static str),
    #[error("invalid language identifier")]
    InvalidLanguageIdentifier,
    #[error("expected key {0}")]
    ExpectedKey(&'static str),
}

/// A numeric value that can be pluralized.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Numeric {
    /// The numeric value.
    pub value: NumericValue,
    /// A string that is prepended to the value.
    pub prefix: Option<String>,
    /// A string that is appended to the value.
    pub suffix: Option<String>,
}

impl Numeric {
    /// Creates a new `Numeric` from a number.
    pub fn new(value: i32) -> Self {
        Self {
            value: NumericValue::Number(value),
            prefix: None,
            suffix: None,
        }
    }

    /// Creates a new `Numeric` from a range.
    pub fn from_range(range: std::ops::Range<i32>) -> Self {
        Self {
            value: NumericValue::Set(vec![
                (range.start, Some(NumericDelimiter::Hyphen)),
                (range.end, None),
            ]),
            prefix: None,
            suffix: None,
        }
    }

    /// Whether the numeric value contains only numbers.
    pub fn will_transform(&self) -> bool {
        self.prefix.is_none() && self.suffix.is_none()
    }

    /// Format the value without the prefix and suffix.
    pub fn fmt_value<T>(&self, buf: &mut T, machine_readable: bool) -> std::fmt::Result
    where
        T: fmt::Write,
    {
        let format = |n: i32, buf: &mut T| -> std::fmt::Result { write!(buf, "{}", n) };

        match &self.value {
            &NumericValue::Number(n) => format(n, buf)?,
            NumericValue::Set(s) => {
                for &(n, sep) in s {
                    format(n, buf)?;
                    if let Some(sep) = sep {
                        if machine_readable {
                            buf.write_char(sep.as_char())?
                        } else {
                            write!(buf, "{}", sep)?
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn fmt_custom<T>(&self, buf: &mut T, machine_readable: bool) -> std::fmt::Result
    where
        T: fmt::Write,
    {
        if let Some(prefix) = &self.prefix {
            buf.write_str(prefix)?;
        }
        self.fmt_value(buf, machine_readable)?;
        if let Some(suffix) = &self.suffix {
            buf.write_str(suffix)?;
        }

        Ok(())
    }

    /// Whether the numeric value is plural.
    pub fn is_plural(&self, is_number_of: bool) -> bool {
        match &self.value {
            NumericValue::Number(n) if is_number_of => n != &1,
            NumericValue::Number(_) => false,
            NumericValue::Set(vec) => vec.len() != 1,
        }
    }

    /// Whether the value is a single number with no prefix or suffix.
    pub fn single_number(&self) -> Option<i32> {
        (self.prefix.is_none() && self.suffix.is_none())
            .then_some(match &self.value {
                NumericValue::Number(n) => Some(*n),
                _ => None,
            })
            .flatten()
    }

    /// Returns a range if the value is a range.
    pub fn range(&self) -> Option<std::ops::Range<i32>> {
        self.value.range()
    }
}

impl FromStr for Numeric {
    type Err = NumericError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut s = Scanner::new(value);
        let prefix =
            s.eat_while(|c: char| !c.is_numeric() && !c.is_whitespace() && c != '-');

        let value = number(&mut s).ok_or(NumericError::NoNumber)?;
        s.eat_whitespace();

        let value = match s.peek() {
            Some(c) if is_delimiter(c) => {
                s.eat();
                let mut items = vec![(value, Some(NumericDelimiter::try_from(c)?))];
                loop {
                    let num = number(&mut s).ok_or(NumericError::NoNumber)?;
                    s.eat_whitespace();
                    match NumericDelimiter::from_str(s.eat_while(is_delimiter)) {
                        Ok(d) => {
                            items.push((num, Some(d)));
                        }
                        Err(_) => {
                            items.push((num, None));
                            break;
                        }
                    }
                }
                NumericValue::Set(items)
            }

            _ => NumericValue::Number(value),
        };
        s.eat_whitespace();
        let post = s.eat_while(|c: char| !c.is_whitespace());

        if !s.after().is_empty() {
            return Err(NumericError::UnexpectedCharactersAfterPostfix);
        }

        Ok(Self {
            value,
            prefix: if prefix.is_empty() { None } else { Some(prefix.to_string()) },
            suffix: if post.is_empty() { None } else { Some(post.to_string()) },
        })
    }
}

impl From<i32> for Numeric {
    fn from(n: i32) -> Self {
        Self::new(n)
    }
}

impl From<u32> for Numeric {
    fn from(n: u32) -> Self {
        Self::new(n as i32)
    }
}

/// Error when parsing a numeric value.
#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
pub enum NumericError {
    /// No number was found.
    #[error("no number found")]
    NoNumber,
    /// Unexpected characters after the postfix. It must not contain any
    /// non-whitespace characters.
    #[error("unexpected characters after postfix")]
    UnexpectedCharactersAfterPostfix,
    /// The string is not a delimiter.
    #[error("not a delimiter")]
    NotADelimiter,
    /// The string does not contain a delimiter.
    #[error("missing delimiter")]
    MissingDelimiter,
}

impl HayagrivaValue for Numeric {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        _ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => {
                Ok(Self::from_str(s).map_err(Into::<DeserializationError>::into)?)
            }
            Yaml::Integer(n) => Ok(Self::new(*n as i32)),
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        match self.single_number() {
            Some(n) => yaml_rust::Yaml::Integer(n as i64),
            None => yaml_rust::Yaml::String({
                let mut buf = String::new();
                self.fmt_custom(&mut buf, false).unwrap();
                buf
            }),
        }
    }

    fn explain() -> &'static str {
        "numeric string or integer"
    }
}

fn number(s: &mut Scanner) -> Option<i32> {
    s.eat_whitespace();
    let negative = s.eat_if('-');
    let num = s.eat_while(|c: char| c.is_numeric());
    if num.is_empty() {
        return None;
    }

    num.parse::<i32>().ok().map(|n| if negative { -n } else { n })
}

impl Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_custom(f, false)
    }
}

/// The numeric value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumericValue {
    /// A single number.
    Number(i32),
    /// A set of numbers.
    Set(Vec<(i32, Option<NumericDelimiter>)>),
}

impl NumericValue {
    /// Returns a range if the value is a range.
    pub fn range(&self) -> Option<std::ops::Range<i32>> {
        match self {
            Self::Number(_) => None,
            Self::Set(vec) => {
                if vec.len() == 2 {
                    let start = vec[0].0;
                    let end = vec[1].0;
                    let first_delim = vec[0].1;

                    if start < end
                        && (first_delim == Some(NumericDelimiter::Hyphen)
                            || (first_delim == Some(NumericDelimiter::Ampersand)
                                && start + 1 == end))
                    {
                        Some(start..end)
                    } else {
                        None
                    }
                } else if vec.len() > 2 {
                    for i in 1..vec.len() {
                        if vec[i - 1].1 != Some(NumericDelimiter::Ampersand) {
                            return None;
                        }

                        if vec[i - 1].0 + 1 != vec[i].0 {
                            return None;
                        }
                    }

                    Some(vec[0].0..vec[vec.len() - 1].0)
                } else {
                    None
                }
            }
        }
    }
}

/// Delimits individual numbers in a numeric value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericDelimiter {
    /// A comma.
    Comma,
    /// An ampersand.
    Ampersand,
    /// A hyphen. Will be converted to an en dash for display.
    Hyphen,
}

impl NumericDelimiter {
    /// Get the character representation of the delimiter.
    pub fn as_char(&self) -> char {
        match self {
            NumericDelimiter::Comma => ',',
            NumericDelimiter::Ampersand => '&',
            NumericDelimiter::Hyphen => '-',
        }
    }
}

impl std::fmt::Display for NumericDelimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumericDelimiter::Comma => f.write_str(", "),
            NumericDelimiter::Ampersand => f.write_str(" & "),
            NumericDelimiter::Hyphen => f.write_char('–'),
        }
    }
}

fn is_delimiter(c: char) -> bool {
    c == ',' || c == '&' || c == '-' || c == '–'
}

impl FromStr for NumericDelimiter {
    type Err = NumericError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first_char = s.chars().next().ok_or(NumericError::MissingDelimiter)?;
        if s.len() > first_char.len_utf8() {
            return Err(NumericError::NotADelimiter);
        }

        Self::try_from(first_char)
    }
}

impl TryFrom<char> for NumericDelimiter {
    type Error = NumericError;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ',' => Ok(NumericDelimiter::Comma),
            '&' => Ok(NumericDelimiter::Ampersand),
            '-' | '–' => Ok(NumericDelimiter::Hyphen),
            _ => Err(NumericError::NotADelimiter),
        }
    }
}

/// A type that may be a string or a stricly typed value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MaybeTyped<T> {
    /// The typed variant.
    Typed(T),
    /// The fallback string variant.
    String(String),
}

impl<T: Display> Display for MaybeTyped<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MaybeTyped::Typed(t) => t.fmt(f),
            MaybeTyped::String(s) => s.fmt(f),
        }
    }
}

impl<T: ToString> MaybeTyped<T> {
    /// Convert the value to a string.
    pub fn to_str(&self) -> Cow<str> {
        match self {
            MaybeTyped::Typed(t) => Cow::Owned(t.to_string()),
            MaybeTyped::String(s) => Cow::Borrowed(s),
        }
    }
}

impl<T> MaybeTyped<T>
where
    T: FromStr,
{
    pub(crate) fn infallible_from_str(s: &str) -> Self {
        match s.parse::<T>() {
            Ok(t) => MaybeTyped::Typed(t),
            Err(_) => MaybeTyped::String(s.to_owned()),
        }
    }
}

impl<T> FromStr for MaybeTyped<T>
where
    T: FromStr,
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::infallible_from_str(s))
    }
}

impl<T> From<T> for MaybeTyped<T> {
    fn from(t: T) -> Self {
        MaybeTyped::Typed(t)
    }
}

impl<T> HayagrivaValue for MaybeTyped<T>
where
    T: HayagrivaValue,
{
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        T::from_yaml(yaml, ctx).map(MaybeTyped::Typed).or_else(|e| {
            let Yaml::String(s) = yaml else {
                return Err(e);
            };

            Ok(MaybeTyped::String(s.clone()))
        })
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        match self {
            MaybeTyped::Typed(t) => t.to_yaml(),
            MaybeTyped::String(s) => yaml_rust::Yaml::String(s.clone()),
        }
    }

    fn explain() -> &'static str {
        T::explain()
    }
}

/// An URL, possibly with a last visited date.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedUrl {
    /// The [Url].
    pub value: Url,
    /// The last visited date.
    pub visit_date: Option<Date>,
}

impl QualifiedUrl {
    /// Create a new qualified URL.
    pub fn new(value: Url, visit_date: Option<Date>) -> Self {
        Self { value, visit_date }
    }
}

impl FromStr for QualifiedUrl {
    type Err = url::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self { value: Url::parse(s)?, visit_date: None })
    }
}

impl Display for QualifiedUrl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl HayagrivaValue for QualifiedUrl {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => Ok(Self::from_str(s)?),
            Yaml::Hash(hash) => {
                let url = hash.get_with_str("value", ctx)?.as_deserialized_str()?;
                let url = Url::from_str(url)?;
                ctx.pop_dict_key();

                let date = Date::from_yaml(hash.get_with_str("date", ctx)?, ctx)?;
                ctx.pop_dict_key();

                Ok(Self::new(url, Some(date)))
            }
            _ => Err(DeserializationError::Expected("url")),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        match self.visit_date {
            Some(date) => yaml_rust::Yaml::Hash({
                let mut hash = yaml_rust::yaml::Hash::new();
                hash.insert_with_str("value", Yaml::String(self.value.to_string()));
                hash.insert_with_str("date", date.to_yaml());
                hash
            }),
            None => yaml_rust::Yaml::String(self.value.to_string()),
        }
    }

    fn explain() -> &'static str {
        "URL string or dictionary with keys \"url\" and \"date\""
    }
}

impl HayagrivaValue for LanguageIdentifier {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        _: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        LanguageIdentifier::from_str(yaml.as_deserialized_str()?)
            .map_err(|_| DeserializationError::InvalidLanguageIdentifier)
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        Yaml::String(self.to_string())
    }

    fn explain() -> &'static str {
        "language identifier string"
    }
}

impl HayagrivaValue for String {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        _: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => Ok(s.clone()),
            Yaml::Integer(n) => Ok(n.to_string()),
            Yaml::Real(r) => Ok(r.to_string()),
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        Yaml::String(self.clone())
    }

    fn explain() -> &'static str {
        "string"
    }
}

impl HayagrivaValue for bool {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        _: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::Boolean(b) => Ok(*b),
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        Yaml::Boolean(*self)
    }

    fn explain() -> &'static str {
        "boolean"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_num() {
        let val = Numeric::from_str("1").unwrap();
        assert!(val.value == NumericValue::Number(1));
        assert!(val.prefix.is_none());
        assert!(val.suffix.is_none());
        assert_eq!(&val.to_string(), "1");

        let val = Numeric::from_str("-5").unwrap();
        assert!(val.value == NumericValue::Number(-5));
        assert!(val.prefix.is_none());
        assert!(val.suffix.is_none());
        assert_eq!(&val.to_string(), "-5");

        let val = Numeric::from_str("1st").unwrap();
        assert!(val.value == NumericValue::Number(1));
        assert!(val.prefix.is_none());
        assert!(val.suffix.as_deref() == Some("st"));
        assert_eq!(&val.to_string(), "1st");

        let val = Numeric::from_str("1, 2").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![(1, Some(NumericDelimiter::Comma)), (2, None)])
        );
        assert_eq!(val.to_string(), "1, 2");

        let val = Numeric::from_str("A16y").unwrap();
        assert!(val.value == NumericValue::Number(16));
        assert!(val.prefix.as_deref() == Some("A"));
        assert!(val.suffix.as_deref() == Some("y"));
        assert_eq!(&val.to_string(), "A16y");

        let val = Numeric::from_str("1-4").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![
                    (1, Some(NumericDelimiter::Hyphen)),
                    (4, None)
                ])
        );

        let val_other = Numeric::from_str("1 - 4").unwrap();
        assert_eq!(val, val_other);
        assert_eq!(&val.to_string(), "1–4");

        let val = Numeric::from_str("2 , 3").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![(2, Some(NumericDelimiter::Comma)), (3, None)])
        );
        assert_eq!(&val.to_string(), "2, 3");

        let val = Numeric::from_str("2 & 3 & 4").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![
                    (2, Some(NumericDelimiter::Ampersand)),
                    (3, Some(NumericDelimiter::Ampersand)),
                    (4, None)
                ])
        );
        assert_eq!(&val.to_string(), "2 & 3 & 4");

        assert!(Numeric::from_str("second").is_err());
        assert!(Numeric::from_str("2nd edition").is_err());
    }
}
