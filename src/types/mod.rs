//! Base types for the bibliography items and their content.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::{self, Display};
use std::str::FromStr;

use serde::de::value::StrDeserializer;
use serde::ser::SerializeMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use url::Url;

pub use numeric::*;
pub use page::*;
pub use persons::*;
pub use strings::*;
pub use time::*;

mod numeric;
mod page;
mod persons;
mod strings;
mod time;

/// Use the [`Display`] implementation of a type for serialization.
macro_rules! serialize_display {
    ($t:ty) => {
        impl Serialize for $t {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_str(&self.to_string())
            }
        }
    };
}

/// Use the [`FromStr`] implementation of a type for deserialization.
macro_rules! deserialize_from_str {
    ($t:ty) => {
        impl<'de> Deserialize<'de> for $t {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let s = <&'de str>::deserialize(deserializer)?;
                FromStr::from_str(s).map_err(de::Error::custom)
            }
        }
    };
}

/// Use the [`FromStr`] implementation of a type for deserialization if it is a
/// string.
macro_rules! derive_or_from_str {
    (
        $(#[$global:meta])*
        $gv:vis struct $s:ident where $expect:literal {
            $(
                $(#[doc = $doc:literal])*
                $(#[serde $serde:tt])*
                $v:vis $i:ident : $t:ty
            ),*
            $(,)?
        }
    ) => {
        $(#[$global])*
        $gv struct $s {
            $(
                $(#[doc = $doc])*
                $v $i: $t,
            )*
        }


        impl<'de> Deserialize<'de> for $s {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
            D: serde::Deserializer<'de>,
            {
                use std::fmt;
                use serde::de::{self, Visitor};
                struct OurVisitor;

                impl<'de> Visitor<'de> for OurVisitor {
                    type Value = $s;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str($expect)
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        Self::Value::from_str(value).map_err(|e| E::custom(e.to_string()))
                    }

                    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
                    where
                        A: serde::de::MapAccess<'de>,
                    {
                        #[derive(Deserialize)]
                        #[serde(rename_all = "kebab-case")]
                        struct Inner {
                            $(
                                $(#[serde $serde])*
                                $i: $t,
                            )*
                        }

                        Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))
                            .map(|inner: Inner| $s { $($i: inner.$i),* })
                    }
                }

                deserializer.deserialize_any(OurVisitor)
            }
        }
    };
}

use derive_or_from_str;
use deserialize_from_str;
use serialize_display;

/// Describes which kind of work a database entry refers to.
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[non_exhaustive]
#[serde(rename_all = "kebab-case")]
pub enum EntryType {
    /// A short text, possibly of journalistic or scientific nature,
    /// appearing in some greater publication.
    #[serde(alias = "Article")]
    Article,
    /// A section of a greater containing work.
    #[serde(alias = "Chapter")]
    Chapter,
    /// A short segment of media on some subject matter.
    /// Could appear in a work of reference or a data set
    #[serde(alias = "Entry")]
    Entry,
    /// Text published within an Anthology.
    #[serde(alias = "Anthos")]
    Anthos,
    /// A document compiled by authors that may be affiliated to an organization.
    /// Presents information for a specific audience or purpose.
    #[serde(alias = "Report")]
    Report,
    /// Scholarly work delivered to fulfill degree requirements at a higher
    /// education institution.
    #[serde(alias = "Thesis")]
    Thesis,
    /// Piece of content that can be found on the internet and is native to the
    /// medium, like an animation, a web app, or a form of content not found
    /// elsewhere. Do not use this entry type when referencing a textual blog
    /// article, instead use an `Article` with a `Blog` parent.
    #[serde(alias = "Web")]
    Web,
    /// A part of a show or another type of performed media, typically all
    /// taking place in the same location.
    #[serde(alias = "Scene")]
    Scene,
    /// A form of artistic/creative expression.
    #[serde(alias = "Artwork")]
    Artwork,
    /// A technical document deposited at a government agency that describes an
    /// invention to legally limit the rights of reproduction to the inventors.
    #[serde(alias = "Patent")]
    Patent,
    /// Reference to a legal case that was or is to be heard at a court of law.
    #[serde(alias = "Case")]
    Case,
    /// The issue of a newspaper that was published on a given day.
    #[serde(alias = "Newspaper")]
    Newspaper,
    /// Legal document or draft there of that is, is to be, or was to be
    /// enacted into binding law.
    #[serde(alias = "Legislation")]
    Legislation,
    /// A document that is not yet published.
    #[serde(alias = "Manuscript")]
    Manuscript,
    /// A post on a social media platform.
    #[serde(alias = "Post")]
    Post,
    /// Items that do not match any of the other Entry type composites.
    #[serde(alias = "Misc")]
    Misc,
    /// A live performance.
    #[serde(alias = "Performance")]
    Performance,
    /// A publication that periodically publishes issues with unique content.
    /// This includes scientific journals and news magazines.
    #[serde(alias = "Periodical")]
    Periodical,
    /// The official published record of the events at a professional
    /// conference.
    #[serde(alias = "Proceedings")]
    Proceedings,
    /// Long-form work published pysically as a set of bound sheets.
    #[serde(alias = "Book")]
    Book,
    /// Set of self-published articles on a website.
    #[serde(alias = "Blog")]
    Blog,
    /// A work of reference. This could be a manual or a dictionary.
    #[serde(alias = "Reference")]
    Reference,
    /// Professional conference. This Entry type implies that the item
    /// referenced has been an event at the conference itself. If you instead
    /// want to reference a paper published in the published proceedings of the
    /// conference, use an `Article` with a `Proceedings` parent.
    #[serde(alias = "Conference")]
    Conference,
    /// Collection of different texts on a single topic/theme.
    #[serde(alias = "Anthology")]
    Anthology,
    /// Publicly visible storage of the source code for a particular software
    /// and its modifications over time.
    #[serde(alias = "Repository")]
    Repository,
    /// Written discussion on the internet triggered by an original post.
    /// Could be on a forum, social network, or Q&A site.
    #[serde(alias = "Thread")]
    Thread,
    /// Motion picture of any form, possibly with accompanying audio.
    #[serde(alias = "Video")]
    Video,
    /// Recorded audible sound of any kind.
    #[serde(alias = "Audio")]
    Audio,
    /// A curated set of artworks.
    #[serde(alias = "Exhibition")]
    Exhibition,
    /// A prior publication of the same item.
    #[serde(alias = "Original")]
    Original,
}

impl EntryType {
    /// Entry parents have implicit defaults. This function returns the default
    /// parent for this entry type.
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
            Self::Post => Self::Post,
            Self::Video => Self::Video,
            Self::Audio => Self::Audio,
            _ => Self::Misc,
        }
    }
}

impl FromStr for EntryType {
    type Err = serde::de::value::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = StrDeserializer::<serde::de::value::Error>::new(s);
        Self::deserialize(s)
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

/// A type that may be a string or a stricly typed value.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, Eq, Hash)]
#[serde(untagged)]
pub enum MaybeTyped<T> {
    /// The typed variant.
    Typed(T),
    /// The fallback string variant.
    String(String),
}

impl<T: ToOwned> MaybeTyped<T> {
    /// Wrap the typed value in a [`Cow`]'s borrowed variant.
    pub fn to_cow(&self) -> MaybeTyped<Cow<T>> {
        match self {
            MaybeTyped::Typed(t) => MaybeTyped::Typed(Cow::Borrowed(t)),
            MaybeTyped::String(s) => MaybeTyped::String(s.clone()),
        }
    }
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

derive_or_from_str! {
    /// An URL, possibly with a last visited date.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct QualifiedUrl where "URL string or dictionary with keys \"url\" and \"date\"" {
        /// The [Url].
        pub value: Url,
        /// The last visited date.
        #[serde(rename = "date")]
        pub visit_date: Option<Date>,
    }
}

impl Serialize for QualifiedUrl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(date) = &self.visit_date {
            let mut map = serializer.serialize_map(Some(2))?;
            map.serialize_entry("value", &self.value)?;
            map.serialize_entry("date", date)?;
            map.end()
        } else {
            self.value.serialize(serializer)
        }
    }
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

/// A set of serial numbers like DOIs, ISBNs, or ISSNs.
/// Keys should be lowercase.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Hash)]
#[serde(transparent)]
pub struct SerialNumber(pub BTreeMap<String, String>);

impl<'de> Deserialize<'de> for SerialNumber {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Choice {
            Map(BTreeMap<String, StringOrNumber>),
            Other(StringOrNumber),
        }

        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrNumber {
            String(String),
            Number(i64),
            UnsignedNumber(u64),
            Float(f64),
        }

        impl Display for StringOrNumber {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::String(s) => s.fmt(formatter),
                    Self::Number(n) => n.fmt(formatter),
                    Self::UnsignedNumber(n) => n.fmt(formatter),
                    Self::Float(f) => f.fmt(formatter),
                }
            }
        }

        Choice::deserialize(deserializer).map(|choice| match choice {
            Choice::Other(text) => SerialNumber(BTreeMap::from_iter(vec![(
                "serial".to_owned(),
                text.to_string(),
            )])),
            Choice::Map(map) => {
                SerialNumber(map.into_iter().map(|(k, v)| (k, v.to_string())).collect())
            }
        })
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
        assert!(val.suffix_str() == Some("st"));
        assert_eq!(&val.to_string(), "1st");

        let val = Numeric::from_str("1, 2").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![(1, Some(NumericDelimiter::Comma)), (2, None)])
        );
        assert_eq!(val.to_string(), "1, 2");

        let val = Numeric::from_str("A16y").unwrap();
        assert!(val.value == NumericValue::Number(16));
        assert!(val.prefix_str() == Some("A"));
        assert!(val.suffix_str() == Some("y"));
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
