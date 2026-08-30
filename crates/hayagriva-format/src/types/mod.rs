//! Types for the Hayagriva format.

use std::{
    collections::BTreeMap,
    fmt::{self, Display},
    str::FromStr,
};

use serde::{Deserialize, Serialize, ser::SerializeMap};
use url::Url;

use hayagriva_core::{
    ChunkedStrParseError, Date, FormatString, types::derive_or_from_str,
};

pub use person::*;
pub use time::*;

mod person;
mod time;

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

derive_or_from_str! {
    /// A publisher, possibly with a location.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Publisher where "FormatString string or dictionary with \"name\" and \"location\"" {
        /// Publisher of the item.
        name: Option<FormatString>,
        /// Physical location at which the item was published or created.
        location: Option<FormatString>,
    }
}

impl Serialize for Publisher {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(location) = &self.location {
            let mut map = serializer.serialize_map(Some(2))?;
            map.serialize_entry("name", &self.name)?;
            map.serialize_entry("location", location)?;
            map.end()
        } else {
            self.name.serialize(serializer)
        }
    }
}

impl Publisher {
    /// Create a new publisher.
    pub fn new(name: Option<FormatString>, location: Option<FormatString>) -> Self {
        Self { name, location }
    }

    /// Publisher of the item.
    pub fn name(&self) -> Option<&FormatString> {
        self.name.as_ref()
    }

    /// Physical location at which the item was published or created.
    pub fn location(&self) -> Option<&FormatString> {
        self.location.as_ref()
    }
}

impl FromStr for Publisher {
    type Err = ChunkedStrParseError;

    /// Creates a new publisher with `s` as its name and no location.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Publisher::new(Some(FormatString::from_str(s)?), None))
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
