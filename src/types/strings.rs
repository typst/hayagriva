use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;

use citationberg::LongShortForm;
use serde::{Deserialize, Serialize, de::Visitor, ser::SerializeMap};
use thiserror::Error;
use unscanny::Scanner;

use crate::lang::{Case, CaseFolder, SentenceCase, TitleCase};

/// A string for presentation.
///
/// It can contain an optional short version and control case folding.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct FormatString {
    /// The canonical version of the string.
    pub value: ChunkedString,
    /// The short version of the string.
    pub short: Option<Box<ChunkedString>>,
}

impl<'de> Deserialize<'de> for FormatString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self};
        struct OurVisitor;

        impl<'de> Visitor<'de> for OurVisitor {
            type Value = FormatString;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str(
                    "a formattable string or a dictionary with an optional short version",
                )
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
                #[serde(untagged)]
                enum Inner {
                    Full { value: ChunkedString, short: Option<ChunkedString> },
                    Val(ChunkedString),
                }

                Deserialize::deserialize(de::value::MapAccessDeserializer::new(map)).map(
                    |inner: Inner| match inner {
                        Inner::Val(value) => Self::Value { value, short: None },
                        Inner::Full { value, short } => {
                            Self::Value { value, short: short.map(Box::new) }
                        }
                    },
                )
            }
        }

        deserializer.deserialize_any(OurVisitor)
    }
}

impl Serialize for FormatString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(short) = &self.short {
            let mut map = serializer.serialize_map(Some(2))?;
            map.serialize_entry("value", &self.value)?;
            map.serialize_entry("short", short)?;
            map.end()
        } else {
            self.value.serialize(serializer)
        }
    }
}

impl FormatString {
    /// Creates a new empty `FormatStr`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new `FormatStr` for the value string.
    pub fn with_value(value: impl Into<String>) -> Self {
        Self {
            value: StringChunk::normal(value).into(),
            short: None,
        }
    }

    /// Creates a new `FormatStr` from a long and a short string.
    pub fn with_short(value: impl Into<String>, short: impl Into<String>) -> Self {
        Self {
            value: StringChunk::normal(value).into(),
            short: Some(Box::new(StringChunk::normal(short).into())),
        }
    }

    /// Format the long version of the string as-is.
    fn fmt_long(&self, buf: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(buf)
    }

    /// Format the short version of the string as-is.
    pub fn fmt_short(&self, buf: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.short {
            Some(short) => short.fmt(buf),
            None => self.value.fmt(buf),
        }
    }

    /// Format this formattable string in title case.
    pub fn format_title_case(&self, props: TitleCase) -> String {
        self.value.format_title_case(props)
    }

    /// Format this formattable string in sentence case.
    pub fn format_sentence_case(&self, props: SentenceCase) -> String {
        self.value.format_sentence_case(props)
    }

    /// Returns the right variant for the given form.
    pub fn select(&self, form: LongShortForm) -> &ChunkedString {
        match form {
            LongShortForm::Long => &self.value,
            LongShortForm::Short => self.short.as_deref().unwrap_or(&self.value),
        }
    }
}

impl fmt::Display for FormatString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_long(f)
    }
}

impl From<String> for FormatString {
    fn from(s: String) -> Self {
        Self::with_value(s)
    }
}

impl From<StringChunk> for FormatString {
    fn from(chunk: StringChunk) -> Self {
        Self { value: chunk.into(), short: None }
    }
}

impl FromStr for FormatString {
    type Err = ChunkedStrParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self { value: ChunkedString::from_str(s)?, short: None })
    }
}

/// A string whose elements can set whether they do case folding.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ChunkedString(pub Vec<StringChunk>);

/// A string whose elements can set whether they do case folding.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct FoldableChunkedString(pub Vec<FoldableStringChunk>);

impl TryFrom<ChunkedString> for FoldableChunkedString {
    type Error = ();

    fn try_from(value: ChunkedString) -> Result<Self, Self::Error> {
        Ok(Self(value.0.into_iter().map(TryInto::try_into).collect::<Result<_, _>>()?))
    }
}

impl From<FoldableChunkedString> for ChunkedString {
    fn from(value: FoldableChunkedString) -> Self {
        Self(value.0.into_iter().map(Into::into).collect())
    }
}

impl<'de> Deserialize<'de> for ChunkedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ChunkedVisitor;

        impl<'de> Visitor<'de> for ChunkedVisitor {
            type Value = ChunkedString;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a formattable string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Self::Value::from_str(v).map_err(|e| E::custom(e.to_string()))
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                #[derive(Deserialize)]
                struct Inner {
                    value: String,
                    #[serde(default)]
                    verbatim: bool,
                }

                Deserialize::deserialize(serde::de::value::MapAccessDeserializer::new(
                    map,
                ))
                .and_then(|inner: Inner| {
                    if inner.verbatim {
                        Ok(StringChunk::verbatim(inner.value).into())
                    } else {
                        Self::Value::from_str(&inner.value)
                            .map_err(|e| serde::de::Error::custom(e.to_string()))
                    }
                })
            }
        }

        deserializer.deserialize_any(ChunkedVisitor)
    }
}

impl Serialize for ChunkedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut buf = String::with_capacity(self.len());
        self.fmt_serialized(&mut buf)
            .map_err(|_| serde::ser::Error::custom("could not write to string"))?;
        serializer.serialize_str(&buf)
    }
}

impl ChunkedString {
    /// Creates a new empty `ChunkedString`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Appends a string to the last chunk if it has the same kind or starts a
    /// new chunk if the types differ.
    pub fn push_str(&mut self, s: &str, kind: ChunkKind) {
        match self.0.last_mut() {
            Some(StringChunk { value, kind: target_kind }) if target_kind == &kind => {
                value.push_str(s);
            }
            _ => {
                self.0.push(StringChunk::new(s, kind));
            }
        }
    }

    /// Appends a character to the last chunk if it has the same kind or starts
    /// a new chunk if the types differ.
    pub fn push_char(&mut self, c: char, kind: ChunkKind) {
        match self.0.last_mut() {
            Some(StringChunk { value, kind: target_kind }) if target_kind == &kind => {
                value.push(c);
            }
            _ => {
                self.0.push(StringChunk::new(c.to_string(), kind));
            }
        }
    }

    /// Appends a chunk to the end of the string.
    pub fn push_chunk(&mut self, chunk: StringChunk) {
        self.0.push(chunk);
    }

    /// Returns the string as a `Cow`. It will be borrowed if there is only one
    /// chunk.
    pub fn to_str(&self) -> Cow<'_, str> {
        if self.0.is_empty() {
            Cow::Borrowed("")
        } else if self.0.len() == 1 {
            Cow::Borrowed(&self.0[0].value)
        } else {
            Cow::Owned(self.to_string())
        }
    }

    /// Write the chunked string as a parenthesized string.
    pub fn fmt_serialized(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        for chunk in &self.0 {
            chunk.fmt_serialized(buf)?;
        }

        Ok(())
    }

    /// Return the length of the string.
    pub fn len(&self) -> usize {
        self.0.iter().map(|c| c.len()).sum()
    }

    /// Returns whether the string is empty.
    pub fn is_empty(&self) -> bool {
        self.0.iter().all(|c| c.is_empty())
    }

    /// Extend with another `ChunkedString`.
    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0);
    }

    /// Format this formattable string in title case.
    pub fn format_title_case(&self, props: TitleCase) -> String {
        let mut c = CaseFolder::with_config(props.into());
        self.fold_case(&mut c);
        c.finish()
    }

    /// Format this formattable string in sentence case.
    pub fn format_sentence_case(&self, props: SentenceCase) -> String {
        let mut c = CaseFolder::with_config(props.into());
        self.fold_case(&mut c);
        c.finish()
    }

    /// Lowercase the string.
    pub fn to_lowercase(&self) -> String {
        let mut c = CaseFolder::with_config(Case::Lowercase);
        self.fold_case(&mut c);
        c.finish()
    }

    /// Uppercase the string.
    pub fn to_uppercase(&self) -> String {
        let mut c = CaseFolder::with_config(Case::Uppercase);
        self.fold_case(&mut c);
        c.finish()
    }

    /// Apply case-folding.
    pub fn fold_case(&self, c: &mut CaseFolder) {
        let config = c.case();
        for chunk in &self.0 {
            match chunk.kind {
                ChunkKind::Normal => c.reconfigure(config),
                ChunkKind::Verbatim | ChunkKind::Math => c.reconfigure(Case::NoTransform),
            };

            c.push_str(&chunk.value);
        }
    }
}

/// Check whether this is a control character for [`ChunkedString`].
fn is_chunk_control(c: char) -> bool {
    c == '\\' || c == '{' || c == '}' || c == '$'
}

/// Error that occurs when parsing a [`ChunkedString`].
///
/// Occurs when there are unbalanced braces. The field contains the position of
/// the unmatched closing brace.
#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
#[error("unmatched closing brace at position {0}")]
pub struct ChunkedStrParseError(usize);

impl FromStr for ChunkedString {
    type Err = ChunkedStrParseError;

    fn from_str(s: &str) -> Result<Self, ChunkedStrParseError> {
        let mut s = Scanner::new(s);
        let mut chunks = Self::new();
        let mut kind = ChunkKind::Normal;
        let mut depth = 0;

        while let Some(c) = s.eat() {
            if c == '\\' && s.peek().is_some_and(is_chunk_control) {
                chunks.push_char(s.eat().unwrap(), kind);
                continue;
            }

            match c {
                '{' if kind != ChunkKind::Math => {
                    depth += 1;
                    kind = ChunkKind::Verbatim;
                }
                '}' if kind != ChunkKind::Math => {
                    if depth == 0 {
                        return Err(ChunkedStrParseError(s.cursor().saturating_sub(1)));
                    }

                    depth -= 1;
                    if depth == 0 {
                        kind = ChunkKind::Normal;
                    }
                }
                '$' if kind == ChunkKind::Math => {
                    kind =
                        if depth > 0 { ChunkKind::Verbatim } else { ChunkKind::Normal };
                }
                '$' => {
                    kind = ChunkKind::Math;
                }
                _ => chunks.push_char(c, kind),
            }
        }

        Ok(chunks)
    }
}

impl From<String> for ChunkedString {
    fn from(s: String) -> Self {
        StringChunk::normal(s).into()
    }
}

impl fmt::Write for ChunkedString {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s, ChunkKind::default());
        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push_char(c, ChunkKind::default());
        Ok(())
    }
}

impl fmt::Display for ChunkedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for chunk in &self.0 {
            chunk.fmt(f)?;
        }

        Ok(())
    }
}

impl From<StringChunk> for ChunkedString {
    fn from(chunk: StringChunk) -> Self {
        Self(vec![chunk])
    }
}

/// A chunk of a string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringChunk {
    /// The string value.
    pub value: String,
    /// Whether the chunk is subject to case folding or contains math.
    pub kind: ChunkKind,
}

/// A chunk of a string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FoldableStringChunk {
    /// The string value.
    pub value: String,
    /// Whether the chunk is subject to case folding or contains math.
    pub kind: FoldableKind,
}

impl TryFrom<StringChunk> for FoldableStringChunk {
    type Error = ();

    fn try_from(value: StringChunk) -> Result<Self, Self::Error> {
        Ok(Self { value: value.value, kind: value.kind.try_into()? })
    }
}

impl From<FoldableStringChunk> for StringChunk {
    fn from(value: FoldableStringChunk) -> Self {
        Self { value: value.value, kind: value.kind.into() }
    }
}

impl StringChunk {
    /// Creates a new `StrChunk` from a string and a kind.
    pub fn new(value: impl Into<String>, kind: ChunkKind) -> Self {
        Self { value: value.into(), kind }
    }

    /// Creates a new `StrChunk` with the `ChunkKind::Normal` kind.
    pub fn normal(value: impl Into<String>) -> Self {
        Self::new(value, ChunkKind::Normal)
    }

    /// Creates a new `StrChunk` with the `ChunkKind::Verbatim` kind.
    pub fn verbatim(value: impl Into<String>) -> Self {
        Self::new(value, ChunkKind::Verbatim)
    }

    /// Creates a new `StrChunk` with the `ChunkKind::Math` kind.
    pub fn math(value: impl Into<String>) -> Self {
        Self::new(value, ChunkKind::Math)
    }

    /// Returns the string value.
    pub fn as_str(&self) -> &str {
        &self.value
    }

    /// Return the length of the string value.
    pub fn len(&self) -> usize {
        self.value.len()
    }

    /// Returns whether the chunk is empty.
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}

impl fmt::Write for StringChunk {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.value.push_str(s);
        Ok(())
    }
}

impl fmt::Display for StringChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind == ChunkKind::Math {
            write!(f, "${}$", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl StringChunk {
    /// Writes the chunk as a parenthesized string.
    fn fmt_serialized(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        fn write_escaped(val: &StringChunk, buf: &mut impl fmt::Write) -> fmt::Result {
            for char in val.value.chars() {
                if is_chunk_control(char) {
                    buf.write_char('\\')?;
                }

                buf.write_char(char)?;
            }

            Ok(())
        }

        match self.kind {
            ChunkKind::Normal => {
                write_escaped(self, buf)?;
            }
            ChunkKind::Verbatim => {
                buf.write_char('{')?;
                write_escaped(self, buf)?;
                buf.write_char('}')?;
            }
            ChunkKind::Math => {
                buf.write_char('$')?;
                write_escaped(self, buf)?;
                buf.write_char('$')?;
            }
        }

        Ok(())
    }
}

/// The kind of a string chunk.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum ChunkKind {
    /// Case-folding will be applied.
    #[default]
    Normal,
    /// Case-folding will not be applied.
    Verbatim,
    /// The contained markup is expected to be evaluated using
    /// [Typst](https://typst.app/).
    Math,
}

/// The kind of a string chunk for use with the case folder.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum FoldableKind {
    /// Case-folding will be applied.
    #[default]
    Normal,
    /// Case-folding will not be applied.
    Verbatim,
}

impl TryFrom<ChunkKind> for FoldableKind {
    type Error = ();

    fn try_from(value: ChunkKind) -> Result<Self, Self::Error> {
        match value {
            ChunkKind::Normal => Ok(Self::Normal),
            ChunkKind::Verbatim => Ok(Self::Verbatim),
            ChunkKind::Math => Err(()),
        }
    }
}

impl From<FoldableKind> for ChunkKind {
    fn from(value: FoldableKind) -> Self {
        match value {
            FoldableKind::Normal => Self::Normal,
            FoldableKind::Verbatim => Self::Verbatim,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escaped_brace() {
        let str = "Hello {World\\{}";
        let c = ChunkedString::from_str(str).unwrap();
        assert_eq!(c.0[0].kind, ChunkKind::Normal);
        assert_eq!(c.0[1].kind, ChunkKind::Verbatim);

        assert_eq!(c.0[0].value, "Hello ");
        assert_eq!(c.0[1].value, "World{");

        let mut buf = String::new();
        c.fmt_serialized(&mut buf).unwrap();
        assert_eq!(buf, str);
    }

    #[test]
    fn escaped_dollar() {
        let str = "Hello \\$World";
        let c = ChunkedString::from_str(str).unwrap();
        assert_eq!(c.0[0].kind, ChunkKind::Normal);
        assert_eq!(c.0[0].value, "Hello $World");

        let mut buf = String::new();
        c.fmt_serialized(&mut buf).unwrap();
        assert_eq!(buf, str);
    }

    #[test]
    fn escaped_backslash() {
        let str = "Hello \\\\World";
        let c = ChunkedString::from_str(str).unwrap();
        assert_eq!(c.0[0].kind, ChunkKind::Normal);
        assert_eq!(c.0[0].value, "Hello \\World");

        let mut buf = String::new();
        c.fmt_serialized(&mut buf).unwrap();
        assert_eq!(buf, str);
    }

    #[test]
    fn normal_backslash() {
        let str = "Hello \\World";
        let str2 = "Hello \\\\World";
        let c = ChunkedString::from_str(str).unwrap();
        let c2 = ChunkedString::from_str(str2).unwrap();
        assert_eq!(c.0[0].kind, ChunkKind::Normal);
        assert_eq!(c.0[0].value, "Hello \\World");
        assert_eq!(c, c2);

        let mut buf = String::new();
        c.fmt_serialized(&mut buf).unwrap();
        assert_eq!(buf, str2);
    }
}
