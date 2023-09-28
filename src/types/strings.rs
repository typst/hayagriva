use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;

use thiserror::Error;
use unscanny::Scanner;
use yaml_rust::Yaml;

use crate::lang::{Case, CaseFolder, SentenceCaseConf, TitleCaseConf};

use super::{DeserializationError, HayagrivaValue, ParseContext, YamlDictExt, YamlExt};

/// A string for presentation.
///
/// It can contain an optional short version and control case folding.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FormatStr {
    /// The canonical version of the string.
    pub value: ChunkedStr,
    /// The short version of the string.
    pub short: Option<ChunkedStr>,
}

impl FormatStr {
    /// Creates a new empty `FormatStr`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new `FormatStr` for the value string.
    pub fn with_value(value: impl Into<String>) -> Self {
        Self {
            value: ChunkedStr::from_normal_str(value, StrChunkKind::Normal),
            short: None,
        }
    }

    /// Creates a new `FormatStr` from a long and a short string.
    pub fn with_short(value: impl Into<String>, short: impl Into<String>) -> Self {
        Self {
            value: ChunkedStr::from_normal_str(value, StrChunkKind::Normal),
            short: Some(ChunkedStr::from_normal_str(short, StrChunkKind::Normal)),
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
    pub fn format_title_case(&self, props: TitleCaseConf) -> String {
        self.value.format_title_case(props)
    }

    /// Format this formattable string in sentence case.
    pub fn format_sentence_case(&self, props: SentenceCaseConf) -> String {
        self.value.format_sentence_case(props)
    }
}

impl fmt::Display for FormatStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_long(f)
    }
}

impl HayagrivaValue for FormatStr {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => Ok(Self { value: ChunkedStr::from_str(s)?, short: None }),
            Yaml::Hash(h) => {
                let short = Some(match h.get_with_str("short", ctx) {
                    Ok(y) => {
                        let res = ChunkedStr::from_yaml(y, ctx)?;
                        ctx.pop_dict_key();
                        res
                    }
                    Err(_) => {
                        return ChunkedStr::from_yaml(yaml, ctx)
                            .map(|v| Self { value: v, short: None })
                    }
                });

                let value = h.get_with_str("value", ctx)?;
                let value = ChunkedStr::from_yaml(value, ctx)?;
                ctx.pop_dict_key();

                Ok(Self { value, short })
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        if let Some(short) = &self.short {
            let mut h = yaml_rust::yaml::Hash::new();
            h.insert_with_str("value", self.value.to_yaml());
            h.insert_with_str("short", short.to_yaml());
            Yaml::Hash(h)
        } else {
            self.value.to_yaml()
        }
    }

    fn explain() -> &'static str {
        "a formattable string or a dictionary with an optional short version"
    }
}

impl From<String> for FormatStr {
    fn from(s: String) -> Self {
        Self::with_value(s)
    }
}

/// A string whose elements can set whether they do case folding.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ChunkedStr(pub Vec<StrChunk>);

impl ChunkedStr {
    /// Creates a new empty `ChunkedStr`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new `ChunkedStr` from a string and a kind.
    pub fn from_normal_str(s: impl Into<String>, kind: StrChunkKind) -> Self {
        Self(vec![StrChunk::new(s, kind)])
    }

    /// Appends a string to the last chunk if it has the same kind.
    pub fn push_str(&mut self, s: &str, kind: StrChunkKind) {
        match self.0.last_mut() {
            Some(StrChunk { value, kind: target_kind }) if target_kind == &kind => {
                value.push_str(s);
            }
            _ => {
                self.0.push(StrChunk::new(s, kind));
            }
        }
    }

    /// Appends a character to the last chunk if it has the same kind.
    pub fn push_char(&mut self, c: char, kind: StrChunkKind) {
        match self.0.last_mut() {
            Some(StrChunk { value, kind: target_kind }) if target_kind == &kind => {
                value.push(c);
            }
            _ => {
                self.0.push(StrChunk::new(c.to_string(), kind));
            }
        }
    }

    /// Appends a chunk to the end of the string.
    pub fn push(&mut self, chunk: StrChunk) {
        self.0.push(chunk);
    }

    /// Returns the string as a `Cow`. It will be borrowed if there is only one
    /// chunk.
    pub fn as_cow(&self) -> Cow<'_, str> {
        if self.0.is_empty() {
            Cow::Borrowed("")
        } else if self.0.len() == 1 {
            Cow::Borrowed(&self.0[0].value)
        } else {
            Cow::Owned(self.to_string())
        }
    }

    /// Write the chunked string as a parenthesized string.
    pub fn fmt_parenthesized(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        for chunk in &self.0 {
            chunk.fmt_parenthesized(buf)?;
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

    /// Extend with another `ChunkedStr`.
    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0);
    }

    /// Format this formattable string in title case.
    pub fn format_title_case(&self, props: TitleCaseConf) -> String {
        let mut c = CaseFolder::from_config(props.into());
        self.fold_case(&mut c);
        c.finish()
    }

    /// Format this formattable string in sentence case.
    pub fn format_sentence_case(&self, props: SentenceCaseConf) -> String {
        let mut c = CaseFolder::from_config(props.into());
        self.fold_case(&mut c);
        c.finish()
    }

    /// Lowercase the string.
    pub fn to_lowercase(&self) -> String {
        let mut c = CaseFolder::from_config(Case::Lowercase);
        self.fold_case(&mut c);
        c.finish()
    }

    /// Uppercase the string.
    pub fn to_uppercase(&self) -> String {
        let mut c = CaseFolder::from_config(Case::Uppercase);
        self.fold_case(&mut c);
        c.finish()
    }

    /// Apply case-folding.
    pub fn fold_case(&self, c: &mut CaseFolder) {
        let config = c.case();
        for chunk in &self.0 {
            match chunk.kind {
                StrChunkKind::Normal => c.config(config),
                StrChunkKind::Verbatim | StrChunkKind::Math => {
                    c.config(Case::NoTransform)
                }
            };

            c.push_str(&chunk.value);
        }
    }
}

fn is_chunk_control(c: char) -> bool {
    c == '\\' || c == '{' || c == '}' || c == '$'
}

/// Error that occurs when parsing a [`ChunkedStr`].
///
/// Occurs when there are unbalanced braces. The field contains the position of
/// the unmatched closing brace.
#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
#[error("unmatched closing brace at position {0}")]
pub struct ChunkedStrParseError(usize);

impl FromStr for ChunkedStr {
    type Err = ChunkedStrParseError;

    fn from_str(s: &str) -> Result<Self, ChunkedStrParseError> {
        let mut s = Scanner::new(s);
        let mut chunks = Self::new();
        let mut kind = StrChunkKind::Normal;
        let mut depth = 0;

        while !s.done() {
            let c = s.eat().unwrap();
            if c == '\\' && s.peek().filter(|c| is_chunk_control(*c)).is_some() {
                chunks.push_char(s.eat().unwrap(), kind);
                break;
            }

            match c {
                '{' if kind != StrChunkKind::Math => {
                    depth += 1;
                    kind = StrChunkKind::Verbatim;
                }
                '}' if kind != StrChunkKind::Math => {
                    if depth == 0 {
                        return Err(ChunkedStrParseError(s.cursor().saturating_sub(1)));
                    }

                    depth -= 1;
                    if depth == 0 {
                        kind = StrChunkKind::Normal;
                    }
                }
                '$' if kind == StrChunkKind::Math => {
                    kind = if depth > 0 {
                        StrChunkKind::Verbatim
                    } else {
                        StrChunkKind::Normal
                    };
                }
                '$' => {
                    kind = StrChunkKind::Math;
                }
                _ => chunks.push_char(c, kind),
            }
        }

        Ok(chunks)
    }
}

impl From<String> for ChunkedStr {
    fn from(s: String) -> Self {
        Self::from_normal_str(s, StrChunkKind::Normal)
    }
}

impl fmt::Write for ChunkedStr {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        match self.0.last_mut() {
            Some(StrChunk { value, kind: StrChunkKind::Normal }) => {
                value.write_str(s)?;
                Ok(())
            }
            _ => {
                self.0
                    .push(StrChunk { value: s.to_owned(), kind: StrChunkKind::Normal });
                Ok(())
            }
        }
    }
}

impl fmt::Display for ChunkedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for chunk in &self.0 {
            chunk.fmt(f)?;
        }

        Ok(())
    }
}

impl From<StrChunk> for ChunkedStr {
    fn from(chunk: StrChunk) -> Self {
        Self(vec![chunk])
    }
}

impl HayagrivaValue for ChunkedStr {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => Ok(Self::from_str(s)?),
            Yaml::Hash(h) => {
                let value = h.get_with_str("value", ctx)?.as_deserialized_str()?;
                ctx.pop_dict_key();

                let res = h.get_with_str("verbatim", ctx);

                let verbatim = match res {
                    Ok(Yaml::Boolean(b)) => *b,
                    Ok(_) => return Err(DeserializationError::ExpectedKey("verbatim")),
                    Err(_) => false,
                };

                if res.is_ok() {
                    ctx.pop_dict_key();
                }

                Ok(StrChunk::new(
                    value,
                    if verbatim { StrChunkKind::Verbatim } else { StrChunkKind::Normal },
                )
                .into())
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        let mut buf = String::with_capacity(self.len());
        self.fmt_parenthesized(&mut buf).unwrap();
        Yaml::String(buf)
    }

    fn explain() -> &'static str {
        "a formattable string"
    }
}

/// A chunk of a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StrChunk {
    /// The string value.
    pub value: String,
    /// Whether the chunk is subject to case folding or contains math.
    pub kind: StrChunkKind,
}

impl StrChunk {
    /// Creates a new `StrChunk` from a string and a kind.
    pub fn new(value: impl Into<String>, kind: StrChunkKind) -> Self {
        Self { value: value.into(), kind }
    }

    /// Creates a new `StrChunk` with the `StrChunkKind::Verbatim` kind.
    pub fn new_verbatim(value: impl Into<String>) -> Self {
        Self::new(value, StrChunkKind::Verbatim)
    }

    /// Creates a new `StrChunk` with the `StrChunkKind::Math` kind.
    pub fn new_math(value: impl Into<String>) -> Self {
        Self::new(value, StrChunkKind::Math)
    }

    /// Creates a new `StrChunk` with the `StrChunkKind::Normal` kind.
    pub fn new_normal(value: impl Into<String>) -> Self {
        Self::new(value, StrChunkKind::Normal)
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

impl fmt::Write for StrChunk {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.value.push_str(s);
        Ok(())
    }
}

impl fmt::Display for StrChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind == StrChunkKind::Math {
            write!(f, "${}$", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl StrChunk {
    /// Writes the chunk as a parenthesized string.
    fn fmt_parenthesized(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        match self.kind {
            StrChunkKind::Normal => {
                write!(buf, "{}", self.value)?;
            }
            StrChunkKind::Verbatim => {
                write!(buf, "{{{}}}", self.value)?;
            }
            StrChunkKind::Math => {
                write!(buf, "${}$", self.value)?;
            }
        }

        Ok(())
    }
}

/// The kind of a string chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StrChunkKind {
    /// Case-folding will be applied.
    Normal,
    /// Case-folding will not be applied.
    Verbatim,
    /// The containing markup is expected to be evaluated using
    /// [Typst](https://typst.app/).
    Math,
}
