use std::{fmt::Display, num::NonZeroUsize, str::FromStr};

use crate::{Numeric, NumericError};

use super::{deserialize_from_str, serialize_display};
use serde::{de, Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PageRanges {
    pub ranges: Vec<PageRangesPart>,
}

impl PageRanges {
    pub fn new(ranges: Vec<PageRangesPart>) -> Self {
        Self { ranges }
    }

    pub fn first(&self) -> Option<&Numeric> {
        self.ranges.first().and_then(PageRangesPart::start)
    }
}

impl Display for PageRanges {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ranges.iter().map(|r| r.fmt(f)).collect()
    }
}

impl FromStr for PageRanges {
    type Err = PageRangesPartErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Split input into different ranges separated by `&` or `,`
        Ok(Self {
            ranges: group_by(s, |c, d| !(c == ',' || c == '&' || d == ',' || d == '&'))
                .map(PageRangesPart::from_str)
                .collect::<Result<_, _>>()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PageRangesPart {
    Ampersand,
    Comma,
    EscapedRange(Numeric, Numeric),
    SinglePage(Numeric),
    Range(Numeric, Numeric),
}

impl PageRangesPart {
    pub fn start(&self) -> Option<&Numeric> {
        match self {
            Self::EscapedRange(s, _) => Some(s),
            Self::SinglePage(s) => Some(s),
            Self::Range(s, _) => Some(s),
            _ => None,
        }
    }
}

impl Display for PageRangesPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            PageRangesPart::Ampersand => "&",
            PageRangesPart::Comma => ", ",
            PageRangesPart::EscapedRange(s, e) => return write!(f, "{s}-{e}"),
            PageRangesPart::SinglePage(s) => return write!(f, "{s}"),
            PageRangesPart::Range(s, e) => return write!(f, "{s}-{e}"),
        };
        Display::fmt(s, f)
    }
}

#[derive(Debug, Clone, Copy, Error)]
pub enum PageRangesPartErr {
    /// The string is malformed.
    #[error("page range string malformed")]
    Malformed,
    /// The string is empty.
    #[error("page range is empty")]
    Empty,
    #[error("todo")]
    NumericErr(NumericError),
}

impl From<NumericError> for PageRangesPartErr {
    fn from(value: NumericError) -> Self {
        Self::NumericErr(value)
    }
}

impl FromStr for PageRangesPart {
    type Err = PageRangesPartErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s.is_empty() {
            return Err(PageRangesPartErr::Empty);
        }
        let p = if s == "&" {
            Self::Ampersand
        } else if s == "," {
            Self::Comma
        } else if s.contains("\\-") {
            // If `-` chars are escaped, write `-`.
            let mut parts = s.split("\\-").map(str::trim);

            let start = parts.next().ok_or(PageRangesPartErr::Empty)?;
            let end = parts.next().ok_or(PageRangesPartErr::Empty)?;

            let r = Self::EscapedRange(parse_number(start)?, parse_number(end)?);
            if parts.next().is_some() {
                return Err(PageRangesPartErr::Malformed);
            }
            r
        } else {
            // Otherwise, split into the two halves of the dash.
            let mut parts = s.split(|c| c == '-' || c == '–').map(str::trim);
            let r = match (parts.next(), parts.next()) {
                (None, None) => todo!(),
                (Some(start), None) => Self::SinglePage(parse_number(start)?),
                (Some(start), Some(end)) => {
                    Self::Range(parse_number(start)?, parse_number(end)?)
                }
                _ => unreachable!(),
            };
            if parts.next().is_some() {
                return Err(PageRangesPartErr::Malformed);
            }
            r
        };
        Ok(p)
    }
}

deserialize_from_str!(PageRanges);
serialize_display!(PageRanges);

fn parse_number(s: &str) -> Result<Numeric, NumericError> {
    todo!()
}

/// Split `s` into maximal chunks such that two successive chars satisfy `pred`.
///
/// Returns an iterator over these chunks.
pub(crate) fn group_by<F>(s: &str, pred: F) -> GroupBy<'_, F>
where
    F: FnMut(char, char) -> bool,
{
    GroupBy::new(s, pred)
}

/// An iterator over string slice in (non-overlapping) chunks separated by a predicate.
///
/// Adapted from the nightly std.
pub(crate) struct GroupBy<'a, P> {
    string: &'a str,
    predicate: P,
}

impl<'a, P> GroupBy<'a, P> {
    pub(crate) fn new(string: &'a str, predicate: P) -> Self {
        GroupBy { string, predicate }
    }
}

impl<'a, P> Iterator for GroupBy<'a, P>
where
    P: FnMut(char, char) -> bool,
{
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.string.is_empty() {
            None
        } else {
            let mut len = 1;
            for w in windows(self.string, 2) {
                let chars: Vec<_> = w.chars().collect();
                let (c, d) = (chars[0], chars[1]);
                if (self.predicate)(c, d) {
                    len += 1
                } else {
                    break;
                }
            }
            let (head, tail) = self.string.split_at(len);
            self.string = tail;
            Some(head)
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.string.chars().size_hint()
    }
}

/// Return an iterator of sliding windows of size `size` over `string`.
///
/// # Panic
///
/// Panics if `size` is zero.
pub(crate) fn windows(string: &str, size: usize) -> Windows<'_> {
    assert!(size > 0);
    Windows::new(string, NonZeroUsize::new(size).unwrap())
}

/// An iterator of sliding windows of size `size` over `string`.
///
/// Each call of `next` advanced the window by one.
pub(crate) struct Windows<'a> {
    string: &'a str,
    size: NonZeroUsize,
}

impl<'a> Windows<'a> {
    pub(crate) fn new(string: &'a str, size: NonZeroUsize) -> Self {
        Self { string, size }
    }
}

impl<'a> Iterator for Windows<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.size.get() > self.string.len() {
            None
        } else {
            let ret = Some(&self.string[..self.size.get()]);
            self.string = &self.string[1..];
            ret
        }
    }
}
