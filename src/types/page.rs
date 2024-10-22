use std::{cmp::Ordering, fmt::Display, num::TryFromIntError, str::FromStr};

use crate::{MaybeTyped, Numeric, NumericError};

use super::{custom_deserialize, serialize_display};
use serde::{Deserialize, Serialize};
use thiserror::Error;

impl MaybeTyped<PageRanges> {
    /// Order the values according to CSL rules.
    pub(crate) fn csl_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (MaybeTyped::Typed(a), MaybeTyped::Typed(b)) => a.csl_cmp(b),
            _ => self.to_string().cmp(&other.to_string()),
        }
    }
}

/// Ranges of page numbers, e.g., `1-4, 5 & 6`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PageRanges {
    /// The given ranges.
    pub ranges: Vec<PageRangesPart>,
}

custom_deserialize!(
    PageRanges where "pages, page ranges, ampersands, and commas"
    fn visit_i32<E: serde::de::Error>(self, v: i32) -> Result<Self::Value, E> {
        Ok(PageRanges::from(v))
    }
    fn visit_u32<E: serde::de::Error>(self, v: u32) -> Result<Self::Value, E> {
        PageRanges::try_from(v).map_err(|_| E::custom("value too large"))
    }
    fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<Self::Value, E> {
        PageRanges::try_from(v).map_err(|_| E::custom("value out of bounds"))
    }
    fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<Self::Value, E> {
        PageRanges::try_from(v).map_err(|_| E::custom("value too large"))
    }
);

impl PageRanges {
    /// Create a new `PageRanges` struct.
    pub fn new(ranges: Vec<PageRangesPart>) -> Self {
        Self { ranges }
    }

    /// Get the first page of the first range.
    pub fn first(&self) -> Option<&Numeric> {
        self.ranges.iter().find_map(PageRangesPart::start)
    }

    /// Order the values according to CSL rules.
    pub(crate) fn csl_cmp(&self, other: &Self) -> std::cmp::Ordering {
        #[derive(PartialEq, Eq)]
        struct OrderablePageRangesPart<'a>(&'a PageRangesPart);

        impl Ord for OrderablePageRangesPart<'_> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.csl_cmp(other.0)
            }
        }

        impl PartialOrd for OrderablePageRangesPart<'_> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        self.ranges
            .iter()
            .map(OrderablePageRangesPart)
            .cmp(other.ranges.iter().map(OrderablePageRangesPart))
    }

    /// Whether to pluralize the `pages` term, when used with this page range.
    pub fn is_plural(&self) -> bool {
        let mut count = 0;
        for range in &self.ranges {
            match range {
                PageRangesPart::SinglePage(_) => count += 1,
                PageRangesPart::Range(s, e) | PageRangesPart::EscapedRange(s, e) => {
                    if s != e {
                        return true;
                    }
                    count += 1
                }
                _ => {}
            }
        }
        count > 1
    }
}

impl From<i32> for PageRanges {
    fn from(value: i32) -> Self {
        Self { ranges: vec![value.into()] }
    }
}

impl TryFrom<u32> for PageRanges {
    type Error = TryFromIntError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Self { ranges: vec![value.try_into()?] })
    }
}

impl TryFrom<i64> for PageRanges {
    type Error = TryFromIntError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(Self { ranges: vec![value.try_into()?] })
    }
}

impl TryFrom<u64> for PageRanges {
    type Error = TryFromIntError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Self { ranges: vec![value.try_into()?] })
    }
}

impl Display for PageRanges {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ranges.iter().try_for_each(|r| r.fmt(f))
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

/// Parts of the page ranges.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PageRangesPart {
    /// An and, i.e, `&`.
    Ampersand,
    /// A comma, i.e., `,`.
    Comma,
    /// An escaped range with start and end, e.g., `1\-4`.
    EscapedRange(Numeric, Numeric),
    /// A single page, e.g., `5`.
    SinglePage(Numeric),
    /// A full range, e.g., `1n8--1n14`.
    Range(Numeric, Numeric),
}

custom_deserialize!(
    PageRangesPart where "a page, a page range, or a separator"
    fn visit_i32<E: serde::de::Error>(self, v: i32) -> Result<Self::Value, E> {
        Ok(PageRangesPart::from(v))
    }
    fn visit_u32<E: serde::de::Error>(self, v: u32) -> Result<Self::Value, E> {
        PageRangesPart::try_from(v).map_err(|_| E::custom("value too large"))
    }
    fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<Self::Value, E> {
        PageRangesPart::try_from(v).map_err(|_| E::custom("value out of bounds"))
    }
    fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<Self::Value, E> {
        PageRangesPart::try_from(v).map_err(|_| E::custom("value too large"))
    }
);

impl PageRangesPart {
    /// The start of a range, if any.
    pub fn start(&self) -> Option<&Numeric> {
        match self {
            Self::EscapedRange(s, _) => Some(s),
            Self::SinglePage(s) => Some(s),
            Self::Range(s, _) => Some(s),
            _ => None,
        }
    }

    /// The end of a range, if any.
    pub fn end(&self) -> Option<&Numeric> {
        match self {
            Self::EscapedRange(_, e) => Some(e),
            Self::Range(_, e) => Some(e),
            Self::SinglePage(_) => None,
            _ => None,
        }
    }

    /// Order the values according to CSL rules.
    pub(crate) fn csl_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Ampersand, Self::Ampersand) => Ordering::Equal,
            (Self::Ampersand, _) => Ordering::Less,
            (_, Self::Ampersand) => Ordering::Greater,
            (Self::Comma, Self::Comma) => Ordering::Equal,
            (Self::Comma, _) => Ordering::Less,
            (_, Self::Comma) => Ordering::Greater,
            (Self::SinglePage(n1), Self::SinglePage(n2)) => n1.csl_cmp(n2),
            (Self::SinglePage(_), _) => Ordering::Less,
            (_, Self::SinglePage(_)) => Ordering::Greater,
            (Self::EscapedRange(s1, e1), Self::EscapedRange(s2, e2)) => {
                let ord = s1.csl_cmp(s2);
                if ord != Ordering::Equal {
                    return ord;
                }
                e1.csl_cmp(e2)
            }
            (Self::EscapedRange(_, _), _) => Ordering::Less,
            (_, Self::EscapedRange(_, _)) => Ordering::Greater,
            (Self::Range(s1, e1), Self::Range(s2, e2)) => {
                let ord = s1.csl_cmp(s2);
                if ord != Ordering::Equal {
                    return ord;
                }
                e1.csl_cmp(e2)
            }
        }
    }
}

impl From<i32> for PageRangesPart {
    fn from(value: i32) -> Self {
        Self::SinglePage(value.into())
    }
}

impl TryFrom<u32> for PageRangesPart {
    type Error = TryFromIntError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let value: i32 = value.try_into()?;
        Ok(Self::SinglePage(value.into()))
    }
}

impl TryFrom<i64> for PageRangesPart {
    type Error = TryFromIntError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        let value: i32 = value.try_into()?;
        Ok(Self::SinglePage(value.into()))
    }
}

impl TryFrom<u64> for PageRangesPart {
    type Error = TryFromIntError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        let value: i32 = value.try_into()?;
        Ok(Self::SinglePage(value.into()))
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

/// Parsing error for page ranges.
#[derive(Debug, Clone, Copy, Error)]
pub enum PageRangesPartErr {
    /// The string is malformed.
    #[error("page range string malformed")]
    Malformed,
    /// The string is empty.
    #[error("page range is empty")]
    Empty,
    /// An error from parsing a numeric value.
    #[error("page range contained invalid numeric value")]
    NumericErr(#[from] NumericError),
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
            let mut parts = s.split(['-', '–']).map(str::trim);
            let r = match (parts.next(), parts.next()) {
                (None, None) => unreachable!(),
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

serialize_display!(PageRanges);

fn parse_number(s: &str) -> Result<Numeric, NumericError> {
    Numeric::from_str(s)
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
            let mut len =
                self.string.chars().next().map(char::len_utf8).unwrap_or_default();
            for (c, d) in self.string.chars().zip(self.string.chars().skip(1)) {
                if (self.predicate)(c, d) {
                    len += d.len_utf8();
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

#[cfg(test)]
mod test {
    #[test]
    fn group_by() {
        fn group(s: &str) -> Vec<&'_ str> {
            super::group_by(s, |c, d| !(c == ',' || c == '&' || d == ',' || d == '&'))
                .collect()
        }
        assert_eq!(["a"], group("a").as_slice());
        assert_eq!(["a", ","], group("a,").as_slice());
        assert_eq!([",", "a"], group(",a").as_slice());
        assert_eq!([",", "a", ","], group(",a,").as_slice());
        assert_eq!(["a", ",", "b"], group("a,b").as_slice());
        assert_eq!(["a-"], group("a-").as_slice());
        // characters that are longer than 1 byte
        assert_eq!(["a–"], group("a–").as_slice());
        assert_eq!(["–a"], group("–a").as_slice());
        assert_eq!(["–a", ","], group("–a,").as_slice());
        assert_eq!(["a–", ",", "–b"], group("a–,–b").as_slice());
    }
}
