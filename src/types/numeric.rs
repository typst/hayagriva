use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};
use std::fmt::Write;
use std::fmt::{self, Display};
use std::str::FromStr;

use citationberg::{GrammarGender, NumberForm, OrdinalLookup};
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize};
use thiserror::Error;
use unscanny::Scanner;

use super::MaybeTyped;

/// A numeric value that can be pluralized.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Numeric {
    /// The numeric value.
    pub value: NumericValue,
    /// A string that is prepended to the value.
    pub prefix: Option<Box<String>>,
    /// A string that is appended to the value.
    pub suffix: Option<Box<String>>,
}

impl<'de> Deserialize<'de> for Numeric {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Error;
        struct OurVisitor;

        /// The visitor parses numbers and strings.
        impl<'de> Visitor<'de> for OurVisitor {
            type Value = Numeric;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a numeric value with optional prefix and suffix")
            }

            /// A default serde fallthrough handler for unsigned integers.
            fn visit_u64<E: Error>(self, v: u64) -> Result<Self::Value, E> {
                Ok(Numeric::new(v.try_into().map_err(|_| E::custom("value too large"))?))
            }

            /// A default serde fallthrough handler for signed integers.
            fn visit_i64<E: Error>(self, v: i64) -> Result<Self::Value, E> {
                Ok(Numeric::new(v.try_into().map_err(|_| E::custom("value too large"))?))
            }

            fn visit_i32<E: Error>(self, v: i32) -> Result<Self::Value, E> {
                Ok(Numeric::new(v))
            }

            fn visit_str<E: Error>(self, v: &str) -> Result<Self::Value, E> {
                Self::Value::from_str(v).map_err(|e| E::custom(e.to_string()))
            }
        }

        deserializer.deserialize_any(OurVisitor)
    }
}

impl Serialize for Numeric {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.value {
            NumericValue::Number(n) if self.will_transform() => {
                serializer.serialize_i32(n)
            }
            _ => serializer.serialize_str(&self.to_string()),
        }
    }
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

    /// Retrieve the prefix string slice.
    pub fn prefix_str(&self) -> Option<&str> {
        self.prefix.as_deref().map(String::as_str)
    }

    /// Retrieve the suffix string slice.
    pub fn suffix_str(&self) -> Option<&str> {
        self.suffix.as_deref().map(String::as_str)
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

    /// Format the value with a given form.
    pub fn with_form<T>(
        &self,
        buf: &mut T,
        form: NumberForm,
        gender: Option<GrammarGender>,
        ords: &OrdinalLookup<'_>,
    ) -> std::fmt::Result
    where
        T: Write,
    {
        let format = |n: i32, buf: &mut T| -> std::fmt::Result {
            match form {
                NumberForm::Ordinal => {
                    write!(buf, "{}{}", n, ords.lookup(n, gender).unwrap_or_default())
                }
                NumberForm::LongOrdinal => match ords.lookup_long(n) {
                    Some(str) => buf.write_str(str),
                    None => {
                        write!(buf, "{}{}", n, ords.lookup(n, gender).unwrap_or_default())
                    }
                },
                NumberForm::Roman if n > 0 && n <= i16::MAX as i32 => {
                    write!(buf, "{:x}", numerals::roman::Roman::from(n as i16))
                }
                NumberForm::Numeric | NumberForm::Roman => write!(buf, "{}", n),
            }
        };

        match &self.value {
            &NumericValue::Number(n) => format(n, buf)?,
            NumericValue::Set(s) => {
                for &(n, sep) in s {
                    format(n, buf)?;
                    if let Some(sep) = sep {
                        write!(buf, "{}", sep)?
                    }
                }
            }
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

    /// Returns the nth number in the set.
    pub fn nth(&self, n: usize) -> Option<i32> {
        match &self.value {
            NumericValue::Number(val) if n == 0 => Some(*val),
            NumericValue::Number(_) => None,
            NumericValue::Set(vec) => vec.get(n).map(|(val, _)| *val),
        }
    }

    /// Order the values according to CSL rules.
    pub(crate) fn csl_cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut i = 0;
        loop {
            let a = self.nth(i);
            let b = other.nth(i);

            match (a, b) {
                (Some(a), Some(b)) => {
                    let ord = a.cmp(&b);
                    if ord != std::cmp::Ordering::Equal {
                        return ord;
                    }
                }
                (Some(_), None) => return std::cmp::Ordering::Greater,
                (None, Some(_)) => return std::cmp::Ordering::Less,
                (None, None) => return std::cmp::Ordering::Equal,
            }

            i += 1;
        }
    }
}

impl<'a> MaybeTyped<Cow<'a, Numeric>> {
    /// Order the values according to CSL rules.
    pub(crate) fn csl_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (MaybeTyped::Typed(a), MaybeTyped::Typed(b)) => a.csl_cmp(b),
            _ => self.to_string().cmp(&other.to_string()),
        }
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
                s.eat_until(|c: char| !is_delimiter(c));
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
            prefix: if prefix.is_empty() {
                None
            } else {
                Some(Box::new(prefix.to_string()))
            },
            suffix: if post.is_empty() { None } else { Some(Box::new(post.to_string())) },
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumericValue {
    /// A single number.
    Number(i32),
    /// A set of numbers.
    Set(Vec<(i32, Option<NumericDelimiter>)>),
}

/// Delimits individual numbers in a numeric value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        if first_char != '-' && s.len() > first_char.len_utf8() {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mixed_range() {
        let s = "34,37--39";
        let n: Numeric = s.parse().unwrap();
        assert_eq!(
            n.value,
            NumericValue::Set(vec![
                (34, Some(NumericDelimiter::Comma)),
                (37, Some(NumericDelimiter::Hyphen)),
                (39, None)
            ])
        );
    }
}
