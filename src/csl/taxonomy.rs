use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt::Write;

use crate::types::{Date, Person};
use crate::Entry;
use citationberg::{taxonomy, LongShortForm, NumberForm, OrdinalLookup};
use unscanny::Scanner;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Numeric {
    value: NumericValue,
    prefix: Option<String>,
    suffix: Option<String>,
}

impl Numeric {
    pub fn will_transform(&self) -> bool {
        self.prefix.is_none() && self.suffix.is_none()
    }

    pub fn with_form<T>(
        &self,
        buf: &mut T,
        form: NumberForm,
        ords: OrdinalLookup<'_>,
    ) -> std::fmt::Result
    where
        T: Write,
    {
        let format = |n: i32, buf: &mut T| -> std::fmt::Result {
            match form {
                NumberForm::Ordinal => {
                    write!(buf, "{}{}", n, ords.lookup(n).unwrap_or_default())
                }
                NumberForm::LongOrdinal => match ords.lookup_long(n) {
                    Some(str) => buf.write_str(str),
                    None => write!(buf, "{}{}", n, ords.lookup(n).unwrap_or_default()),
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

    pub fn is_plural(&self, is_number_of: bool) -> bool {
        match &self.value {
            NumericValue::Number(n) if is_number_of => n != &1,
            NumericValue::Number(_) => false,
            NumericValue::Set(vec) => vec.len() != 1,
        }
    }
}

impl TryFrom<&str> for Numeric {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut s = Scanner::new(value);
        let prefix =
            s.eat_while(|c: char| !c.is_numeric() && !c.is_whitespace() && c != '-');

        let value = number(&mut s).ok_or("No number found")?;
        s.eat_whitespace();

        let value = match s.peek() {
            Some(c) if is_delimiter(c) => {
                s.eat();
                let mut items = vec![(value, Some(Delimiter::try_from(c)?))];
                loop {
                    let num = number(&mut s).ok_or("No number found")?;
                    s.eat_whitespace();
                    match Delimiter::try_from(s.eat_while(is_delimiter)) {
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
            return Err("Unexpected characters after postfix");
        }

        Ok(Self {
            value,
            prefix: if prefix.is_empty() { None } else { Some(prefix.to_string()) },
            suffix: if post.is_empty() { None } else { Some(post.to_string()) },
        })
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

impl std::fmt::Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(prefix) = &self.prefix {
            f.write_str(prefix)?;
        }
        self.with_form(f, NumberForm::Numeric, OrdinalLookup::empty())?;
        if let Some(suffix) = &self.suffix {
            f.write_str(suffix)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NumericValue {
    Number(i32),
    Set(Vec<(i32, Option<Delimiter>)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Delimiter {
    Comma,
    Ampersand,
    Hyphen,
}

impl std::fmt::Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Delimiter::Comma => f.write_str(", "),
            Delimiter::Ampersand => f.write_str(" & "),
            Delimiter::Hyphen => f.write_char('–'),
        }
    }
}

fn is_delimiter(c: char) -> bool {
    c == ',' || c == '&' || c == '-' || c == '–'
}

impl TryFrom<&str> for Delimiter {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let first_char = s.chars().next().ok_or("Empty string")?;
        if s.len() > first_char.len_utf8() {
            return Err("More than one character");
        }

        Self::try_from(first_char)
    }
}

impl TryFrom<char> for Delimiter {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ',' => Ok(Delimiter::Comma),
            '&' => Ok(Delimiter::Ampersand),
            '-' | '–' => Ok(Delimiter::Hyphen),
            _ => Err("Not a delimiter"),
        }
    }
}

/// A type that may be a string or a stricly typed value.
pub enum MaybeTyped<T> {
    /// The typed variant.
    Typed(T),
    /// The fallback string variant.
    String(String),
}

impl<T: ToString> MaybeTyped<T> {
    pub fn to_str(&self) -> Cow<str> {
        match self {
            MaybeTyped::Typed(t) => Cow::Owned(t.to_string()),
            MaybeTyped::String(s) => Cow::Borrowed(s),
        }
    }
}

pub(crate) fn resolve_number_variable(
    entry: &Entry,
    variable: taxonomy::NumberVariable,
) -> Option<MaybeTyped<Numeric>> {
    todo!()
}

// Number variables are standard variables.
pub(crate) fn resolve_standard_variable(
    entry: &Entry,
    form: LongShortForm,
    variable: taxonomy::StandardVariable,
) -> Option<&str> {
    todo!()
}

pub(crate) fn resolve_date_variable(
    entry: &Entry,
    variable: taxonomy::DateVariable,
) -> Option<Date> {
    todo!()
}

pub(crate) fn resolve_name_variable(
    entry: &Entry,
    variable: taxonomy::NameVariable,
) -> Option<Vec<Person>> {
    todo!()
}

mod tests {
    use super::*;

    #[test]
    fn parse_num() {
        let val = Numeric::try_from("1").unwrap();
        assert!(val.value == NumericValue::Number(1));
        assert!(val.prefix.is_none());
        assert!(val.suffix.is_none());
        assert_eq!(&val.to_string(), "1");

        let val = Numeric::try_from("-5").unwrap();
        assert!(val.value == NumericValue::Number(-5));
        assert!(val.prefix.is_none());
        assert!(val.suffix.is_none());
        assert_eq!(&val.to_string(), "-5");

        let val = Numeric::try_from("1st").unwrap();
        assert!(val.value == NumericValue::Number(1));
        assert!(val.prefix.is_none());
        assert!(val.suffix.as_deref() == Some("st"));
        assert_eq!(&val.to_string(), "1st");

        let val = Numeric::try_from("1, 2").unwrap();
        assert!(
            val.value == NumericValue::Set(vec![(1, Some(Delimiter::Comma)), (2, None)])
        );
        assert_eq!(val.to_string(), "1, 2");

        let val = Numeric::try_from("A16y").unwrap();
        assert!(val.value == NumericValue::Number(16));
        assert!(val.prefix.as_deref() == Some("A"));
        assert!(val.suffix.as_deref() == Some("y"));
        assert_eq!(&val.to_string(), "A16y");

        let val = Numeric::try_from("1-4").unwrap();
        assert!(
            val.value == NumericValue::Set(vec![(1, Some(Delimiter::Hyphen)), (4, None)])
        );

        let val_other = Numeric::try_from("1 - 4").unwrap();
        assert_eq!(val, val_other);
        assert_eq!(&val.to_string(), "1–4");

        let val = Numeric::try_from("2 , 3").unwrap();
        assert!(
            val.value == NumericValue::Set(vec![(2, Some(Delimiter::Comma)), (3, None)])
        );
        assert_eq!(&val.to_string(), "2, 3");

        let val = Numeric::try_from("2 & 3 & 4").unwrap();
        assert!(
            val.value
                == NumericValue::Set(vec![
                    (2, Some(Delimiter::Ampersand)),
                    (3, Some(Delimiter::Ampersand)),
                    (4, None)
                ])
        );
        assert_eq!(&val.to_string(), "2 & 3 & 4");

        assert!(Numeric::try_from("second").is_err());
        assert!(Numeric::try_from("2nd edition").is_err());
    }
}
