use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt::Write;

use crate::types::{Date, FmtString, Person};
use crate::Entry;
use citationberg::taxonomy::{NumberVariable, StandardVariable};
use citationberg::{taxonomy, LongShortForm, NumberForm, OrdinalLookup};
use unscanny::Scanner;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Numeric {
    value: NumericValue,
    prefix: Option<String>,
    suffix: Option<String>,
}

impl Numeric {
    pub fn new(value: i32) -> Self {
        Self {
            value: NumericValue::Number(value),
            prefix: None,
            suffix: None,
        }
    }

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
    variable: NumberVariable,
) -> Option<MaybeTyped<Numeric>> {
    match variable {
        NumberVariable::ChapterNumber => entry.get_with_selector(
            "volume",
            &select!(
                (("e":Anthos) > ("p":Anthology)) |
                (("e":*) > ("p":Reference)) |
                (("e":Article) > ("p":Proceedings)) |
                (("e":*) > ("p":Book))
            ),
            "e",
        ),
        NumberVariable::CitationNumber => todo!("that's for us to do baby"),
        NumberVariable::CollectionNumber => entry.get_with_selector(
            "volume",
            &select!(
                (* > ("p":(Book | Anthology | Proceedings)))
            ),
            "p",
        ),
        NumberVariable::Edition => entry.get_recursive("edition"),
        NumberVariable::FirstReferenceNoteNumber => todo!("you guessed it, baybee"),
        NumberVariable::Issue => entry.get_recursive("issue"),
        NumberVariable::Locator => todo!("boy oh boy"),
        NumberVariable::Number => entry.get("serial-number"),
        NumberVariable::NumberOfPages => entry.get("page-total"),
        NumberVariable::NumberOfVolumes => entry.get("volume-total"),
        NumberVariable::Page => entry.get("page-range"),
        NumberVariable::PageFirst => {
            return entry
                .page_range()
                .map(|r| MaybeTyped::Typed(Numeric::new(r.start as i32)))
        }
        NumberVariable::PartNumber => entry.get_with_selector(
            "volume",
            &select!(
                (("e":*) > (Article | Blog | Book | Legislation))
            ),
            "e",
        ),
        NumberVariable::PrintingNumber => None,
        NumberVariable::Section => None,
        NumberVariable::SupplementNumber => None,
        NumberVariable::Version => {
            entry.get_with_selector("edition", &select!(("e":Repository)), "e")
        }
        NumberVariable::Volume => entry.get("volume"),
    };

    todo!()
}

// Number variables are standard variables.
pub(crate) fn resolve_standard_variable(
    entry: &Entry,
    form: LongShortForm,
    variable: StandardVariable,
) -> Option<&str> {
    match variable {
        StandardVariable::Abstract => None,
        StandardVariable::Annote => None,
        StandardVariable::Archive => entry.get_recursive("archive"),
        StandardVariable::ArchiveCollection => None,
        StandardVariable::ArchiveLocation => entry.get("archive-location"),
        StandardVariable::ArchivePlace => None,
        StandardVariable::Authority => entry.get("organization"),
        StandardVariable::CallNumber => None,
        StandardVariable::CitationKey => return Some(entry.key.as_str()),
        // The spec tells us that the CSL processor may assign this, we do not.
        StandardVariable::CitationLabel => None,
        // Get third-order title first, then second-order title.
        StandardVariable::CollectionTitle => entry
            .parents()
            .unwrap_or(&[])
            .iter()
            .find_map(|p| p.get_parents("title"))
            .or_else(|| entry.get_parents("title")),
        StandardVariable::ContainerTitle => entry.get_parents("title"),
        StandardVariable::ContainerTitleShort => None,
        StandardVariable::Dimensions => entry.get("runtime"),
        StandardVariable::Division => None,
        StandardVariable::DOI => entry.get("doi"),
        StandardVariable::Event | StandardVariable::EventTitle => entry
            .get_with_selector(
                "title",
                &select!(* > ("p":(Exhibition | Conference | Misc))),
                "p",
            ),
        StandardVariable::EventPlace => entry.get_with_selector(
            "location",
            &select!(* > ("p":(Exhibition | Conference | Misc))),
            "p",
        ),
        StandardVariable::Genre => None,
        StandardVariable::ISBN => entry.get("isbn"),
        StandardVariable::ISSN => entry.get("issn"),
        StandardVariable::Jurisdiction => None,
        StandardVariable::Keyword => None,
        StandardVariable::Language => entry.get_recursive("language"),
        StandardVariable::License => None,
        StandardVariable::Medium => None,
        StandardVariable::Note => entry.get("note"),
        StandardVariable::OriginalPublisher => None,
        StandardVariable::OriginalPublisherPlace => None,
        StandardVariable::OriginalTitle => None,
        StandardVariable::PartTitle => None,
        StandardVariable::PMCID => None,
        StandardVariable::PMID => None,
        StandardVariable::Publisher => entry.get_recursive("publisher"),
        StandardVariable::PublisherPlace => None,
        StandardVariable::References => None,
        StandardVariable::ReviewedGenre => None,
        StandardVariable::ReviewedTitle => entry.get_parents("title"),
        StandardVariable::Scale => None,
        StandardVariable::Source => {
            entry.get_with_selector("title", &select!(* > ("p":Repository)), "p")
        }
        StandardVariable::Status => None,
        StandardVariable::Title => entry.get("title"),
        StandardVariable::TitleShort => None,
        StandardVariable::URL => entry.get_recursive("url"),
        StandardVariable::VolumeTitle => {
            let selector = select!(
                (Anthos > ("p":Anthology)) |
                (Entry  > ("p":*)) |
                (* > ("p":Reference)) |
                (Article > ("p":Proceedings))
            );
            entry.get_with_selector("title", &selector, "p")
        }
        StandardVariable::YearSuffix => todo!("we actually have to generate this"),
    }
    .and_then(|v| <&FmtString>::try_from(v).ok())
    .map(|v: &FmtString| v.value.as_str())
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
) -> Option<Vec<&Person>> {
    todo!()
}

pub(crate) fn matches_entry_type(
    entry: &Entry,
    kind: citationberg::taxonomy::Kind,
) -> bool {
    todo!()
}

#[cfg(test)]
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
