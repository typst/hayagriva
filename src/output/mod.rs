//! Citation and bibliography styles.

use super::types::Person;
use super::{sel, Entry};
use crate::selectors::Id;
use crate::types::EntryType::{Chapter, Scene};
use std::collections::HashMap;
use std::convert::Into;
use std::fmt::Write;
use std::ops::{Add, AddAssign};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

pub mod apa;
pub mod chicago;
pub mod ieee;
pub mod mla;

/// Describes a pair of brackets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bracket {
    /// "(...)" Parentheses.
    Parentheses,
    /// "[...]" Brackets.
    SquareBrackets,
    /// No brackets should be used.
    None,
}

/// Describes a pair of brackets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketMode {
    /// Expression is wrapped by [`Bracket`]s
    Wrapped,
    /// Expression is not wrapped by brackets.
    Unwrapped,
}

impl Bracket {
    /// Get the according left bracket.
    pub fn left(&self) -> Option<char> {
        match self {
            Bracket::Parentheses => Some('('),
            Bracket::SquareBrackets => Some('['),
            Bracket::None => None,
        }
    }

    /// Get the according right bracket.
    pub fn right(&self) -> Option<char> {
        match self {
            Bracket::Parentheses => Some(')'),
            Bracket::SquareBrackets => Some(']'),
            Bracket::None => None,
        }
    }

    /// Wrap some string in brackets.
    pub fn wrap(&self, s: &mut String) {
        if let Some(left) = self.left() {
            s.insert(0, left)
        }
        if let Some(right) = self.right() {
            s.push(right)
        }
    }

    /// Wrap some [`DisplayString`] in brackets.
    pub fn wrap_ds(&self, mut ds: DisplayString) -> DisplayString {
        if let Some(left) = self.left() {
            ds = DisplayString::from_string(left.to_string()) + ds;
        }
        if let Some(right) = self.right() {
            ds.push(right)
        }
        ds
    }
}

/// This enum describes where the output string of the [CitationFormatter]
/// should be set: Inside the text or in a
pub enum CitationMode {
    /// Set citation text in a footnote. Only produce a superscript footnote
    /// symbol at the matching position of the text. (footnote numbers are
    /// managed by the callee; may be rendered as endnotes).
    Footnote,
    /// The citation text should be set directly where the text appears.
    InText,
}

/// Will be raised if a user-specified citation is not possible with the given
/// database.
#[derive(Debug, Error)]
pub enum CitationError {
    /// A key could not be found.
    #[error("key {0} could not be fount in the citation database")]
    KeyNotFound(String),
    /// A number was required for this citation format but not found.
    #[error("key {0} did not contain a number")]
    NoNumber(String),
}

/// Structs that implement this can be used to generate bibliography references
/// for sources.
pub trait BibliographyFormatter {
    /// Get a string with optional formatting that describes `Entry` in
    /// accordance with the implementing struct's style.
    fn format(&self, entry: &Entry, prev_entry: Option<&Entry>) -> DisplayString;
}

/// Structs that can be used to get some string can implement this trait
/// in order to indicate what kind of brackets their output is normally
/// wrapped in.
pub trait BracketPreference {
    /// Indicates what brackets the output is normally wrapped in.
    fn default_brackets() -> Bracket;
    /// Indicates whether the output is normally wrapped at all.
    fn default_bracket_mode() -> BracketMode;
}

/// Get citations wrapped in the right pair of brackets.
/// This trait will be automatically implemented for all structs that are
/// [`CitationFormatter`]s and have a [`BracketPreference`].
pub trait BracketCitFormatter<'s> {
    /// Get the citation in brackets if the citation style prefers that.
    fn format_bracketed_default(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError>;

    /// Get the citation with or without surrounding brackets, as specified
    /// by the `mode` argument.
    fn format_bracketed(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
        mode: BracketMode,
    ) -> Result<DisplayString, CitationError>;
}

impl<'s, T> BracketCitFormatter<'s> for T
where
    T: CitationFormatter<'s> + BracketPreference,
{
    fn format_bracketed_default(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        self.format_bracketed(citation, Self::default_bracket_mode())
    }

    fn format_bracketed(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
        mode: BracketMode,
    ) -> Result<DisplayString, CitationError> {
        if mode == BracketMode::Wrapped {
            self.format(citation).map(|ds| Self::default_brackets().wrap_ds(ds))
        } else {
            self.format(citation)
        }
    }
}

/// Represents a citation of one or more database entries.
#[derive(Clone, Copy, Debug)]
pub struct AtomicCitation<'s> {
    /// Key of the cited entry.
    pub key: &'s str,
    /// Supplements for each entry key such as page or chapter number.
    pub supplement: Option<&'s str>,
    /// Assigned number of the citation.
    pub number: Option<usize>,
}

impl<'s> AtomicCitation<'s> {
    /// Create a new atomic citation.
    pub fn new(key: &'s str, supplement: Option<&'s str>, number: Option<usize>) -> Self {
        Self { key, supplement, number }
    }
}

/// Structs implementing this trait can generate the appropriate reference
/// markers for a single `Citation` struct. They do not have to see subsequent
/// citations to determine the marker value.
pub trait CitationFormatter<'s> {
    /// Get a reference for the passed citation struct.
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError>;
}

/// Checks if the keys are in the database and returns them as reference
/// markers, since they are already unique.
pub struct KeyCitationFormatter<'s> {
    entries: HashMap<&'s str, &'s Entry>,
}

impl<'s> KeyCitationFormatter<'s> {
    /// Create a new `KeyCitationFormatter`.
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        let entries = entries.map(|e| (e.key.as_ref(), e)).collect();
        Self { entries }
    }
}

impl<'s> CitationFormatter<'s> for KeyCitationFormatter<'s> {
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        let mut items = vec![];
        for atomic in citation {
            if !self.entries.contains_key(atomic.key) {
                return Err(CitationError::KeyNotFound(atomic.key.to_string()));
            }

            items.push(if let Some(supplement) = atomic.supplement {
                format!("{} ({})", atomic.key, supplement)
            } else {
                atomic.key.to_string()
            });
        }

        let mut res = DisplayString::new();
        res.start_format(Formatting::Bold);
        res += &items.join(", ");
        res.commit_formats();
        Ok(res)
    }
}

impl<'s> BracketPreference for KeyCitationFormatter<'s> {
    fn default_brackets() -> Bracket {
        Bracket::SquareBrackets
    }

    fn default_bracket_mode() -> BracketMode {
        BracketMode::Unwrapped
    }
}

/// Output IEEE-style numerical reference markers.
pub struct NumericalCitationFormatter<'s> {
    entries: HashMap<&'s str, &'s Entry>,
}

impl<'s> NumericalCitationFormatter<'s> {
    /// Create a new `NumericalCitationFormatter`.
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        let entries = entries.map(|e| (e.key.as_ref(), e)).collect();
        Self { entries }
    }
}

impl<'s> CitationFormatter<'s> for NumericalCitationFormatter<'s> {
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        let mut ids = vec![];
        for atomic in citation {
            if !self.entries.contains_key(atomic.key) {
                return Err(CitationError::KeyNotFound(atomic.key.to_string()));
            }

            let number = atomic
                .number
                .ok_or_else(|| CitationError::NoNumber(atomic.key.to_string()))?;
            ids.push((number, atomic.supplement));
        }

        ids.sort_by(|(a, _), (b, _)| a.cmp(&b));

        enum CiteElement<'a> {
            Range(std::ops::Range<usize>),
            Single((usize, Option<&'a str>)),
        }

        let mut res_elems = vec![];

        for (number, supplement) in ids {
            if let Some(s) = supplement {
                res_elems.push(CiteElement::Single((number, Some(s))));
                continue;
            }

            match res_elems.last() {
                Some(CiteElement::Range(r)) if r.end == number - 1 => {
                    let mut r = r.clone();
                    res_elems.pop().unwrap();
                    r.end = number;
                    res_elems.push(CiteElement::Range(r));
                }
                _ if supplement.is_some() => {
                    res_elems.push(CiteElement::Single((number, supplement)));
                }
                _ => {
                    res_elems.push(CiteElement::Range(number .. number));
                }
            }
        }

        let re = res_elems
            .into_iter()
            .map(|e| match e {
                CiteElement::Range(r) if r.start != r.end => {
                    format!("{}-{}", r.start, r.end)
                }
                CiteElement::Range(r) => r.start.to_string(),
                CiteElement::Single((n, s)) => {
                    if let Some(sup) = s {
                        format!("{}, {}", n, sup)
                    } else {
                        n.to_string()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join("; ");

        Ok(format!("[{}]", re).into())
    }
}

impl<'s> BracketPreference for NumericalCitationFormatter<'s> {
    fn default_brackets() -> Bracket {
        Bracket::SquareBrackets
    }

    fn default_bracket_mode() -> BracketMode {
        BracketMode::Wrapped
    }
}

fn format_range<T: std::fmt::Display + PartialEq>(
    prefix_s: &str,
    prefix_m: &str,
    range: &std::ops::Range<T>,
) -> String {
    let space = if prefix_s.is_empty() { "" } else { " " };
    if range.start == range.end {
        format!("{}{}{}", prefix_s, space, range.start)
    } else {
        format!("{}{}{}–{}", prefix_m, space, range.start, range.end)
    }
}

fn name_list(persons: &[Person]) -> Vec<String> {
    let mut names = vec![];

    for author in persons.iter() {
        names.push(author.name_first(true, false));
    }

    names
}

fn name_list_straight(persons: &[Person]) -> Vec<String> {
    let mut names = vec![];

    for author in persons.iter() {
        names.push(author.given_first(true));
    }

    names
}

/// Formatting modifiers for strings.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Formatting {
    /// **Bold print**
    Bold,
    /// _italic print_
    Italic,
    /// Do not hyphenate, e.g. for URLs.
    NoHyphenation,
}

/// Will move a format range's indicies by `o`.
pub(crate) fn offset_format_range(
    r: (std::ops::Range<usize>, Formatting),
    o: usize,
) -> (std::ops::Range<usize>, Formatting) {
    ((r.0.start + o) .. (r.0.end + o), r.1)
}

/// A printable string with a list of formatting modifications
#[derive(Clone, Debug)]
pub struct DisplayString {
    /// The string content.
    pub value: String,
    /// Information about formatted ranges.
    pub formatting: Vec<(std::ops::Range<usize>, Formatting)>,

    pending_formatting: Vec<(usize, Formatting)>,
}

impl Add<&str> for DisplayString {
    type Output = DisplayString;

    #[inline]
    fn add(mut self, other: &str) -> DisplayString {
        self.value.push_str(other);
        self
    }
}

impl Add<Self> for DisplayString {
    type Output = Self;

    #[inline]
    fn add(mut self, other: Self) -> Self {
        self.formatting.append(
            &mut other
                .formatting
                .into_iter()
                .map(|e| offset_format_range(e, self.value.len()))
                .collect(),
        );
        self.value.push_str(&other.value);
        self
    }
}

impl AddAssign<&String> for DisplayString {
    fn add_assign(&mut self, other: &String) {
        self.value.push_str(other);
    }
}

impl AddAssign<&str> for DisplayString {
    fn add_assign(&mut self, other: &str) {
        self.value.push_str(other);
    }
}

impl AddAssign<Self> for DisplayString {
    fn add_assign(&mut self, other: Self) {
        self.formatting.append(
            &mut other
                .formatting
                .into_iter()
                .map(|e| offset_format_range(e, self.value.len()))
                .collect(),
        );
        self.value.push_str(&other.value);
    }
}

impl Into<String> for DisplayString {
    fn into(self) -> String {
        self.value
    }
}

impl Into<DisplayString> for String {
    fn into(self) -> DisplayString {
        DisplayString::from_string(self)
    }
}

impl Into<DisplayString> for &str {
    fn into(self) -> DisplayString {
        DisplayString::from_string(self)
    }
}

impl DisplayString {
    /// Constructs a new DisplayString.
    pub fn new() -> Self {
        Self {
            value: String::new(),
            formatting: vec![],
            pending_formatting: vec![],
        }
    }

    /// Use a String to create a display string.
    pub fn from_string<S: Into<String>>(s: S) -> Self {
        Self {
            value: s.into(),
            formatting: vec![],
            pending_formatting: vec![],
        }
    }

    /// Get the length of the string.
    pub fn len(&self) -> usize {
        self.value.len()
    }

    /// Is the string empty?
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    /// Get the last character.
    pub fn last(&self) -> Option<char> {
        self.value.chars().last()
    }

    /// Push onto the string.
    pub fn push(&mut self, ch: char) {
        self.value.push(ch);
    }

    pub(crate) fn start_format(&mut self, f: Formatting) {
        debug_assert!(!self.pending_formatting.iter().any(|e| e.1 == f));
        self.pending_formatting.push((self.len(), f));
    }

    /// Resets all of the formatting.
    pub fn clear_formatting(&mut self) {
        self.formatting.clear();
    }

    pub(crate) fn commit_formats(&mut self) {
        for (start, f) in self.pending_formatting.iter() {
            self.formatting.push((*start .. self.len(), *f))
        }

        self.pending_formatting.clear();
    }

    pub(crate) fn add_if_some<S: Into<String>>(
        &mut self,
        item: Option<S>,
        prefix: Option<&str>,
        postfix: Option<&str>,
    ) {
        if let Some(item) = item {
            if let Some(prefix) = prefix {
                *self += prefix;
            }
            *self += &item.into();
            if let Some(postfix) = postfix {
                *self += postfix;
            }
        }
    }

    /// Joins a number of display strings with a seperator in-between.
    pub fn join(items: &[Self], joiner: &str) -> Self {
        let mut res = DisplayString::new();
        for (i, e) in items.iter().enumerate() {
            if i != 0 {
                res += joiner;
            }

            res += e.clone();
        }

        res
    }

    /// Applies the formatting as ANSI / VT100 control sequences and
    /// prints that formatted string to standard output.
    pub fn print_ansi_vt100(&self) -> String {
        let mut start_end = vec![];

        for item in &self.formatting {
            let opt = item.1;
            if opt == Formatting::NoHyphenation {
                continue;
            }
            let min = item.0.start;
            let max = item.0.end;

            start_end.push((opt.clone(), min, false));
            start_end.push((opt, max, true));
        }

        start_end.sort_by(|a, b| a.1.cmp(&b.1).reverse());

        let mut res = String::new();
        let mut pointer = self.len();

        for (f, index, end) in &start_end {
            res = (&self.value[*index .. pointer]).to_string() + &res;
            pointer = *index;

            let code = if *end {
                "0"
            } else {
                match f {
                    Formatting::Bold => "1",
                    Formatting::Italic => "3",
                    Formatting::NoHyphenation => unreachable!(),
                }
            };
            res = format!("\x1b[{}m", code) + &res;
        }
        res = (&self.value[0 .. pointer]).to_string() + &res;

        res
    }
}

fn push_comma_quote_aware(s: &mut String, comma: char, space: bool) {
    let cur_len = s.len();
    if cur_len > 3 && s.is_char_boundary(cur_len - 3) && &s[cur_len - 3 ..] == "”" {
        s.truncate(cur_len - 3);
        if s.chars().last() != Some(comma) {
            s.push(comma);
            s.push_str("”");
        }
    } else if !s.is_empty() {
        if s.chars().last() != Some(comma) {
            s.push(comma);
        }
    }

    if space && !s.is_empty() {
        s.push(' ');
    }
}

fn abbreviate_publisher(s: &str, up: bool) -> String {
    let s1 = if up {
        s.replace("University Press", "UP")
            .replace("University", "U")
            .replace("Universität", "U")
            .replace("Université", "U")
            .replace("Press", "P")
            .replace("Presse", "P")
    } else {
        s.into()
    };
    let business_words = [
        "Co",
        "Co.",
        "Corp",
        "Corp.",
        "Corporated",
        "Corporation",
        "Inc",
        "Inc.",
        "Incorporated",
        "Limited",
        "Ltd",
        "Ltd.",
        "S A",
        "S.A.",
        "Sociedad Anónima",
        "Société Anonyme",
    ];
    s1.split(' ')
        .filter(|w| !w.is_empty() && business_words.binary_search(w).is_err())
        .collect::<Vec<_>>()
        .join(" ")
}

fn delegate_titled_entry(mut entry: &Entry) -> &Entry {
    let mut parent = entry.parents().and_then(|v| v.first());
    while sel!(alt Id(Chapter), Id(Scene)).matches(entry) && entry.title().is_none() {
        if let Some(p) = parent {
            entry = &p;
            parent = entry.parents().and_then(|v| v.first());
        } else {
            break;
        }
    }

    entry
}

fn alph_designator(pos: usize) -> char {
    (b'a' + (pos % 26) as u8) as char
}
/// Output citations like "Oba20" or "KMS96".
pub struct Alphabetical<'s> {
    entries: HashMap<&'s str, &'s Entry>,
    /// How many letters to allow to describe an entry.
    pub letters: usize,
}

impl<'s> Alphabetical<'s> {
    /// Create a new `NumericalCitationFormatter`.
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        let entries = entries.map(|e| (e.key.as_ref(), e)).collect();
        Self { entries, letters: 3 }
    }
}

impl<'s> CitationFormatter<'s> for Alphabetical<'s> {
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        let mut items = vec![];
        for atomic in citation {
            let entry = self
                .entries
                .get(atomic.key)
                .ok_or_else(|| CitationError::KeyNotFound(atomic.key.into()))?;

            let creators = chicago::get_creators(entry).0;
            let mut res = match creators.len() {
                0 => if let Some(org) = entry.organization() {
                    org.into()
                } else if let Some(title) = delegate_titled_entry(entry).title() {
                    title.into()
                } else {
                    atomic.key.chars().filter(|c| c.is_alphabetic()).collect::<String>()
                }
                .graphemes(true)
                .enumerate()
                .filter(|(i, _)| *i < self.letters)
                .map(|(_, e)| e)
                .collect::<String>(),
                1 => creators[0]
                    .name
                    .graphemes(true)
                    .enumerate()
                    .filter(|(i, _)| *i < self.letters)
                    .map(|(_, e)| e)
                    .collect::<String>(),
                2 | 3 => {
                    let mut res = String::new();
                    for person in creators {
                        res += person.name.graphemes(true).next().unwrap_or_default();
                    }
                    res
                }
                _ => {
                    let mut s = creators[0]
                        .name
                        .graphemes(true)
                        .enumerate()
                        .filter(|(i, _)| *i < self.letters)
                        .map(|(_, e)| e)
                        .collect::<String>();
                    s.push('+');
                    s
                }
            };

            let year = entry
                .any_date()
                .or_else(|| entry.any_url().and_then(|u| u.visit_date.as_ref()))
                .map(|date| {
                    let mut year = i32::abs(date.year % 100);
                    if date.year <= 0 {
                        year += 1;
                    }
                    year
                });

            if let Some(year) = year {
                let mut num = String::with_capacity(2);
                write!(&mut num, "{:02}", year).unwrap();
                res += &num;
            }

            if let Some(number) = atomic.number {
                res.push(alph_designator(number));
            }

            items.push(res);
        }

        Ok(items.join("; ").into())
    }
}

impl<'s> BracketPreference for Alphabetical<'s> {
    fn default_brackets() -> Bracket {
        Bracket::SquareBrackets
    }

    fn default_bracket_mode() -> BracketMode {
        BracketMode::Wrapped
    }
}
