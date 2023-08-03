//! Citation and bibliography styles.

mod apa;
mod chicago;
mod ieee;
mod mla;

pub use apa::Apa;
pub use chicago::author_date::ChicagoAuthorDate;
pub use chicago::notes::{ChicagoNoteStyle, ChicagoNotes};
pub use chicago::{ChicagoAccessDateVisibility, ChicagoConfig};
pub use ieee::Ieee;
pub use mla::Mla;

use std::fmt::{self, Debug, Display, Formatter, Write};
use std::ops::{Add, AddAssign};
use std::{cmp::Ordering, convert::Into};

use isolang::Language;
use linked_hash_map::LinkedHashMap;
use unicode_segmentation::UnicodeSegmentation;

use super::types::Person;
use super::Entry;

/// A database record that contains some style-set supplementary info.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct Record<'a> {
    /// The entry the record is associated with.
    pub entry: &'a Entry,
    /// A prefix set by the citation styles that is reproduced in the bibliography.
    pub prefix: Option<String>,
    /// A disambiguation between similar entries set by the citation styles that is reproduced in the bibliography.
    ///
    /// Often turns up as `0` becomes `a` and so forth.
    pub disambiguation: Option<usize>,
}

impl<'a> Record<'a> {
    /// Create a new database record from an entry.
    ///
    /// Will default to `None` for the `prefix` and `disambiguation` fields.
    pub(crate) fn from_entry(entry: &'a Entry) -> Self {
        Self { entry, prefix: None, disambiguation: None }
    }
}

/// A citation of a single entry.
#[derive(Copy, Clone, Debug)]
pub struct Citation<'a> {
    /// The cited entry.
    pub entry: &'a Entry,
    /// Supplement for the entry such as page or chapter number.
    pub supplement: Option<&'a str>,
}

impl<'a> Citation<'a> {
    /// Create a new citation.
    pub fn new(entry: &'a Entry, supplement: Option<&'a str>) -> Self {
        Self { entry, supplement }
    }
}

/// Contains the [`DisplayString`] for a citation and indicates its recommended
/// placement within a document.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct DisplayCitation {
    /// The formatted citation.
    pub display: DisplayString,
    /// Whether this citation is, in fact, a footnote.
    pub is_footnote: bool,
}

impl DisplayCitation {
    /// Create a new [`DisplayCitation`]
    pub fn new(display: DisplayString, is_footnote: bool) -> Self {
        Self { display, is_footnote }
    }
}

/// Contains the [`DisplayString`] for a bibliography reference as well as
/// the prefix nececcitated by any previously used citation styles and a
/// reference to the matching [`Entry`].
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct DisplayReference<'a> {
    /// The cited entry.
    pub entry: &'a Entry,
    /// The prefix of the reference.
    pub prefix: Option<DisplayString>,
    /// The formatted reference.
    pub display: DisplayString,
}

impl<'a> DisplayReference<'a> {
    /// Create a new reference for display purposes.
    pub fn new(
        entry: &'a Entry,
        prefix: Option<DisplayString>,
        display: DisplayString,
    ) -> Self {
        DisplayReference { entry, prefix, display }
    }
}

/// A database of citation entries.
#[derive(Clone, Default, Debug, PartialEq)]
#[non_exhaustive]
pub struct Database<'a> {
    /// Records in order of insertion. Citation style might change their content.
    pub records: LinkedHashMap<&'a str, Record<'a>>,
}

impl<'a> Database<'a> {
    /// Create a new citation database.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new database from a collection of entries.
    ///
    /// A word of caution on entries that are added here but not cited
    /// can be found in [`Self::push`]. All items included here will appear
    /// in the bibliography.
    pub fn from_entries(entries: impl IntoIterator<Item = &'a Entry>) -> Self {
        let mut res = Self::new();
        for entry in entries {
            res.push(entry);
        }
        res
    }

    /// Push an entry into the database, making it part of the resulting
    /// bibliography.
    ///
    /// If you push an entry, but do not cite it afterwards, some more complex
    /// citation elements may become malformed (e.g. if you push two entries
    /// with same author and year, but cite only one of them, Chicago
    /// Author-Date will format this one as "2020A" instead of "2020").
    pub fn push(&mut self, entry: &'a Entry) {
        if !self.records.contains_key(entry.key()) {
            let record = Record::from_entry(entry);
            self.records.insert(record.entry.key(), record);
        }
    }

    fn records(&self) -> linked_hash_map::Values<&'a str, Record<'a>> {
        self.records.values()
    }

    /// Cite entries and format them with the given style.
    ///
    /// The `parts` are the individual cited entries in a composite citation
    /// like "[1 p. 5, 2]".
    ///
    /// If you cite an entry that has not yet been pushed, it is pushed for you
    /// automatically. However, some more complex citation elements may become
    /// malformed (e.g. if you cite two entries with the same author and year,
    /// but pushed only one of them, Chicago Author-Date will format them as
    /// "2020" and "2020A" instead of "2020A" and "2020B").
    pub fn citation<S>(
        &mut self,
        style: &mut S,
        parts: &[Citation<'a>],
    ) -> DisplayCitation
    where
        S: CitationStyle<'a> + ?Sized,
    {
        for p in parts {
            self.push(p.entry);
        }

        style.citation(self, parts)
    }

    /// Format a bibliography of all cited entries with the given style.
    ///
    /// The bibliography is ordered as specified by the `ordering`. If it is
    /// `None`, the style's [default ordering](BibliographyStyle::ordering) is
    /// applied.
    ///
    /// Returns the entries along with their styled references.
    pub fn bibliography<S>(
        &self,
        style: &S,
        ordering: Option<BibliographyOrdering>,
    ) -> Vec<DisplayReference<'a>>
    where
        S: BibliographyStyle<'a> + ?Sized,
    {
        style.bibliography(self, ordering.unwrap_or_else(|| style.ordering()))
    }

    /// Format a single entry for a bibliography with the given style.
    ///
    /// Returns the entry along with its styled reference if it is present in
    /// the database, otherwise the return value will be `None`.
    pub fn reference<S>(&self, style: &S, key: &str) -> Option<DisplayReference<'a>>
    where
        S: BibliographyStyle<'a>,
    {
        self.records.get(key).map(|record| style.reference(record))
    }
}

/// Provides a function to create in-text citations.
pub trait CitationStyle<'a> {
    /// Formats several [`Citation`]s using a [`Database`].
    ///
    /// This function is best used through [`Database::citation`] which performs
    /// additional processing on the database to support the operation.
    /// Implementors of this trait should expect corresponding records for the
    /// items in `parts` to be already present in the database.
    fn citation(
        &mut self,
        db: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation;

    /// Indicates the default brackets used by this style to wrap citations.
    fn brackets(&self) -> Brackets;

    /// Indicates whether citations should normally be wrapped in brackets in
    /// this style.
    fn wrapped(&self) -> bool;
}

/// Provides a function to create bibliographies.
pub trait BibliographyStyle<'a> {
    /// Formats all records in a [`Database`] as a bibliography. This function
    /// is best used through [`Database::bibliography`].
    fn bibliography(
        &self,
        db: &Database<'a>,
        ordering: BibliographyOrdering,
    ) -> Vec<DisplayReference<'a>>;

    /// Formats a single [`Record`] as a reference.
    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a>;

    /// Indicates the default ordering for this style.
    fn ordering(&self) -> BibliographyOrdering;
}

/// Describes the bracket preference of a citation style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Brackets {
    /// "(...)" Parentheses.
    Round,
    /// "[...]" Brackets.
    Square,
    /// No brackets.
    None,
}

impl Brackets {
    /// Get the according left bracket.
    pub fn left(&self) -> Option<char> {
        match self {
            Brackets::Round => Some('('),
            Brackets::Square => Some('['),
            Brackets::None => None,
        }
    }

    /// Get the according right bracket.
    pub fn right(&self) -> Option<char> {
        match self {
            Brackets::Round => Some(')'),
            Brackets::Square => Some(']'),
            Brackets::None => None,
        }
    }

    /// Wrap a [`DisplayString`] in brackets.
    pub fn wrap(&self, mut display: DisplayString) -> DisplayString {
        if let Some(left) = self.left() {
            display = DisplayString::from_string(left.to_string()) + display;
        }
        if let Some(right) = self.right() {
            display.push(right)
        }
        display
    }
}

/// Specify the ordering of bibliography items.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum BibliographyOrdering {
    /// Order items by the alphanumerical order of their prefixes.
    ByPrefix,
    /// Order items by the numeric order of their prefixes.
    ByNumericPrefix,
    /// Order items by their authors, then titles, and finally dates.
    ByAuthor,
    /// Do not reorder. Bibliography will be in insertion order of the database.
    ByInsertionOrder,
}

/// Citations that just consist of entry keys.
///
/// The entry with the key "lit" would yield the result **lit**.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct Keys {}

impl Keys {
    /// Create a new instance of this [`CitationStyle`].
    pub fn new() -> Self {
        Self {}
    }
}

impl<'a> CitationStyle<'a> for Keys {
    fn citation(
        &mut self,
        _: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut items = vec![];
        for atomic in parts {
            let mut res = DisplayString::new();
            res.start_format(Formatting::Bold);
            res += atomic.entry.key();
            res.commit_formats();

            if let Some(supplement) = atomic.supplement {
                res += " (";
                res += supplement;
                res.push(')');
            }

            items.push(res)
        }

        DisplayCitation::new(DisplayString::join(&items, ", "), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Square
    }

    fn wrapped(&self) -> bool {
        false
    }
}

/// Output IEEE-style numerical reference markers.
///
/// An example would be 1 or 3-7; 9.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Numerical {
    used_numbers: Vec<usize>,
    /// Order of the numeric references.
    pub ordering: NumericalOrdering,
}

impl Numerical {
    /// Creates a new instance.
    pub fn new() -> Self {
        Self::default()
    }
}

/// Specify the order in which numbers are assigned.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum NumericalOrdering {
    /// Numbers will be ordered by entries' authors, then titles, and finally
    /// dates.
    ///
    /// Note that this only works if all entries were pushed into the database
    /// before you cite anything.
    ByAuthor,
    /// Numbers will be in insertion order of the database.
    ByInsertionOrder,
}

impl Default for NumericalOrdering {
    fn default() -> Self {
        Self::ByInsertionOrder
    }
}

impl<'a> CitationStyle<'a> for Numerical {
    fn citation(
        &mut self,
        db: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut ids = vec![];
        if self.ordering == NumericalOrdering::ByAuthor {
            let mut sorted = db.records.clone().into_iter().collect::<Vec<_>>();
            sorted.sort_by(|(_, v), (_, v2)| author_title_ord(v.entry, v2.entry));

            for atomic in parts {
                let index = sorted
                    .iter()
                    .position(|(_, r)| r.entry == atomic.entry)
                    .expect("database record for entry has to exist at this point");

                let number = if let Some(prefix) = db
                    .records
                    .get(atomic.entry.key())
                    .unwrap()
                    .prefix
                    .as_ref()
                    .and_then(|p| p.parse().ok())
                {
                    prefix
                } else {
                    let mut counter = index;

                    if self.used_numbers.contains(&counter) {
                        counter = 0;
                    }

                    while self.used_numbers.contains(&counter) {
                        counter += 1;
                    }
                    self.used_numbers.push(counter);

                    db.records.get_mut(atomic.entry.key()).unwrap().prefix =
                        Some((counter + 1).to_string());
                    counter + 1
                };
                ids.push((number, atomic.supplement));
            }
        } else {
            for atomic in parts {
                let number = if let Some(prefix) = db
                    .records
                    .get(atomic.entry.key())
                    .unwrap()
                    .prefix
                    .as_ref()
                    .and_then(|p| p.parse().ok())
                {
                    prefix
                } else {
                    self.used_numbers.push(0);
                    let n = self.used_numbers.len();
                    db.records.get_mut(atomic.entry.key()).unwrap().prefix =
                        Some(n.to_string());
                    n
                };
                ids.push((number, atomic.supplement));
            }
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
                    res_elems.push(CiteElement::Range(number..number));
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

        DisplayCitation::new(re.into(), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Square
    }

    fn wrapped(&self) -> bool {
        true
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Formatting {
    /// **Bold print**
    Bold,
    /// _Italic print_
    Italic,
    /// Should link to the given URL.
    Link(String),
}

/// Will move a format range's indices by `o`.
fn offset_format_range(
    r: (std::ops::Range<usize>, Formatting),
    o: usize,
) -> (std::ops::Range<usize>, Formatting) {
    ((r.0.start + o)..(r.0.end + o), r.1)
}

/// A printable string with a list of formatting modifications.
#[derive(Clone, PartialEq, Eq)]
pub struct DisplayString {
    /// The string content.
    pub value: String,
    /// Information about formatted ranges.
    pub formatting: Vec<(std::ops::Range<usize>, Formatting)>,
    /// Formatting with still unknown end.
    pending: Option<(std::ops::RangeFrom<usize>, Formatting)>,
}

impl Default for DisplayString {
    fn default() -> Self {
        Self {
            value: String::new(),
            formatting: vec![],
            pending: None,
        }
    }
}

impl DisplayString {
    /// Constructs an empty display string.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a display string from a string.
    pub fn from_string(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            formatting: vec![],
            pending: None,
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

    /// Removes all of the formatting.
    pub fn clear_formatting(&mut self) {
        self.formatting.clear();
    }

    /// Wrap the display string with the citation style's
    /// [preferred brackets](CitationStyle::brackets) if
    /// the citation style [typically uses brackets](CitationStyle::wrapped).
    pub fn with_default_brackets<'a, S>(self, style: &S) -> Self
    where
        S: CitationStyle<'a> + ?Sized,
    {
        if style.wrapped() {
            style.brackets().wrap(self)
        } else {
            self
        }
    }

    /// Wrap the display string with the citation style's
    /// [preferred brackets](CitationStyle::brackets), disregarding whether
    /// the citation style [typically uses brackets](CitationStyle::wrapped).
    pub fn with_forced_brackets<'a, S>(self, style: &S) -> Self
    where
        S: CitationStyle<'a> + ?Sized,
    {
        style.brackets().wrap(self)
    }

    pub(crate) fn start_format(&mut self, f: Formatting) {
        debug_assert!(self.pending.is_none());
        self.pending = Some((self.len().., f));
    }

    pub(crate) fn commit_formats(&mut self) {
        if let Some((range, fmt)) = self.pending.take() {
            self.formatting.push((range.start..self.len(), fmt))
        }
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

    /// Joins a number of display strings with a separator in-between.
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

    /// Applies the formatting as ANSI / VT100 control sequences.
    pub fn ansi_vt100(&self) -> String {
        let mut start_end = vec![];

        for item in &self.formatting {
            let opt = &item.1;
            if matches!(opt, Formatting::Link(_)) {
                continue;
            }
            let min = item.0.start;
            let max = item.0.end;

            start_end.push((opt, min, false));
            start_end.push((opt, max, true));
        }

        start_end.sort_by(|a, b| a.1.cmp(&b.1).reverse());

        let mut res = String::new();
        let mut pointer = self.len();

        for (f, index, end) in &start_end {
            res = (&self.value[*index..pointer]).to_string() + &res;
            pointer = *index;

            let code = if *end {
                "0"
            } else {
                match f {
                    Formatting::Bold => "1",
                    Formatting::Italic => "3",
                    Formatting::Link(_) => unreachable!(),
                }
            };
            res = format!("\x1b[{}m", code) + &res;
        }
        res = (&self.value[0..pointer]).to_string() + &res;

        res
    }
}

impl Add<&str> for DisplayString {
    type Output = DisplayString;

    fn add(mut self, other: &str) -> DisplayString {
        self.value.push_str(other);
        self
    }
}

impl Add<Self> for DisplayString {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        let len = self.value.len();
        self.formatting
            .extend(other.formatting.into_iter().map(|e| offset_format_range(e, len)));
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

impl Display for DisplayString {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            f.pad(&self.value)
        } else {
            f.pad(&self.ansi_vt100())
        }
    }
}

impl Debug for DisplayString {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("DisplayString")
            .field("value", &self.value)
            .field("formatting", &self.formatting)
            .finish()
    }
}

impl PartialOrd for DisplayString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl Ord for DisplayString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl From<DisplayString> for String {
    fn from(display: DisplayString) -> Self {
        display.value
    }
}

impl From<String> for DisplayString {
    fn from(string: String) -> Self {
        Self::from_string(string)
    }
}

impl From<&str> for DisplayString {
    fn from(string: &str) -> Self {
        Self::from_string(string)
    }
}

fn push_comma_quote_aware(s: &mut String, comma: char, space: bool) {
    let cur_len = s.len();
    if cur_len > 3 && s.is_char_boundary(cur_len - 3) && &s[cur_len - 3..] == "”" {
        s.truncate(cur_len - 3);
        if !s.ends_with(comma) {
            s.push(comma);
            s.push('”');
        }
    } else if !s.is_empty() && !s.ends_with(comma) {
        s.push(comma);
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
    while select!(Chapter | Scene).matches(entry) && entry.title().is_none() {
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

fn omit_initial_articles(s: &str) -> String {
    let parts = s.split(' ').collect::<Vec<_>>();
    if parts.len() < 2 {
        return s.to_string();
    }

    if ["a", "an", "the"].contains(&parts.first().unwrap().to_lowercase().as_ref()) {
        (&parts[1..]).join(" ")
    } else {
        s.to_string()
    }
}

fn sorted_bibliography(
    mut items: Vec<(DisplayReference, Vec<Person>)>,
    ordering: BibliographyOrdering,
) -> Vec<DisplayReference> {
    match ordering {
        BibliographyOrdering::ByPrefix => {
            items.sort_by(|(a, _), (b, _)| a.prefix.cmp(&b.prefix));
        }
        BibliographyOrdering::ByNumericPrefix => items.sort_by_key(|(reference, _)| {
            reference.prefix.as_ref().and_then(|prefix| {
                prefix
                    .value
                    .chars()
                    .filter(|c| c.is_numeric())
                    .collect::<String>()
                    .parse::<i64>()
                    .ok()
            })
        }),
        BibliographyOrdering::ByAuthor => {
            items.sort_by(|(a_ref, a_auths), (b_ref, b_auths)| {
                author_title_ord_custom(
                    a_ref.entry,
                    b_ref.entry,
                    Some(a_auths),
                    Some(b_auths),
                )
            });
        }
        _ => {}
    }
    items.into_iter().map(|(a, _)| a).collect()
}

fn author_title_ord(item: &Entry, other: &Entry) -> Ordering {
    author_title_ord_custom(item, other, item.authors(), other.authors())
}

fn author_title_ord_custom(
    item: &Entry,
    other: &Entry,
    mut auth1: Option<&[Person]>,
    mut auth2: Option<&[Person]>,
) -> Ordering {
    if let Some(a) = auth1 {
        if a.is_empty() {
            auth1 = None;
        }
    }
    if let Some(a) = auth2 {
        if a.is_empty() {
            auth2 = None;
        }
    }

    match (auth1, auth2) {
        (Some(authors), Some(other_authors)) => authors.cmp(other_authors),
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => Ordering::Equal,
    }
    .then_with(|| {
        if let (Some(title), Some(other_title)) = (item.title(), other.title()) {
            omit_initial_articles(&title.canonical.value)
                .cmp(&omit_initial_articles(&other_title.canonical.value))
        } else {
            Ordering::Equal
        }
    })
    .then_with(|| {
        if let (Some(date), Some(other_date)) = (item.date_any(), other.date_any()) {
            date.year.cmp(&other_date.year).then(
                if let (Some(month), Some(other_month)) = (date.month, other_date.month) {
                    month.cmp(&other_month).then(
                        if let (Some(day), Some(other_day)) = (date.day, other_date.day) {
                            day.cmp(&other_day)
                        } else {
                            Ordering::Equal
                        },
                    )
                } else {
                    Ordering::Equal
                },
            )
        } else {
            Ordering::Equal
        }
    })
}

/// Citations following a simple alphanumerical style.
///
/// For example, the output could be Rass97 or MKG+21. \
/// Corresponds to LaTeX's `alphabetical` style.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Alphanumerical {
    /// Defines how many letters are allowed to describe an entry.
    pub letters: usize,
}

impl Default for Alphanumerical {
    fn default() -> Self {
        Self::new()
    }
}

impl Alphanumerical {
    /// Create a new instance of this [`CitationStyle`].
    pub fn new() -> Self {
        Self { letters: 3 }
    }

    fn creators(&self, entry: &Entry) -> String {
        let creators = chicago::get_creators(entry).0;

        match creators.len() {
            0 => {
                let pseudo_creator = if let Some(org) = entry.organization() {
                    org.into()
                } else if let Some(title) = delegate_titled_entry(entry).title() {
                    title.canonical.value.clone()
                } else {
                    entry.key().chars().filter(|c| c.is_alphabetic()).collect::<String>()
                };

                pseudo_creator.graphemes(true).take(self.letters).collect()
            }
            1 => creators[0].name.graphemes(true).take(self.letters).collect(),
            2 | 3 => creators
                .iter()
                .filter_map(|person| person.name.graphemes(true).next())
                .collect(),
            _ => creators[0]
                .name
                .graphemes(true)
                .take(self.letters)
                .chain(std::iter::once("+"))
                .collect(),
        }
    }

    fn year(entry: &Entry) -> Option<String> {
        let year = entry
            .date_any()
            .or_else(|| entry.url_any().and_then(|u| u.visit_date.as_ref()))
            .map(|date| {
                let mut year = i32::abs(date.year % 100);
                if date.year <= 0 {
                    year += 1;
                }
                year
            });

        year.map(|y| {
            let mut num = String::with_capacity(2);
            write!(&mut num, "{:02}", y).unwrap();
            num
        })
    }
}

impl<'a> CitationStyle<'a> for Alphanumerical {
    fn citation(
        &mut self,
        db: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut items = vec![];
        for atomic in parts {
            let mut entry = atomic.entry;

            entry = delegate_titled_entry(entry);

            let creators = self.creators(entry);
            let mut res = creators.clone();
            let year_opt = Self::year(entry);
            if let Some(year) = year_opt.as_ref() {
                res += year;
            }

            if db.records.get(atomic.entry.key()).unwrap().disambiguation.is_some() {
                if let Some(num) =
                    db.records.get(atomic.entry.key()).unwrap().disambiguation
                {
                    res.push(alph_designator(num));
                }
            } else {
                let similar: Vec<_> = db
                    .records()
                    .filter(|r| {
                        (&self.creators(r.entry), Self::year(r.entry).as_ref())
                            == (&creators, year_opt.as_ref())
                    })
                    .collect();
                if similar.len() > 1 {
                    let disambiguation =
                        similar.iter().position(|&r| r.entry == entry).unwrap();
                    db.records.get_mut(atomic.entry.key()).unwrap().disambiguation =
                        Some(disambiguation);
                    res.push(alph_designator(disambiguation));
                }
            }

            let record = db.records.get_mut(atomic.entry.key()).unwrap();
            record.prefix = Some(res.clone());

            if let Some(supplement) = atomic.supplement {
                res += ", ";
                res += supplement;
            }

            items.push(res);
        }

        DisplayCitation::new(items.join("; ").into(), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Square
    }

    fn wrapped(&self) -> bool {
        true
    }
}

/// Citations following a Chicago-like author-title format.
///
/// Results could look like this: Prokopov, “It Is Fast or It Is Wrong”.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct AuthorTitle {
    /// This citation style uses code from Chicago, therefore the settings are
    /// contained within this struct.
    pub config: ChicagoConfig,
}

impl AuthorTitle {
    /// Create a new instance of this [`CitationStyle`].
    pub fn new() -> Self {
        Self::default()
    }

    fn creator_list(&self, entry: &Entry) -> String {
        let creators = chicago::get_creators(entry).0;
        match creators.len() {
            0 => {
                if let Some(org) = entry.organization() {
                    org.into()
                } else {
                    String::new()
                }
            }
            _ => chicago::and_list(
                creators.into_iter().map(|person| {
                    if let Some(prefix) = person.prefix {
                        format!("{} {}", prefix, person.name)
                    } else {
                        person.name.clone()
                    }
                }),
                false,
                self.config.et_al_limit,
            ),
        }
    }
}

impl<'a> CitationStyle<'a> for AuthorTitle {
    fn citation(
        &mut self,
        _: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut items = vec![];
        for atomic in parts {
            let entry = delegate_titled_entry(atomic.entry);

            let mut res: DisplayString = self.creator_list(entry).into();
            let title = entry
                .title()
                .map(|_| chicago::get_chunk_title(entry, false, true, &self.config));

            if let Some(title) = title.clone() {
                push_comma_quote_aware(&mut res.value, ',', true);
                res += title;
            }

            if let Some(lang) = entry.language() {
                push_comma_quote_aware(&mut res.value, ',', true);
                res += "(";
                res += Language::from_639_1(lang.language.as_str()).unwrap().to_name();
                res += ")";
            }

            if let Some(supplement) = atomic.supplement {
                push_comma_quote_aware(&mut res.value, ',', true);
                res += supplement;
            }

            items.push(res);
        }

        DisplayCitation::new(DisplayString::join(&items, "; "), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Round
    }

    fn wrapped(&self) -> bool {
        false
    }
}
