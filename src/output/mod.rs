use super::types::Person;
use super::{Entry, FieldTypes};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::convert::Into;
use std::ops::{Add, AddAssign};
use thiserror::Error;

pub mod apa;
pub mod ieee;

/// Will be raised if a user-specified citation is not possible with the given
/// database.
#[derive(Debug, Error)]
pub enum CitationError {
    #[error("key {0} could not be fount in the citation database")]
    KeyNotFound(String),
    #[error("the citation contains no keys")]
    EmptyCitation,
    #[error("key {0} does not contain the author field required for this style")]
    NoAuthorField(String),
    #[error("key {0} does not contain the date field required for this style")]
    NoYearField(String),
}

/// Structs that implement this can be used to generate bibliography references
/// for sources.
pub trait BibliographyGenerator {
    /// Get a string with optional formatting that describes `Entry` in
    /// accordance with the implementing struct's style.
    fn get_reference(&self, entry: &Entry) -> DisplayString;
}

/// Represents a citation of one or more database entries.
pub struct Citation<'s> {
    /// Cited entry keys.
    keys: Vec<&'s str>,
    /// Supplements for each entry key such as page or chapter number.
    #[allow(dead_code)]
    supplements: Vec<Option<&'s str>>,
}

/// Structs implementing this trait can generate the appropriate reference
/// markers for a list of `Citation` structs.
pub trait ReferenceGenerator {
    /// Add a new citation struct. Has to be called in the desired order.
    fn push_citation(&mut self, citation: Citation) -> Result<(), CitationError>;
    /// Dump the citation references for each pushed citation in push order.
    fn dump(&self) -> Result<Vec<String>, CitationError>;
    /// List the cited keys.
    fn used_keys(&self) -> Vec<String>;
}

/// Structs implementing this trait can generate the appropriate reference
/// markers for a single `Citation` struct. They do not have to see subsequent
/// citations to determine the marker value.
pub trait ImmediateReferenceGenerator {
    /// Get a reference for the passed citation struct.
    fn get_reference(&mut self, citation: Citation) -> Result<String, CitationError>;
    /// List the cited keys.
    fn used_keys(&self) -> Vec<String>;
}

/// Saves strings and retreives them. Implement this if you are implementing the
/// `ImmediateReferenceGenerator` and you will receive a blanket implementation
/// for the more generic `ReferenceGenerator`.
pub trait StringMemory {
    fn save_string(&mut self, val: String);
    fn dump_memory(&self) -> Vec<String>;
}

impl<T: ImmediateReferenceGenerator + StringMemory> ReferenceGenerator for T {
    fn push_citation(&mut self, citation: Citation) -> Result<(), CitationError> {
        let cit = self.get_reference(citation)?;
        self.save_string(cit);
        Ok(())
    }

    fn dump(&self) -> Result<Vec<String>, CitationError> {
        Ok(self.dump_memory())
    }

    fn used_keys(&self) -> Vec<String> {
        self.used_keys()
    }
}

/// Checks if the keys are in the database and returns them as reference
/// markers, since they are already unique.
pub struct KeyReferenceGenerator<'s> {
    memory: Vec<String>,
    entries: &'s HashMap<String, Entry>,
    cited: HashSet<String>,
}

impl ImmediateReferenceGenerator for KeyReferenceGenerator<'_> {
    fn get_reference(&mut self, citation: Citation) -> Result<String, CitationError> {
        for &key in &citation.keys {
            if !self.entries.contains_key(key) {
                return Err(CitationError::KeyNotFound(key.to_string()));
            }

            self.cited.insert(key.to_string());
        }

        let mut res =
            citation.keys.first().ok_or(CitationError::EmptyCitation)?.to_string();

        for key in &citation.keys[1 ..] {
            res += ", ";
            res += key;
        }

        Ok(res)
    }

    fn used_keys(&self) -> Vec<String> {
        self.cited.iter().cloned().collect()
    }
}

impl StringMemory for KeyReferenceGenerator<'_> {
    fn save_string(&mut self, val: String) {
        self.memory.push(val);
    }

    fn dump_memory(&self) -> Vec<String> {
        self.memory.clone()
    }
}

/// Output IEEE-style numerical reference markers.
pub struct NumericalReferenceGenerator<'s> {
    memory: Vec<String>,
    entries: &'s HashMap<String, Entry>,
    unique_works_cited: u32,
    num_map: HashMap<String, u32>,
}

impl ImmediateReferenceGenerator for NumericalReferenceGenerator<'_> {
    fn get_reference(&mut self, citation: Citation) -> Result<String, CitationError> {
        let mut ids = vec![];
        for &key in &citation.keys {
            if !self.entries.contains_key(key) {
                return Err(CitationError::KeyNotFound(key.to_string()));
            }

            let id = if let Some(&val) = self.num_map.get(key) {
                val
            } else {
                self.unique_works_cited += 1;
                self.num_map.insert(key.to_string(), self.unique_works_cited);
                self.unique_works_cited
            };

            ids.push(id);
        }

        Ok(ids
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", "))
    }

    fn used_keys(&self) -> Vec<String> {
        self.num_map.keys().cloned().collect()
    }
}

impl StringMemory for NumericalReferenceGenerator<'_> {
    fn save_string(&mut self, val: String) {
        self.memory.push(val);
    }

    fn dump_memory(&self) -> Vec<String> {
        self.memory.clone()
    }
}

/// Get Harvard-style Author-Year citations like "Howard, 1997; Jean et al. 2001"
pub struct AuthorYearReferenceGenerator<'s> {
    cited_keys: Vec<Vec<String>>,
    entries: &'s HashMap<String, Entry>,
}

fn author_list_names(authors: &Vec<Person>) -> String {
    if authors.len() == 0 {
        return String::new();
    }

    if authors.len() > 3 {
        format!("{} et al.", authors.first().unwrap().name)
    } else {
        let mut res = authors.first().unwrap().name.clone();
        for author in &authors[1 ..] {
            res += ", ";
            res += &author.name;
        }
        res
    }
}

impl ReferenceGenerator for AuthorYearReferenceGenerator<'_> {
    fn push_citation(&mut self, citation: Citation) -> Result<(), CitationError> {
        for &key in &citation.keys {
            if let Some(e) = self.entries.get(key) {
                if !e.content.contains_key("author") {
                    Err(CitationError::NoAuthorField(key.to_string()))?
                } else if !e.content.contains_key("date") {
                    Err(CitationError::NoYearField(key.to_string()))?
                }
            } else {
                Err(CitationError::KeyNotFound(key.to_string()))?
            }
        }

        self.cited_keys
            .push(citation.keys.into_iter().map(|s| s.to_string()).collect());

        Ok(())
    }

    fn dump(&self) -> Result<Vec<String>, CitationError> {
        let unique_ordered: BTreeSet<String> = self.used_keys().into_iter().collect();

        let mut triple: HashMap<&str, (String, i32, u32)> = HashMap::new();
        let mut count_map: HashMap<(String, i32), u32> = HashMap::new();

        for key in &unique_ordered {
            if triple.contains_key(&key.as_ref()) {
                continue;
            }

            let e = self.entries.get(key).unwrap();
            let name = e
                .content
                .get("author")
                .map(|a| match a {
                    FieldTypes::Persons(ps) => author_list_names(ps),
                    _ => panic!("author field should have person type"),
                })
                .unwrap();
            let year = e
                .content
                .get("date")
                .map(|a| match a {
                    FieldTypes::Date(d) => d.year,
                    _ => panic!("date field should have date type"),
                })
                .unwrap();

            let val = if let Some(val) = count_map.get(&(name.clone(), year)) {
                val + 1
            } else {
                0
            };

            count_map.insert((name.clone(), year), val);
            triple.insert(key.as_ref(), (name, year, val));
        }

        let mut coll = vec![];
        for citation in &self.cited_keys {
            let mut res = String::new();
            let mut first = true;
            for key in citation {
                if !first {
                    res += "; ";
                } else {
                    first = false;
                }

                let (names, year, count) = triple.get(&key.as_ref()).unwrap().clone();
                let is_alph =
                    count > 0 || *count_map.get(&(names.clone(), year)).unwrap() > 0;
                let letter = if is_alph {
                    ((b'a' + (count as u8 % 26)) as char).to_string()
                } else {
                    String::new()
                };
                res += &format!("{}, {:04}{}", names, year, letter);
            }
            coll.push(res);
        }

        Ok(coll)
    }

    fn used_keys(&self) -> Vec<String> {
        self.cited_keys.iter().flat_map(|s| s.iter()).cloned().collect()
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
        names.push(author.get_name_first());
    }

    names
}

fn name_list_straight(persons: &[Person]) -> Vec<String> {
    let mut names = vec![];

    for author in persons.iter() {
        names.push(author.get_given_name_initials_first());
    }

    names
}

/// Formatting modifiers for strings.
/// Each variant holds a tuple with a start and
/// an end index.
#[derive(Clone, Debug)]
pub enum FormatVariants {
    Bold((usize, usize)),
    Italic((usize, usize)),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum FormatVariantOptions {
    Bold,
    Italic,
}

impl FormatVariants {
    pub fn get_min(&self) -> usize {
        match self {
            Self::Bold((a, _)) => a,
            Self::Italic((a, _)) => a,
        }
        .clone()
    }
    pub fn get_max(&self) -> usize {
        match self {
            Self::Bold((_, b)) => b,
            Self::Italic((_, b)) => b,
        }
        .clone()
    }
    pub fn offset(self, o: i32) -> Self {
        match self {
            Self::Bold((a, b)) => {
                Self::Bold(((a as i32 + o) as usize, (b as i32 + o) as usize))
            }
            Self::Italic((a, b)) => {
                Self::Italic(((a as i32 + o) as usize, (b as i32 + o) as usize))
            }
        }
        .clone()
    }
    pub fn get_range(&self) -> std::ops::Range<usize> {
        self.get_min() .. self.get_max()
    }

    pub(crate) fn from_option(
        opt: FormatVariantOptions,
        start: usize,
        end: usize,
    ) -> Self {
        let tuple = (start, end);

        match opt {
            FormatVariantOptions::Bold => Self::Bold(tuple),
            FormatVariantOptions::Italic => Self::Italic(tuple),
        }
    }

    pub(crate) fn get_option(&self) -> FormatVariantOptions {
        match self {
            Self::Bold(_) => FormatVariantOptions::Bold,
            Self::Italic(_) => FormatVariantOptions::Italic,
        }
    }
}

/// A printable string with a list of formatting modifications
#[derive(Clone, Debug)]
pub struct DisplayString {
    pub value: String,
    pub formatting: Vec<FormatVariants>,

    pending_formatting: Vec<(FormatVariantOptions, usize)>,
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
                .map(|e| e.offset(self.value.len() as i32))
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
                .map(|e| e.offset(self.value.len() as i32))
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

impl DisplayString {
    pub fn new() -> Self {
        Self {
            value: String::new(),
            formatting: vec![],
            pending_formatting: vec![],
        }
    }

    pub fn from_str(s: &str) -> Self {
        Self {
            value: s.to_string(),
            formatting: vec![],
            pending_formatting: vec![],
        }
    }

    pub fn from_string(s: String) -> Self {
        Self {
            value: s,
            formatting: vec![],
            pending_formatting: vec![],
        }
    }

    pub fn len(&self) -> usize {
        self.value.len()
    }

    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    pub fn last(&self) -> Option<char> {
        self.value.chars().last()
    }

    pub fn push(&mut self, ch: char) {
        self.value.push(ch);
    }

    pub(crate) fn start_format(&mut self, f: FormatVariantOptions) {
        debug_assert!(!self.pending_formatting.iter().any(|e| e.0 == f));
        self.pending_formatting.push((f, self.len()));
    }

    pub(crate) fn commit_formats(&mut self) {
        for (f, start) in &self.pending_formatting {
            self.formatting.push(FormatVariants::from_option(
                f.clone(),
                *start,
                self.len(),
            ))
        }

        self.pending_formatting.clear();
    }

    pub(crate) fn add_if_ok<S: Into<String>, T>(
        &mut self,
        item: Result<S, T>,
        prefix: Option<&str>,
        postfix: Option<&str>,
    ) {
        if let Ok(item) = item {
            if let Some(prefix) = prefix {
                *self += prefix;
            }
            *self += &item.into();
            if let Some(postfix) = postfix {
                *self += postfix;
            }
        }
    }

    pub fn print_ansi_vt100(&self) -> String {
        let mut start_end = vec![];

        for item in &self.formatting {
            let opt = item.get_option();
            let min = item.get_min();
            let max = item.get_max();

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
                    FormatVariantOptions::Bold => "1",
                    FormatVariantOptions::Italic => "3",
                }
            };
            res = format!("\x1b[{}m", code) + &res;
        }
        res = (&self.value[0 .. pointer]).to_string() + &res;

        res
    }
}
