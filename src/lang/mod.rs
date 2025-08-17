//! Language-dependant string transformations.

pub(crate) mod en;
pub(crate) mod name;

use std::{fmt::Write, mem};

use crate::types::{FoldableKind, FoldableStringChunk};

/// Rules for the title case transformation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct TitleCase {
    /// Always capitalize after a full stop, question or exclamation mark, and
    /// colon even if the punctuation is followed by a word on the
    /// capitalization blacklist.
    pub always_capitalize_after_punctuation: bool,
    /// Always capitalize the last word of the title, even if it is on the
    /// capitalization blacklist.
    pub always_capitalize_last_word: bool,
    /// Always capitalize a word matching or exceeding a certain length, even
    /// if it is on the capitalization blacklist.
    pub always_capitalize_min_len: Option<usize>,
    /// Treat the hyphen as a word separator, i. e. capitalize in hyphenated
    /// expressions.
    pub hyphen_word_separator: bool,
    /// Do not touch all-uppercase words like "USA" if the title contains
    /// lower-case characters at all.
    pub keep_all_uppercase_words: bool,
    /// Enable usage of the capitalization blacklist.
    pub use_exception_dictionary: bool,
    /// Discard whitespace at the start of the title.
    pub trim_start: bool,
    /// Discard whitespace at the end of the title.
    pub trim_end: bool,
}

impl Default for TitleCase {
    fn default() -> Self {
        Self {
            always_capitalize_after_punctuation: true,
            always_capitalize_last_word: false,
            always_capitalize_min_len: None,
            hyphen_word_separator: true,
            keep_all_uppercase_words: true,
            use_exception_dictionary: true,
            trim_start: true,
            trim_end: true,
        }
    }
}

impl TitleCase {
    /// Construct TitleCaseProperties with the default values.
    pub fn new() -> Self {
        Default::default()
    }
}

/// Rules for the sentence case transformation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct SentenceCase {
    /// Capitalize words that contain caps
    /// in a non-start position (e. g. `fahrCard`).
    pub capitalize_words_with_caps_inside: bool,
    /// Enables the output of characters that directly follow a dot in the case
    /// that they appear in the source argument (e. g. `fig. 4.C`).
    pub do_not_format_after_dot: bool,
    /// Do not touch all-uppercase words like "ISS" if the title contains
    /// lower-case characters at all.
    pub keep_all_uppercase_words: bool,
    /// Enable usage of the capitalization whitelist.
    pub use_exception_dictionary: bool,
    /// Discard whitespace at the start of the title.
    pub trim_start: bool,
    /// Discard whitespace at the end of the title.
    pub trim_end: bool,
}

impl Default for SentenceCase {
    fn default() -> Self {
        Self {
            capitalize_words_with_caps_inside: true,
            do_not_format_after_dot: true,
            keep_all_uppercase_words: true,
            use_exception_dictionary: true,
            trim_start: true,
            trim_end: true,
        }
    }
}

impl SentenceCase {
    /// Construct a new `SentenceCase` with the default values.
    pub fn new() -> Self {
        Default::default()
    }
}

/// Which case to transform to
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum Case {
    /// Capitalize all words except for some words using customary English rules.
    Title(TitleCase),
    /// Capitalize at the start of sentences as well as proper nouns.
    Sentence(SentenceCase),
    /// CAPITALIZE EVERYTHING.
    Uppercase,
    /// lowercase everything.
    Lowercase,
    /// Do not apply a case transformation.
    #[default]
    NoTransform,
    /// Capitalize the first letter of the first word if it is lowercase.
    FirstUpper,
    /// Capitalize the first letter of each word.
    AllUpper,
}

impl Case {
    /// Transform a string to a case.
    ///
    /// If you need a reconfigurable buffer, use [`CaseFolder`].
    pub fn transform(self, s: &str) -> String {
        let mut buf = CaseFolder::with_config(self);
        buf.push_str(s);
        buf.finish()
    }
}

impl From<TitleCase> for Case {
    fn from(props: TitleCase) -> Self {
        Self::Title(props)
    }
}

impl From<SentenceCase> for Case {
    fn from(props: SentenceCase) -> Self {
        Self::Sentence(props)
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
enum CharClass {
    #[default]
    NewSentence,
    MaybeNewSentence,
    NewSubclause,
    MaybeSubclause,
    NewWord,
    InWord,
}

impl CharClass {
    fn step(self, c: char, hyphen_separates: bool) -> Self {
        match c {
            '.' | '?' | '!' | '…' | '‽' | '。' | ':' => {
                if self.is_in_word() {
                    Self::MaybeNewSentence
                } else {
                    self
                }
            }
            '(' | '[' | '{' => Self::NewSubclause,
            ')' | ']' | '}' => self,
            '"' | '”' | '“' | '»' | '›' | '«' | '‹' | '‘' | '\'' => self,
            ',' | ';' => {
                if self.is_in_word() {
                    Self::MaybeSubclause
                } else {
                    self
                }
            }
            _ if c.is_whitespace()
                || (hyphen_separates && matches!(c, '-' | '–' | '—')) =>
            {
                match self {
                    Self::MaybeNewSentence => Self::NewSentence,
                    Self::MaybeSubclause => Self::NewSubclause,
                    Self::InWord => Self::NewWord,
                    _ => self,
                }
            }
            _ => Self::InWord,
        }
    }

    fn is_new_sentence(&self) -> bool {
        matches!(self, Self::NewSentence)
    }

    fn is_new_subclause(&self) -> bool {
        matches!(self, Self::NewSubclause | Self::NewSentence)
    }

    fn is_new_word(&self) -> bool {
        matches!(self, Self::NewWord | Self::NewSubclause | Self::NewSentence)
    }

    fn is_in_word(&self) -> bool {
        matches!(self, Self::InWord | Self::MaybeSubclause | Self::MaybeNewSentence)
    }
}

/// Buffer that adjusts word case on the fly.
#[derive(Debug, Clone)]
pub struct CaseFolder {
    case: Case,
    /// Only true if the only characters after a configuration change were
    /// whitespace.
    pristine: bool,
    char_class: CharClass,
    buf: String,
    /// Index of finished at which the last configuration change happened.
    last_reconfig: usize,
    /// Index of the last word in the buffer.
    last_word: Option<WordData>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum WordCase {
    Lowercase,
    HasNonStartUpper,
    AllUpper,
}

impl WordCase {
    fn feed(self, is_upper: bool, second: bool) -> Self {
        match (self, is_upper, second) {
            (Self::Lowercase, true, _) => Self::HasNonStartUpper,
            (Self::AllUpper, false, false) => Self::HasNonStartUpper,
            (Self::AllUpper, false, true) => Self::Lowercase,
            _ => self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct WordData {
    start: usize,
    end: usize,
    case: WordCase,
    follows_punctuation: bool,
    start_was_upper: bool,
}

impl WordData {
    fn new(
        c: char,
        buf_before: usize,
        buf_now: usize,
        prev_class: CharClass,
        char_class: CharClass,
    ) -> Option<Self> {
        let start_was_upper = c.is_uppercase();
        char_class.is_in_word().then(|| Self {
            start: buf_before,
            end: buf_now,
            case: if start_was_upper { WordCase::AllUpper } else { WordCase::Lowercase },
            follows_punctuation: prev_class.is_new_subclause(),
            start_was_upper,
        })
    }

    fn feed(
        self,
        c: char,
        buf_before: usize,
        buf_now: usize,
        prev_class: CharClass,
        char_class: CharClass,
    ) -> Option<Self> {
        if char_class.is_new_word() {
            return None;
        }

        if !prev_class.is_in_word() && char_class == CharClass::InWord {
            let start_was_upper = c.is_uppercase();
            return Some(Self {
                start: buf_before,
                end: buf_now,
                case: if start_was_upper {
                    WordCase::AllUpper
                } else {
                    WordCase::Lowercase
                },
                follows_punctuation: prev_class.is_new_subclause(),
                start_was_upper,
            });
        }

        let second = self.end - self.start == 1;
        let case = if c.is_ascii_punctuation() {
            self.case
        } else {
            self.case.feed(c.is_uppercase(), second)
        };
        Some(Self {
            start: self.start,
            end: buf_now,
            case,
            follows_punctuation: self.follows_punctuation,
            start_was_upper: self.start_was_upper,
        })
    }

    fn is_all_upper(self) -> bool {
        matches!(self.case, WordCase::AllUpper) && self.end - self.start > 1
    }

    fn is_continuing_word(self, other: Option<Self>) -> bool {
        match other {
            Some(other) => self.start == other.start,
            None => false,
        }
    }
}

#[derive(Debug)]
enum WordVerdict {
    AllUpper,
    Capitalize,
    Lowercase,
    Keep,
}

impl Default for CaseFolder {
    fn default() -> Self {
        Self {
            case: Case::default(),
            pristine: true,
            char_class: CharClass::default(),
            buf: String::new(),
            last_reconfig: 0,
            last_word: None,
        }
    }
}

impl CaseFolder {
    /// Create a new case folder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a case folder with a case configuration.
    pub fn with_config(case: Case) -> Self {
        Self { case, ..Default::default() }
    }

    /// Add a string to the buffer.
    pub fn push_str(&mut self, s: &str) {
        match self.case {
            Case::NoTransform => {
                self.buf.push_str(s);
                self.char_class = CharClass::default();
            }
            Case::Uppercase => {
                self.buf.extend(s.chars().flat_map(char::to_uppercase));
                self.char_class = CharClass::default();
            }
            Case::Lowercase => {
                self.buf.extend(s.chars().flat_map(char::to_lowercase));
                self.char_class = CharClass::default();
            }
            _ => {
                for c in s.chars() {
                    self.push(c);
                }
            }
        }

        self.pristine = false;
    }

    /// Prevent whitespace trimming before and after the current position.
    pub fn prevent_trimming(&mut self) {
        self.last_reconfig = self.buf.len();
        self.pristine = false;
    }

    /// Add a string chunk to the buffer.
    pub fn push_verbatim(&mut self, value: &str) {
        let conf = mem::replace(&mut self.case, Case::NoTransform);
        self.last_reconfig = self.buf.len();
        self.push_str(value);
        self.last_reconfig = self.buf.len();
        self.case = conf;
        self.char_class = CharClass::InWord;
    }

    /// Add a string chunk to the buffer.
    pub fn push_chunk(&mut self, chunk: &FoldableStringChunk) {
        match chunk.kind {
            FoldableKind::Verbatim => {
                self.push_verbatim(&chunk.value);
            }
            FoldableKind::Normal => self.push_str(&chunk.value),
        }
    }

    /// Add a character to the buffer.
    pub fn push(&mut self, c: char) {
        let prev_class = self.char_class;
        let hyphen_separates = match self.case {
            Case::Title(props) => props.hyphen_word_separator,
            _ => false,
        };
        self.char_class = prev_class.step(c, hyphen_separates);

        let old_buf_len = self.buf.len();

        match self.case {
            Case::Title(config)
                if config.trim_start && self.pristine && c.is_whitespace() => {}
            Case::Title(_) => {
                if prev_class.is_new_word() {
                    self.buf.extend(c.to_uppercase());
                } else {
                    self.buf.extend(c.to_lowercase());
                }
            }
            Case::Sentence(config)
                if config.trim_start && self.pristine && c.is_whitespace() => {}
            Case::Sentence(config) => {
                if prev_class.is_new_sentence() {
                    self.buf.extend(c.to_uppercase());
                } else if config.do_not_format_after_dot
                    && matches!(
                        prev_class,
                        CharClass::MaybeNewSentence | CharClass::MaybeSubclause
                    )
                {
                    self.buf.push(c);
                } else {
                    self.buf.extend(c.to_lowercase());
                }
            }
            Case::FirstUpper => {
                if self.pristine && c.is_lowercase() {
                    self.buf.extend(c.to_uppercase());
                } else {
                    self.buf.push(c);
                }
            }
            Case::AllUpper => {
                if prev_class.is_new_word() && c.is_lowercase() {
                    self.buf.extend(c.to_uppercase());
                } else {
                    self.buf.push(c);
                }
            }
            Case::Uppercase => self.buf.extend(c.to_uppercase()),
            Case::Lowercase => self.buf.extend(c.to_lowercase()),
            Case::NoTransform => self.buf.push(c),
        }

        self.last_word = match self.last_word {
            Some(data) => {
                let new_data = data.feed(
                    c,
                    old_buf_len,
                    self.buf.len(),
                    prev_class,
                    self.char_class,
                );

                if !data.is_continuing_word(new_data) {
                    self.process_word()
                }

                new_data
            }
            None => {
                WordData::new(c, old_buf_len, self.buf.len(), prev_class, self.char_class)
            }
        };

        self.pristine = self.pristine && c.is_whitespace();
    }

    /// Find the word with only alphabetic characters that starts at `self.last_word.start`.
    fn find_word(&self) -> Option<&str> {
        let data = self.last_word?;
        let mut alphabetic_end = data.start;
        while alphabetic_end < data.end
            && self.buf[alphabetic_end..]
                .chars()
                .next()
                .map(char::is_alphanumeric)
                .unwrap_or_default()
        {
            alphabetic_end += 1;
            while !self.buf.is_char_boundary(alphabetic_end) {
                alphabetic_end += 1;
            }
        }

        Some(&self.buf[data.start..alphabetic_end])
    }

    fn process_word(&mut self) {
        let mut verdict = WordVerdict::Keep;
        let data = if let Some(data) = self.last_word {
            data
        } else {
            return;
        };

        match self.case {
            Case::Title(config) => {
                if config.keep_all_uppercase_words && data.is_all_upper() {
                    verdict = WordVerdict::AllUpper;
                } else if config.use_exception_dictionary {
                    // Check if the word should be decapitalized.
                    let mut lookup = true;

                    // Do not lowercase if the word follows punctuation.
                    lookup &= !config.always_capitalize_after_punctuation
                        || !data.follows_punctuation;

                    if let Some(min_len) = config.always_capitalize_min_len {
                        // Do not lowercase if word is too long.
                        lookup &= data.end - data.start < min_len;
                    }

                    let term = self.find_word().unwrap_or_default();

                    if lookup {
                        verdict = if en::NEVER_CAPITALIZE.binary_search(&term).is_ok() {
                            WordVerdict::Lowercase
                        } else {
                            WordVerdict::Keep
                        };
                    }
                }
            }
            Case::Sentence(config) => {
                if config.capitalize_words_with_caps_inside
                    && data.case == WordCase::HasNonStartUpper
                {
                    verdict = WordVerdict::Capitalize;
                } else if config.keep_all_uppercase_words && data.is_all_upper() {
                    verdict = WordVerdict::AllUpper;
                } else if config.use_exception_dictionary {
                    let term = self.find_word().unwrap_or_default();
                    verdict = if en::ALWAYS_CAPITALIZE.binary_search(&term).is_ok() {
                        WordVerdict::Capitalize
                    } else {
                        WordVerdict::Keep
                    };
                }
            }
            _ => {}
        }

        match verdict {
            WordVerdict::AllUpper => {
                map_chars(&mut self.buf, data.start..data.end, |c| {
                    if c.is_lowercase() { Some(c.to_uppercase()) } else { None }
                });
            }
            WordVerdict::Capitalize => capitalize_char(&mut self.buf, data.start),
            WordVerdict::Lowercase => match &self.buf[data.start..].char_indices().next()
            {
                Some((_, c)) if c.is_uppercase() => {
                    map_chars(
                        &mut self.buf,
                        data.start..data.start + c.len_utf8(),
                        |c| Some(c.to_lowercase()),
                    );
                }
                _ => {}
            },
            WordVerdict::Keep => {}
        }
    }

    fn may_trim_end(&mut self) {
        let trim = match self.case {
            Case::Title(config) => config.trim_end,
            Case::Sentence(config) => config.trim_end,
            _ => false,
        };

        // Trim all whitespace at the end of the buffer until self.last_reconfig.
        while trim
            && self.buf.len() > self.last_reconfig
            && self.buf.ends_with(char::is_whitespace)
        {
            self.buf.pop();
        }
        match self.case {
            Case::Title(config) if config.always_capitalize_last_word => {
                let start = match self.last_word {
                    Some(data) => Some(data.start),
                    None => self
                        .buf
                        .rfind(char::is_whitespace)
                        .map(|i| i + self.buf[i..].chars().next().unwrap().len_utf8()),
                };

                let Some(start) = start else { return };
                if start < self.last_reconfig || start >= self.buf.len() {
                    return;
                }

                capitalize_char(&mut self.buf, start);
            }
            _ => {}
        }
    }

    /// Retrieve the configuration of the CaseFolder.
    pub fn case(&self) -> Case {
        self.case
    }

    /// Change the configuration of the CaseFolder.
    pub fn reconfigure(&mut self, case: Case) {
        if self.case == case {
            return;
        }

        self.may_trim_end();
        self.last_reconfig = self.buf.len();
        self.pristine = true;
        self.case = case;
    }

    /// Yield the transformed string.
    pub fn finish(mut self) -> String {
        self.process_word();
        self.may_trim_end();
        self.buf
    }

    /// Whether the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    /// Return the length of the buffer.
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    /// Whether the buffer contains only whitespace.
    pub fn has_content(&self) -> bool {
        !self.buf.chars().all(char::is_whitespace)
    }

    /// Yield the buffer as a mutable string. Must call [`Self::mark_changed`]
    /// if the length of the string is changed.
    pub(crate) fn as_string_mut(&mut self) -> &mut String {
        &mut self.buf
    }

    /// Check if the buffer ends with a character.
    pub fn ends_with(&self, pattern: char) -> bool {
        self.buf.ends_with(pattern)
    }

    /// Notify the struct that an outside manipulation to the underlying buffer
    /// occurred.
    pub fn mark_changed(&mut self) {
        self.last_reconfig = self.buf.len();
        self.last_word = None;
    }
}

impl Write for CaseFolder {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.push(c);
        Ok(())
    }
}

/// Maps the characters inside of a string in place.
/// The mapping function returns an iterator over the characters that should
/// replace the current character. It can also return `None` to indicate that
/// the current character should not be replaced.
fn map_chars<F, I>(buf: &mut String, range: std::ops::Range<usize>, mut f: F)
where
    I: Iterator<Item = char>,
    F: FnMut(char) -> Option<I>,
{
    // Try to capitalize in-place.
    let mut i = range.start;
    let mut orig_chars = range.start;
    while let Some(char) = buf[i..].chars().next() {
        if orig_chars >= range.end {
            break;
        }

        orig_chars += char.len_utf8();

        match f(char) {
            None => {
                i += char.len_utf8();
                continue;
            }
            Some(iter) => {
                let mut end = i + char.len_utf8();
                for c in iter {
                    buf.replace_range(i..end, c.encode_utf8(&mut [0; 4]));
                    i += c.len_utf8();
                    end = i;
                }
            }
        }
    }
}

fn capitalize_char(buf: &mut String, start: usize) {
    match &buf[start..].char_indices().next() {
        Some((_, c)) if c.is_lowercase() => {
            map_chars(buf, start..start + c.len_utf8(), |c| Some(c.to_uppercase()));
        }
        _ => {}
    }
}

/// Check if a character is a CJK character.
pub(crate) fn is_cjk(c: char) -> bool {
    let cp: u32 = c.into();
    (0x4E00..=0x9FFF).contains(&cp)
        || (0x3400..=0x4DBF).contains(&cp)
        || (0x20000..=0x2A6DF).contains(&cp)
        || (0x2A700..=0x2B73F).contains(&cp)
        || (0x2B740..=0x2B81F).contains(&cp)
        || (0x2B820..=0x2CEAF).contains(&cp)
        || (0xF900..=0xFAFF).contains(&cp)
        || (0x2F800..=0x2FA1F).contains(&cp)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fold_char_class() {
        let test = "Hi there, I am! Will.I.Am is here";
        let mut case = CharClass::default();
        for (i, c) in test.chars().enumerate() {
            case = case.step(c, true);

            if [
                0, 1, 3, 4, 5, 6, 7, 10, 12, 13, 16, 17, 18, 19, 21, 23, 24, 26, 27, 29,
                30, 31, 32,
            ]
            .contains(&i)
            {
                assert_eq!(case, CharClass::InWord);
            } else if [2, 11, 25, 28].contains(&i) {
                assert_eq!(case, CharClass::NewWord);
            } else if i == 9 {
                assert_eq!(case, CharClass::NewSubclause);
            } else if i == 15 {
                assert_eq!(case, CharClass::NewSentence);
            } else if i == 8 {
                assert_eq!(case, CharClass::MaybeSubclause);
            } else if [14, 20, 22].contains(&i) {
                assert_eq!(case, CharClass::MaybeNewSentence);
            } else {
                panic!("Unexpected char class at {i}: {case:?}");
            }
        }
    }

    #[test]
    fn fold_words() {
        let test = "Hi, USA! I am airTastic.";
        let mut length = 0;
        let mut case = CharClass::default();
        let mut word_data: Option<WordData> = None;

        for c in test.chars() {
            let old_case = case;
            let curr_case = case.step(c, true);

            let new_word_data = match word_data {
                Some(idx) => {
                    idx.feed(c, length, length + c.len_utf8(), old_case, curr_case)
                }
                None => {
                    WordData::new(c, length, length + c.len_utf8(), old_case, curr_case)
                }
            };

            match new_word_data {
                Some(new_word_data) => {
                    // "Hi,", "USA!", "I"
                    assert!(
                        new_word_data.start_was_upper
                            == [0, 1, 2, 4, 5, 6, 7, 9].contains(&length),
                        "Unexpected start_was_upper {} at {}",
                        new_word_data.start_was_upper,
                        length
                    );

                    // "Hi," (true at start), "USA!", "I"
                    assert!(
                        new_word_data.follows_punctuation
                            == [0, 1, 2, 4, 5, 6, 7, 9].contains(&length),
                        "Unexpected follows_punctuation {} at {}",
                        new_word_data.follows_punctuation,
                        length
                    );

                    // "Hi,"
                    if length == 2 {
                        assert_eq!(new_word_data.start, 0);
                        assert_eq!(new_word_data.end, 3);
                    }

                    // "USA!"
                    if length == 7 {
                        assert_eq!(new_word_data.start, 4);
                        assert_eq!(new_word_data.end, 8);
                    }

                    // "I"
                    if length == 9 {
                        assert_eq!(new_word_data.start, 9);
                        assert_eq!(new_word_data.end, 10);
                    }

                    // "am"
                    if length == 12 {
                        assert_eq!(new_word_data.start, 11);
                        assert_eq!(new_word_data.end, 13);
                    }

                    // "airTastic."
                    if length == 23 {
                        assert_eq!(new_word_data.start, 14);
                        assert_eq!(new_word_data.end, 24);
                    }
                }
                None => {
                    assert!(
                        [3, 8, 10, 13].contains(&length),
                        "Unexpected word break at {length}"
                    );
                }
            }

            word_data = new_word_data;
            case = curr_case;
            length += c.len_utf8();
        }
    }

    #[test]
    fn title_case_last_word() {
        let case: Case = TitleCase {
            always_capitalize_last_word: true,
            ..Default::default()
        }
        .into();

        let title = case.transform("a holistic investigation of me in general so ");
        assert_eq!("A Holistic Investigation of Me in General So", title);
    }

    #[test]
    fn title_case_char_segmentation() {
        let case: Case = TitleCase::new().into();
        let title = case.transform("She AiN’T Be Getting on my Nerves");
        assert_eq!("She Ain’t Be Getting on My Nerves", title);

        let title =
            case.transform("We don’t bank on the Pope’s decisions being sensible");
        assert_eq!("We Don’t Bank on the Pope’s Decisions Being Sensible", title);
    }

    #[test]
    fn title_case_edge_cases() {
        let mut props = TitleCase::new();
        props.trim_start = false;
        props.trim_end = false;
        let case: Case = props.into();

        let title = case.transform("          crap.   oh  ");
        assert_eq!("          Crap.   Oh  ", title);

        let title = case.transform("");
        assert_eq!("", title);

        let title = case.transform("The reason I've got");
        assert_eq!("The Reason I've Got", title);

        let title = case.transform("UK");
        assert_eq!("UK", title);
    }

    #[test]
    fn title_case_punctuation() {
        let case: Case = TitleCase::new().into();

        let title =
            case.transform("Around a table: the reason why we just could not care");
        assert_eq!("Around a Table: The Reason Why We Just Could Not Care", title);

        let title =
        case.transform("'My colleague is a robot' – exploring frontline employees' willingness to work with collaborative service robots");
        assert_eq!(
            "'My Colleague Is a Robot' – Exploring Frontline Employees' Willingness to Work with Collaborative Service Robots",
            title
        );
    }

    #[test]
    fn title_case_verbatim() {
        let case: Case = TitleCase::new().into();
        let mut folder = CaseFolder::with_config(case);
        folder.push_str("I am ");
        folder.push_verbatim("S");
        folder.push_str("tokes");
        assert_eq!("I Am Stokes", folder.finish());
    }

    #[test]
    fn title_case_word_length() {
        let mut props = TitleCase::new();
        props.always_capitalize_min_len = Some(4);
        props.keep_all_uppercase_words = false;
        let case: Case = props.into();

        let title = case.transform("sToNES iNSidE OF CaVES");
        assert_eq!("Stones Inside of Caves", title);
    }

    #[test]
    fn title_case_name_detection() {
        let mut props = TitleCase::new();
        let case: Case = props.into();

        let title = case.transform("Exploring the NASA deep labs");
        assert_eq!("Exploring the NASA Deep Labs", title);

        props.keep_all_uppercase_words = false;
        let case: Case = props.into();

        let title = case.transform("P-HACKING IN SCIENCE: AN OBITUARY");
        assert_eq!("P-Hacking in Science: An Obituary", title);
    }

    #[test]
    fn title_case_full_stop_handling() {
        let mut props = TitleCase::new();
        props.always_capitalize_min_len = Some(4);
        let case: Case = props.into();

        let title = case.transform("Facebook.com and aHo are corporate behemoths");
        assert_eq!("Facebook.com and Aho Are Corporate Behemoths", title);

        let title = case.transform("Still. coming into focus.");
        assert_eq!("Still. Coming Into Focus.", title);
    }

    #[test]
    fn title_case_hyphens() {
        let case: Case = TitleCase::new().into();

        let title =
            case.transform("Comparative study of Self-reporting students' performance");
        assert_eq!("Comparative Study of Self-Reporting Students' Performance", title);
    }

    #[test]
    fn sentence_case() {
        let case: Case = SentenceCase::new().into();

        let title = case.transform("This page is not for Discussions. Please Use the Table below to Find the Most Appropriate Section to Post.");
        assert_eq!(
            "This page is not for discussions. Please use the table below to find the most appropriate section to post.",
            title
        );
    }

    #[test]
    fn online() {
        let case: Case = TitleCase::new().into();
        let mut folder = CaseFolder::with_config(case);
        folder.push_str("hello world ");
        folder.push_str("[");
        let case = folder.case;
        folder.reconfigure(Case::FirstUpper);
        folder.push_str("online");
        folder.reconfigure(case);
        folder.push_str("]");
        assert_eq!("Hello World [Online]", folder.finish());
    }

    #[test]
    fn sentence_case_char_segmentation() {
        let case: Case = SentenceCase::new().into();

        let title = case.transform("She AINT Be Getting on my Nerves");
        assert_eq!("She AINT be getting on my nerves", title);

        let title =
            case.transform("We don’t bank on the Pope’s decisions being sensible");
        assert_eq!("We don’t bank on the pope’s decisions being sensible", title);
    }

    #[test]
    fn sentence_case_edge_cases() {
        let mut props = SentenceCase::new();
        props.trim_start = false;
        props.trim_end = false;
        let case: Case = props.into();

        let title = case.transform("          crap.   oh  ");
        assert_eq!("          Crap.   Oh  ", title);

        let title = case.transform("");
        assert_eq!("", title);
    }

    #[test]
    fn sentence_case_dictionary() {
        let case: Case = SentenceCase::new().into();

        let title = case.transform(
            "if i must distance myself from the euroPe-centric mindset for a moment",
        );
        assert_eq!(
            "If I must distance myself from the Europe-centric mindset for a moment",
            title
        );
    }

    #[test]
    fn sentence_case_no_transform() {
        let case: Case = SentenceCase::new().into();

        let title = case.transform(
            "As seen in Figure 4.A, we achieved a significant performance increase",
        );
        assert_eq!(
            "As seen in figure 4.A, we achieved a significant performance increase",
            title
        );
    }

    #[test]
    fn sentence_case_name_detection() {
        let case: Case = SentenceCase::new().into();

        let title = case.transform(
            "We want to present GRAL, a localization algorithm for Wireless Sensor Networks",
        );
        assert_eq!(
            "We want to present GRAL, a localization algorithm for wireless sensor networks",
            title
        );

        let title = case
            .transform("Ubiquity airMAX is the next generation of networking hardware");
        assert_eq!(
            "Ubiquity Airmax is the next generation of networking hardware",
            title
        );

        let case: Case = SentenceCase {
            keep_all_uppercase_words: false,
            capitalize_words_with_caps_inside: false,
            ..Default::default()
        }
        .into();

        let title = case
            .transform("SOME PEOPLE CAN NEVER STOP TO SCREAM. IT IS DRIVING ME CRAZY!");
        assert_eq!(
            "Some people can never stop to scream. It is driving me crazy!",
            title
        );
    }
}
