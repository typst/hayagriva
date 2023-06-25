//! Language-dependant string transformations.

pub(crate) mod en;

/// Convert a string to a well-defined lower/uppercase scheme.
pub trait Case {
    /// Apply the case transformation to the argument.
    fn apply(&self, string: &str) -> String;
}

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
            always_capitalize_last_word: true,
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

impl Case for TitleCase {
    /// Put the `title` argument into title case (every word starts with a capital
    /// letter, except for some prepositions...) as specified by `&self`.
    fn apply(&self, title: &str) -> String {
        let title = match (self.trim_start, self.trim_end) {
            (true, true) => title.trim(),
            (true, false) => title.trim_start(),
            (false, true) => title.trim_end(),
            (false, false) => title,
        };

        // Do lowercase letters appear?
        // Otherwise, acronym detection will not be performed
        let mut has_lowercase = false;

        // Indices at which words start and
        // whether they should force capitalization.
        let mut word_indices = vec![];

        // Next non-ws or punctuation character is a word start
        let mut resume_word = true;
        // Should the next word force capitalizaion?
        let mut priority_override = true;
        let mut resume_start = 0;

        let mut i = 0;

        for c in title.chars() {
            if !title.is_char_boundary(i) {
                i += c.len_utf8();
                continue;
            }
            if has_lowercase || c.is_lowercase() {
                has_lowercase = true;
            }

            if c == '.' || c == ':' || c == '?' || c == '!' {
                priority_override = self.always_capitalize_after_punctuation;
            } else if c == '('
                || c == '-' && self.hyphen_word_separator
                || c == '['
                || c == '{'
                || c == ','
                || c == ';'
                || c == '"'
                || (c == '\'' && self.use_exception_dictionary)
                || (c == '’' && self.use_exception_dictionary)
                || c.is_whitespace()
            {
                if !resume_word {
                    resume_start = i;
                }
                resume_word = true;
            } else if resume_word {
                word_indices.push((i, resume_start, priority_override));
                priority_override = false;
                resume_word = false;
            } else {
                // Priority override only stays on while non-word chars appear
                priority_override = false;
            }

            i += c.len_utf8();
        }

        let mut word_length = vec![];
        for i in 0..word_indices.len() {
            let index = word_indices[i].0;
            let len = if let Some((_, re_start, _)) = word_indices.get(i + 1) {
                re_start - index
            } else {
                title.len() - index
            };

            let retain = has_lowercase
                && self.keep_all_uppercase_words
                && title[index..index + len]
                    .chars()
                    .all(|c| c.is_uppercase() || !c.is_alphanumeric());

            word_length.push((len, retain));
        }

        let word_indices: Vec<(usize, bool, usize, bool)> = word_indices
            .into_iter()
            .zip(word_length.into_iter())
            .map(|((ind, _, prio), (len, retain))| (ind, prio, len, retain))
            .collect();

        let mut res = String::new();
        let mut iter = title.chars();

        let mut insert_index = 0;
        let total_words = word_indices.len();
        let mut last_retain = false;
        for (word_index, (index, force_cap, len, retain)) in
            word_indices.into_iter().enumerate()
        {
            let last = word_index == total_words - 1;

            while insert_index < index {
                let c = iter.next().expect("title string terminates before word start");
                if last_retain {
                    res.push(c)
                } else {
                    res.push_str(&c.to_lowercase().to_string());
                }
                insert_index += c.len_utf8();
            }

            let c = iter.next().expect("title string terminates before word start");
            last_retain = retain;

            if force_cap
                || (last && self.always_capitalize_last_word)
                || len >= self.always_capitalize_min_len.unwrap_or(usize::MAX)
            {
                res.push_str(&c.to_uppercase().to_string());
            } else {
                // Collect word to check it against the database
                let word = &title[index..index + len].to_lowercase();

                if self.use_exception_dictionary
                    && en::NEVER_CAPITALIZE.binary_search(&word.as_str()).is_ok()
                {
                    res.push_str(&c.to_lowercase().to_string());
                } else {
                    res.push_str(&c.to_uppercase().to_string());
                }
            }

            insert_index += c.len_utf8();
        }

        // Deplete iterator
        for c in iter {
            if last_retain  {
                res.push_str(&c.to_string());
            }
            else {            
                res.push_str(&c.to_lowercase().to_string());
            }        
        }

        res
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
    /// Construct TitleCaseProperties with the default values.
    pub fn new() -> Self {
        Default::default()
    }
}

impl Case for SentenceCase {
    /// Put the `title` argument into sentence case (capitalize only after
    /// sentence ends or at the start, as well as (if the whitelist is used)
    /// select exceptions).
    fn apply(&self, title: &str) -> String {
        // Do lowercase letters appear?
        // Otherwise, acronym detection will not be performed
        let mut has_lowercase = false;

        // Indices at which words start and
        // whether they should force capitalization.
        let mut word_indices = vec![];

        // Next non-ws or punctuation character is a word start
        let mut resume_word = true;
        // Should the next word be capitalized?
        let mut do_uppercase = true;
        // Should the next word be exempt from transformation?
        let mut no_transformation = false;
        let mut resume_start = 0;

        let mut i = 0;

        for c in title.chars() {
            if !title.is_char_boundary(i) {
                i += c.len_utf8();
                continue;
            }
            if has_lowercase || c.is_lowercase() {
                has_lowercase = true;
            }

            if c == '.' || c == ':' {
                if !resume_word {
                    resume_start = i;
                }
                no_transformation = true;
                resume_word = true;
            } else if c == '?' || c == '!' {
                if !resume_word {
                    resume_start = i;
                }
                do_uppercase = true;
                no_transformation = false;
                resume_word = true;
            } else if c == '-' || c == ',' || c == ';' || c.is_whitespace() {
                if !resume_word {
                    resume_start = i;
                }
                resume_word = true;

                if no_transformation {
                    do_uppercase = true;
                    no_transformation = false;
                }
            } else if resume_word {
                word_indices.push((i, resume_start, do_uppercase, no_transformation));
                do_uppercase = false;
                no_transformation = false;
                resume_word = false;
            }

            i += c.len_utf8();
        }

        /// Describes the case situation of a word.
        #[derive(Debug, PartialEq, Eq)]
        enum CaseSituation {
            /// This word is in all-caps.
            AllUppercase,
            /// Word contains upper-case characters in non-start position, like `FizzBuzz`.
            HasNonFirstUppercase,
            /// Word contains no upper-case characters or only at the start.
            Normal,
        }

        let mut word_length = vec![];
        for i in 0..word_indices.len() {
            let index = word_indices[i].0;
            let len = if let Some((_, resume_start, _, _)) = word_indices.get(i + 1) {
                resume_start - index
            } else {
                title.len() - index
            };

            let mut index_next = index + 1;

            while index_next < index + len
                && index_next < title.len()
                && !title.is_char_boundary(index_next)
            {
                index_next += 1;
            }

            let situation = if len <= 1 {
                CaseSituation::Normal
            } else if title[index..index + len]
                .chars()
                .all(|c| c.is_uppercase() || !c.is_alphanumeric())
            {
                CaseSituation::AllUppercase
            } else if title[index_next..index + len].chars().any(|c| c.is_uppercase()) {
                CaseSituation::HasNonFirstUppercase
            } else {
                CaseSituation::Normal
            };

            word_length.push((len, situation));
        }

        let word_indices: Vec<(usize, bool, bool, usize, CaseSituation)> = word_indices
            .into_iter()
            .zip(word_length.into_iter())
            .map(|((ind, _, upper, no_tf), (len, situation))| {
                (ind, upper, no_tf, len, situation)
            })
            .collect();

        let mut res = String::new();
        let mut iter = title.chars();

        let mut insert_index = 0;
        let mut last_retain = false;
        for (index, force_cap, no_transform, len, situation) in word_indices.into_iter() {
            while insert_index < index {
                let c = iter.next().expect("title string terminates before word start");
                if last_retain {
                    res.push(c)
                } else {
                    res.push_str(&c.to_lowercase().to_string());
                }
                insert_index += c.len_utf8();
            }

            let c = iter.next().expect("title string terminates before word start");
            last_retain = (no_transform && self.do_not_format_after_dot
                || situation == CaseSituation::AllUppercase
                    && (self.keep_all_uppercase_words
                        || self.capitalize_words_with_caps_inside))
                && has_lowercase;

            if force_cap
                || ((situation == CaseSituation::HasNonFirstUppercase
                    && self.capitalize_words_with_caps_inside)
                    || (situation == CaseSituation::AllUppercase
                        && (self.keep_all_uppercase_words
                            || self.capitalize_words_with_caps_inside)))
                    && has_lowercase
            {
                res.push_str(&c.to_uppercase().to_string());
            } else if no_transform && self.do_not_format_after_dot && has_lowercase {
                res.push(c);
            } else {
                // Collect word to check it against the database
                let word = &title[index..index + len];
                let uppercase = word.chars().any(|c| c.is_uppercase());

                if self.use_exception_dictionary
                    && (uppercase || word.len() < 2)
                    && en::ALWAYS_CAPITALIZE
                        .binary_search(&word.to_lowercase().as_str())
                        .is_ok()
                {
                    res.push_str(&c.to_uppercase().to_string());
                } else {
                    res.push_str(&c.to_lowercase().to_string());
                }
            }

            insert_index += c.len_utf8();
        }

        // Deplete iterator
        for c in iter {
            res.push_str(&c.to_lowercase().to_string());
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::{Case, SentenceCase, TitleCase};

    #[test]
    fn title_case_last_word() {
        let props = TitleCase::new();

        let title = props.apply("a holistic investigation of me in general so ");
        assert_eq!("A Holistic Investigation of Me in General So", title);
    }

    #[test]
    fn title_case_char_segmentation() {
        let props = TitleCase::new();
        let title = props.apply("She AiN’T Be Getting on my Nerves");
        assert_eq!("She Ain’t Be Getting on My Nerves", title);

        let title = props.apply("We don’t bank on the Pope’s decisions being sensible");
        assert_eq!("We Don’t Bank on the Pope’s Decisions Being Sensible", title);
    }

    #[test]
    fn title_case_edge_cases() {
        let mut props = TitleCase::new();
        props.trim_start = false;
        props.trim_end = false;

        let title = props.apply("          crap.   oh  ");
        assert_eq!("          Crap.   Oh  ", title);

        let title = props.apply("");
        assert_eq!("", title);
    }

    #[test]
    fn title_case_punctuation() {
        let props = TitleCase::new();

        let title = props.apply("Around a table: the reason why we just could not care");
        assert_eq!("Around a Table: The Reason Why We Just Could Not Care", title);
    }

    #[test]
    fn title_case_word_length() {
        let mut props = TitleCase::new();
        props.always_capitalize_min_len = Some(4);
        props.keep_all_uppercase_words = false;

        let title = props.apply("sToNES iNSidE OF CaVES");
        assert_eq!("Stones Inside of Caves", title);
    }

    #[test]
    fn title_case_name_detecion() {
        let props = TitleCase::new();

        let title = props.apply("Exploring NASA's new moon strategy");
        assert_eq!("Exploring NASA's New Moon Strategy", title);

        let title = props.apply("P-HACKING IN SCIENCE: AN OBITUARY");
        assert_eq!("P-Hacking in Science: An Obituary", title);
    }

    #[test]
    fn title_case_full_stop_handling() {
        let mut props = TitleCase::new();
        props.always_capitalize_min_len = Some(4);

        let title = props.apply("Facebook.com and aHo are corporate behemoths");
        assert_eq!("Facebook.com and Aho Are Corporate Behemoths", title);

        let title = props.apply("Still. coming into focus.");
        assert_eq!("Still. Coming Into Focus.", title);
    }

    #[test]
    fn title_case_hyphens() {
        let props = TitleCase::new();

        let title =
            props.apply("Comparative study of Self-reporting students' performance");
        assert_eq!("Comparative Study of Self-Reporting Students' Performance", title);
    }

    #[test]
    fn sentence_case() {
        let props = SentenceCase::new();

        let title =
            props.apply("This page is not for Discussions. Please Use the Table below to Find the Most Appropriate Section to Post.");
        assert_eq!(
            "This page is not for discussions. Please use the table below to find the most appropriate section to post.",
            title
        );
    }

    #[test]
    fn sentence_case_char_segmentation() {
        let props = SentenceCase::new();
        let title = props.apply("She AIN’T Be Getting on my Nerves");
        assert_eq!("She AIN’T be getting on my nerves", title);

        let title = props.apply("We don’t bank on the Pope’s decisions being sensible");
        assert_eq!("We don’t bank on the pope’s decisions being sensible", title);
    }

    #[test]
    fn sentence_case_edge_cases() {
        let mut props = SentenceCase::new();
        props.trim_start = false;
        props.trim_end = false;

        let title = props.apply("          crap.   oh  ");
        assert_eq!("          Crap.   Oh  ", title);

        let title = props.apply("");
        assert_eq!("", title);
    }

    #[test]
    fn sentence_case_dictionary() {
        let props = SentenceCase::new();

        let title = props.apply(
            "if i may distance myself from the euroPe-centric mindset for a moment",
        );
        assert_eq!(
            "If I may distance myself from the Europe-centric mindset for a moment",
            title
        );
    }

    #[test]
    fn sentence_case_no_transform() {
        let props = SentenceCase::new();

        let title = props.apply(
            "As seen in Figure 4.A, we achieved a significant performance increase",
        );
        assert_eq!(
            "As seen in figure 4.A, we achieved a significant performance increase",
            title
        );
    }

    #[test]
    fn sentence_case_name_detection() {
        let props = SentenceCase::new();

        let title = props.apply("We want to present GRAL, a localization algorithm for Wireless Sensor Networks");
        assert_eq!(
            "We want to present GRAL, a localization algorithm for wireless sensor networks",
            title
        );

        let title =
            props.apply("Ubiquity airMAX is the next generation of networking hardware");
        assert_eq!(
            "Ubiquity Airmax is the next generation of networking hardware",
            title
        );

        let title =
            props.apply("SOME PEOPLE CAN NEVER STOP TO SCREAM. IT IS DRIVING ME CRAZY!");
        assert_eq!(
            "Some people can never stop to scream. It is driving me crazy!",
            title
        );
    }
}
