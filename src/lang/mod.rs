use usize;

mod en;

/// Rules for the title case transformation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TitleCaseTransformer {
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
    /// Treat the hyphen as a word seperator, i.e. capitalize in hyphenated
    /// expressions.
    pub hyphen_word_seperator: bool,
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

impl Default for TitleCaseTransformer {
    fn default() -> Self {
        Self {
            always_capitalize_after_punctuation: true,
            always_capitalize_last_word: true,
            always_capitalize_min_len: None,
            hyphen_word_seperator: true,
            keep_all_uppercase_words: true,
            use_exception_dictionary: true,
            trim_start: true,
            trim_end: true,
        }
    }
}

impl TitleCaseTransformer {
    /// Construct TitleCaseProperties with the default values.
    pub fn new() -> Self {
        Default::default()
    }

    /// Put the `title` argument into title case (every word starts with a capital
    /// letter, except for some prepositions...) as specified by the `props` argument.
    pub fn apply(&self, title: &str) -> String {
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

        for (i, c) in title.chars().enumerate() {
            if has_lowercase || c.is_lowercase() {
                has_lowercase = true;
            }

            if c == '.' || c == ':' || c == '?' || c == '!' {
                priority_override = self.always_capitalize_after_punctuation;
            } else if c == '('
                || c == '-' && self.hyphen_word_seperator
                || c == '['
                || c == '{'
                || c == '\'' && self.use_exception_dictionary
                || c == '’' && self.use_exception_dictionary
                || c.is_whitespace()
            {
                resume_word = true;
            } else if resume_word {
                word_indices.push((i, priority_override));
                priority_override = false;
                resume_word = false;
            } else {
                // Priority override only stays on while non-word chars appear
                priority_override = false;
            }
        }

        let mut word_length = vec![];
        for i in 0 .. word_indices.len() {
            let index = word_indices[i].0;
            let len = if let Some((next_index, _)) = word_indices.get(i + 1) {
                next_index - index - 1
            } else {
                title.chars().count() - index
            };

            let retain = has_lowercase
                && self.keep_all_uppercase_words
                && title[index .. index + len].chars().all(|c| c.is_uppercase());

            word_length.push((len, retain));
        }

        let word_indices: Vec<(usize, bool, usize, bool)> = word_indices
            .into_iter()
            .zip(word_length.into_iter())
            .map(|((ind, prio), (len, retain))| (ind, prio, len, retain))
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
                insert_index += 1;
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
                let word = &title[index .. index + len].to_lowercase();

                if self.use_exception_dictionary
                    && en::NEVER_CAPITALIZE.binary_search(&word.as_str()).is_ok()
                {
                    res.push_str(&c.to_lowercase().to_string());
                } else {
                    res.push_str(&c.to_uppercase().to_string());
                }
            }

            insert_index += 1;
        }

        // Deplete iterator
        while let Some(c) = iter.next() {
            res.push_str(&c.to_lowercase().to_string());
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::TitleCaseTransformer;

    #[test]
    fn title_case_last_word() {
        let props = TitleCaseTransformer::new();

        let title = props.apply("a holistic investigation of me in general so ");
        assert_eq!("A Holistic Investigation of Me in General So", title);
    }

    #[test]
    fn title_case_edge_cases() {
        let mut props = TitleCaseTransformer::new();
        props.trim_start = false;
        props.trim_end = false;

        let title = props.apply("          crap.   oh  ");
        assert_eq!("          Crap.   Oh  ", title);

        let title = props.apply("");
        assert_eq!("", title);
    }

    #[test]
    fn title_case_punctuation() {
        let props = TitleCaseTransformer::new();

        let title = props.apply("Around a table: the reason why we just could not care");
        assert_eq!(
            "Around a Table: The Reason Why We Just Could Not Care",
            title
        );
    }

    #[test]
    fn title_case_word_length() {
        let mut props = TitleCaseTransformer::new();
        props.always_capitalize_min_len = Some(4);
        props.keep_all_uppercase_words = false;

        let title = props.apply("sToNES iNSidE OF CaVES");
        assert_eq!("Stones Inside of Caves", title);
    }

    #[test]
    fn title_case_name_detecion() {
        let props = TitleCaseTransformer::new();

        let title = props.apply("Exploring NASA's new moon strategy");
        assert_eq!("Exploring NASA's New Moon Strategy", title);

        let title = props.apply("P-HACKING IN SCIENCE: AN OBITUARY");
        assert_eq!("P-Hacking in Science: An Obituary", title);
    }

    #[test]
    fn title_case_full_stop_handling() {
        let mut props = TitleCaseTransformer::new();
        props.always_capitalize_min_len = Some(4);

        let title = props.apply("Facebook.com and aHo are corporate behemoths");
        assert_eq!("Facebook.com and Aho Are Corporate Behemoths", title);

        let title = props.apply("Still. coming into focus.");
        assert_eq!("Still. Coming Into Focus.", title);
    }

    #[test]
    fn title_case_hyphens() {
        let props = TitleCaseTransformer::new();

        let title =
            props.apply("Comparative study of Self-reporting students' performance");
        assert_eq!(
            "Comparative Study of Self-Reporting Students' Performance",
            title
        );
    }
}
