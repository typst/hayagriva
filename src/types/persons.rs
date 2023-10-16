use std::borrow::Cow;
use std::cmp::Ordering;
use std::str::FromStr;

use citationberg::LongShortForm;
use serde::ser::SerializeMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

use crate::lang::en::ARTICLES;
use crate::lang::is_cjk;
use crate::lang::name::NAME_PARTICLES;
use crate::util::{deserialize_one_or_many, serialize_one_or_many};

use super::derive_or_from_str;

/// A list of persons with a common role.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PersonsWithRoles {
    /// The persons.
    #[serde(serialize_with = "serialize_one_or_many")]
    #[serde(deserialize_with = "deserialize_one_or_many")]
    pub names: Vec<Person>,
    /// The role the persons had in the creation of the cited item.
    pub role: PersonRole,
}

impl PersonsWithRoles {
    /// Create a new list of persons with a common role.
    pub fn new(names: Vec<Person>, role: PersonRole) -> Self {
        Self { names, role }
    }
}

/// Specifies the role a group of persons had in the creation to the
/// cited item.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[non_exhaustive]
#[serde(rename_all = "kebab-case")]
pub enum PersonRole {
    /// Translated the work from a foreign language to the cited edition.
    Translator,
    /// Authored an afterword.
    Afterword,
    /// Authored an foreword.
    Foreword,
    /// Authored an introduction.
    Introduction,
    /// Provided value-adding annotations.
    Annotator,
    /// Commented the work.
    Commentator,
    /// Holds a patent or similar.
    Holder,
    /// Compiled the works in an [Anthology](super::EntryType::Anthology).
    Compiler,
    /// Founded the publication.
    Founder,
    /// Collaborated on the cited item.
    Collaborator,
    /// Organized the creation of the cited item.
    Organizer,
    /// Performed in the cited item.
    CastMember,
    /// Composed all or parts of the cited item's musical / audible components.
    Composer,
    /// Produced the cited item.
    Producer,
    /// Lead Producer for the cited item.
    ExecutiveProducer,
    /// Did the writing for the cited item.
    Writer,
    /// Shot film/video for the cited item.
    Cinematography,
    /// Directed the cited item.
    Director,
    /// Illustrated the cited item.
    Illustrator,
    /// Provided narration or voice-over for the cited item.
    Narrator,

    /// Various other roles described by the contained string.
    #[serde(skip)]
    Unknown(String),
}

derive_or_from_str! {
    /// Holds the name of a person.
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Person where "a name string or a dictionary with a \"name\" key" {
        /// The family name.
        pub name: String,
        /// The given name / forename.
        pub given_name: Option<String>,
        /// A prefix of the family name such as 'van' or 'de'.
        pub prefix: Option<String>,
        /// A suffix of the family name such as 'Jr.' or 'IV'.
        pub suffix: Option<String>,
        /// Another name (often user name) the person might be known under.
        pub alias: Option<String>,
    }
}

impl Serialize for Person {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Aliases are not represented in the string, prefixes can create
        // ambiguity.
        if self.alias.is_none() && self.prefix.is_none() {
            serializer.serialize_str(&self.name_first(false, false))
        } else {
            let entries = [
                ("name", Some(&self.name)),
                ("given-name", self.given_name.as_ref()),
                ("prefix", self.prefix.as_ref()),
                ("suffix", self.suffix.as_ref()),
                ("alias", self.alias.as_ref()),
            ];

            let map_len = entries.iter().filter(|(_, v)| v.is_some()).count();

            let mut map = serializer.serialize_map(Some(map_len))?;

            for (key, value) in entries.iter() {
                if let Some(value) = value {
                    map.serialize_entry(key, value)?;
                }
            }

            map.end()
        }
    }
}

/// Error that may occur when parsing a slice of strings as a name.
#[derive(Clone, Copy, Debug, Error, PartialEq, Eq)]
pub enum PersonError {
    /// The name has too many parts to be appropriately parsed.
    #[error("too many parts")]
    TooManyParts,
    /// The name is empty.
    #[error("part list is empty")]
    Empty,
    /// The role is unknown.
    #[error("unknown role")]
    UnknownRole,
}

impl Person {
    /// This function expects a list of strings with its length between one and
    /// three. The first part will be interpreted as the `<prefix> <Name>`, the
    /// second part as the given name and the third part as the suffix.
    ///
    /// The prefix and name are separated just like in BiBTeX, as described
    /// [Nicolas Markey describes in "Tame the BeaST"][taming], p. 24. The gist
    /// is that the given name will start at the first word with a capital
    /// letter, if there are any such words.
    ///
    /// The call site of this function in the library obtains the slice by
    /// calling `split(",")` on a string like `"Des Egdens, Britta"`.
    ///
    /// [taming]: https://ftp.rrze.uni-erlangen.de/ctan/info/bibtex/tamethebeast/ttb_en.pdf
    pub fn from_strings(mut parts: Vec<&str>) -> Result<Self, PersonError> {
        if parts.is_empty() {
            return Err(PersonError::Empty);
        } else if parts.len() > 3 {
            return Err(PersonError::TooManyParts);
        }

        for part in parts.iter_mut() {
            *part = part.trim();
        }

        let last_pre = parts[0];
        let given_name =
            if parts.len() > 1 { Some(parts.last().unwrap().to_string()) } else { None };

        let suffix = if parts.len() > 2 { Some(parts[1].to_string()) } else { None };

        let mut word_start = true;
        let mut last_lower_case_end: i32 = -1;
        let mut is_lowercase = false;
        let mut last_word_start = 0;
        let mut has_seen_uppercase_words = false;

        for (index, c) in last_pre.chars().enumerate() {
            if c.is_whitespace() {
                word_start = true;
                continue;
            }

            if word_start {
                last_word_start = index;

                if c.is_lowercase() {
                    is_lowercase = true;
                } else {
                    is_lowercase = false;
                    has_seen_uppercase_words = true;
                }
            }

            if is_lowercase {
                last_lower_case_end = index as i32;
            }

            word_start = false;
        }

        let mut name = String::new();
        let mut prefix = String::new();
        for (index, c) in last_pre.chars().enumerate() {
            if (index as i32 <= last_lower_case_end && has_seen_uppercase_words)
                || (!has_seen_uppercase_words && index < last_word_start)
            {
                prefix.push(c);
            } else if has_seen_uppercase_words || index >= last_word_start {
                name.push(c);
            }
        }

        let prefix = if prefix.is_empty() { None } else { Some(prefix) };
        if prefix.is_some() {
            name = name.trim_start().to_string();
        }

        Ok(Person { name, given_name, prefix, suffix, alias: None })
    }

    /// Formats the given name into initials.
    ///
    /// For example, `"Judith Beatrice"` would yield `"J. B."` if the
    /// `delimiter` argument is set to `Some(".")`, `"Klaus-Peter"` would become
    /// `"K-P"` without a delimiter.
    pub fn initials(
        &self,
        buf: &mut impl std::fmt::Write,
        delimiter: Option<&str>,
        with_hyphen: bool,
    ) -> std::fmt::Result {
        let Some(gn) = &self.given_name else {
            return Ok(());
        };

        let mut collect = true;
        let mut non_empty = false;

        for (_, gr) in gn.grapheme_indices(true) {
            if let Some(c) = gr.chars().next() {
                if c.is_whitespace() || c == '-' {
                    if !collect {
                        if let Some(delimiter) = delimiter {
                            buf.write_str(delimiter)?;
                        }

                        collect = true;
                        buf.write_char(if with_hyphen { c } else { ' ' })?;
                    }
                    continue;
                }
            }

            if collect {
                buf.write_str(gr)?;
                collect = false;
                non_empty = true;
            }
        }

        if non_empty && !collect {
            if let Some(delim) = delimiter {
                buf.write_str(delim)?;
            }
        }

        Ok(())
    }

    /// Yields the first name of a person. Will add the delimiter after initials
    /// / single letters.
    pub fn first_name_with_delimiter(
        &self,
        buf: &mut impl std::fmt::Write,
        delimiter: Option<&str>,
    ) -> std::fmt::Result {
        let Some(name) = &self.given_name else {
            return Ok(());
        };

        let mut first = true;
        for item in name.split(' ') {
            if !first {
                buf.write_char(' ')?;
            }

            buf.write_str(item)?;

            if let Some(delimiter) = delimiter {
                if item.graphemes(true).count() == 1 {
                    buf.write_str(delimiter)?;
                }
            }

            first = false;
        }

        Ok(())
    }

    /// Get the name with the family name fist, the initials
    /// afterwards, separated by a comma.
    pub fn name_first(&self, initials: bool, prefix_given_name: bool) -> String {
        let mut res = if !prefix_given_name {
            if let Some(prefix) = &self.prefix {
                format!("{} {}", prefix, self.name)
            } else {
                self.name.clone()
            }
        } else {
            self.name.clone()
        };

        if initials {
            if self.given_name.is_some() {
                res += ", ";
                self.initials(&mut res, Some("."), true).unwrap();
            }
        } else if let Some(given_name) = &self.given_name {
            res += ", ";
            res += given_name;
        }

        if prefix_given_name {
            if let Some(prefix) = &self.prefix {
                if self.given_name.is_some() {
                    res.push(' ');
                }

                res += prefix;
            }
        }

        if let Some(suffix) = &self.suffix {
            res += ", ";
            res += suffix;
        }
        res
    }

    /// Get the name with the given name first, the family name afterwards.
    pub fn given_first(&self, initials: bool) -> String {
        let mut res = String::new();

        if initials {
            if self.given_name.is_some() {
                self.initials(&mut res, Some("."), true).unwrap();
                res.push(' ');
            }
        } else if let Some(given_name) = &self.given_name {
            res += given_name;
            res.push(' ');
        }

        if let Some(prefix) = &self.prefix {
            res += prefix;
            res += " ";
        }

        res += &self.name;

        if let Some(suffix) = &self.suffix {
            res += " ";
            res += suffix;
        }

        res
    }

    /// Get the non-dropping name particle in the family name.
    pub fn name_particle(&self) -> Option<&str> {
        for (idx, char) in self.name.char_indices().rev() {
            if char != ' ' {
                continue;
            }

            let particle = &self.name[0..idx];
            let lowercase = particle.to_lowercase();
            if NAME_PARTICLES.binary_search(&lowercase.as_str()).is_ok() {
                return Some(particle);
            }
        }

        None
    }

    /// Get the family name without the non-dropping particle.
    pub fn name_without_particle(&self) -> &str {
        if let Some(particle) = self.name_particle() {
            self.name[particle.len()..].trim_start()
        } else {
            self.name.as_str()
        }
    }

    /// Get only the dropping and non-dropping particle of the family name.
    pub fn name_particles(&self) -> Option<Cow<str>> {
        match (&self.prefix, self.name_particle()) {
            (Some(dropping), Some(non_dropping)) => {
                Some(Cow::Owned(format!("{} {}", dropping, non_dropping)))
            }
            (Some(dropping), None) => Some(Cow::Borrowed(dropping.as_str())),
            (None, Some(non_dropping)) => Some(Cow::Borrowed(non_dropping)),
            (None, None) => None,
        }
    }

    /// Whether to treat this as an institutional name.
    pub fn is_institutional(&self) -> bool {
        self.given_name.is_none() && self.suffix.is_none() && self.prefix.is_none()
    }

    /// Whether the name contains CJK characters.
    pub fn is_cjk(&self) -> bool {
        self.name.chars().any(is_cjk)
            || self.given_name.as_ref().map_or(false, |gn| gn.chars().any(is_cjk))
    }

    /// Get the name without the leading article.
    pub fn name_without_article(&self) -> &str {
        let space_idx = self.name.find(' ');
        if let Some(space_idx) = space_idx {
            if space_idx + 1 != self.name.len()
                && ARTICLES.binary_search(&&self.name[0..space_idx]).is_ok()
            {
                &self.name[space_idx + 1..]
            } else {
                self.name.as_str()
            }
        } else {
            self.name.as_str()
        }
    }

    /// Order according to the CSL specification.
    pub(crate) fn csl_cmp(
        &self,
        other: &Self,
        form: LongShortForm,
        demote_particle: bool,
    ) -> std::cmp::Ordering {
        let self_cjk = self.is_cjk();
        let other_cjk = other.is_cjk();

        if self_cjk != other_cjk {
            // Put CJK names last.
            return self_cjk.cmp(&other_cjk);
        } else if self_cjk && other_cjk {
            // Apply special CJK rules.
            return match form {
                LongShortForm::Long => self
                    .name
                    .cmp(&other.name)
                    .then_with(|| self.given_name.cmp(&other.given_name)),
                LongShortForm::Short => self.name.cmp(&other.name),
            };
        }

        if demote_particle {
            self.name_without_particle()
                .cmp(other.name_without_particle())
                .then_with(|| self.name_particles().cmp(&other.name_particles()))
                .then_with(|| self.given_name.cmp(&other.given_name))
                .then_with(|| self.suffix.cmp(&other.suffix))
        } else {
            self.name
                .cmp(&other.name)
                .then_with(|| self.prefix.cmp(&other.prefix))
                .then_with(|| self.given_name.cmp(&other.given_name))
                .then_with(|| self.suffix.cmp(&other.suffix))
        }
    }
}

impl Ord for Person {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name
            .cmp(&other.name)
            .then(self.given_name.cmp(&other.given_name))
            .then(self.suffix.cmp(&other.suffix))
            .then(self.prefix.cmp(&other.prefix))
    }
}

impl PartialOrd for Person {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl FromStr for Person {
    type Err = PersonError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_strings(s.split(',').collect())
    }
}

#[cfg(test)]
mod tests {
    use super::Person;

    #[test]
    fn person_initials() {
        let mut s = String::new();
        let p = Person::from_strings(vec!["Dissmer", "Courtney Deliah"]).unwrap();
        p.initials(&mut s, Some("."), true).unwrap();
        assert_eq!("C. D.", s);

        let mut s = String::new();
        let p = Person::from_strings(vec!["Günther", "Hans-Joseph"]).unwrap();
        p.initials(&mut s, None, true).unwrap();
        assert_eq!("H-J", s);
    }

    #[test]
    fn person_non_dropping_initials() {
        let p = Person::from_strings(vec!["Von Der Leyen", "Ursula"]).unwrap();
        assert_eq!("Von Der", p.name_particle().unwrap());
        assert_eq!("Leyen", p.name_without_particle());
    }
    #[test]
    fn person_middle_initial() {
        let p = Person::from_strings(vec!["Kirk", "James T"]).unwrap();
        let mut s = String::new();
        p.first_name_with_delimiter(&mut s, Some(".")).unwrap();
        assert_eq!("James T.", s);
    }
}
