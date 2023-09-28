use std::{cmp::Ordering, str::FromStr};

use strum::{Display, EnumString};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;
use yaml_rust::{yaml, Yaml};

use crate::lang::en::ARTICLES;
use crate::lang::is_cjk;
use crate::lang::name::NAME_PARTICLES;

use super::DeserializationError;

use super::{HayagrivaValue, ParseContext, YamlDictExt, YamlExt};

/// A list of persons with a common role.
pub type PersonsWithRoles = (Vec<Person>, PersonRole);

/// Specifies the role a group of persons had in the creation to the
/// cited item.
#[derive(Clone, Debug, Display, EnumString, PartialEq, Eq)]
#[non_exhaustive]
#[strum(serialize_all = "kebab_case")]
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
    #[strum(disabled)]
    Unknown(String),
}

/// Holds the name of a person.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Person {
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
            if ARTICLES.binary_search(&&self.name[0..space_idx]).is_ok() {
                &self.name[space_idx + 1..]
            } else {
                self.name.as_str()
            }
        } else {
            self.name.as_str()
        }
    }
}

impl HayagrivaValue for Person {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, super::DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => Ok(Person::from_strings(s.split(',').collect())?),
            Yaml::Hash(h) => {
                let name = h.get_with_str("name", ctx)?.as_deserialized_str()?;
                ctx.pop_dict_key();

                let mut optional_key = |key| -> Result<_, DeserializationError> {
                    let res = h
                        .get_with_str(key, ctx)
                        .ok()
                        .map(|v| v.as_deserialized_str().map(ToString::to_string))
                        .transpose()?;
                    ctx.pop_dict_key();
                    Ok(res)
                };

                Ok(Person {
                    name: name.to_string(),
                    given_name: optional_key("given-name")?,
                    prefix: optional_key("prefix")?,
                    suffix: optional_key("suffix")?,
                    alias: optional_key("alias")?,
                })
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        if self.alias.is_none() && self.prefix.is_none() {
            Yaml::String(self.name_first(false, false))
        } else {
            let entries = [
                ("name", Some(&self.name)),
                ("given-name", self.given_name.as_ref()),
                ("prefix", self.prefix.as_ref()),
                ("suffix", self.suffix.as_ref()),
                ("alias", self.alias.as_ref()),
            ];
            let hash = entries
                .iter()
                .filter_map(|(key, val)| {
                    val.map(|v| (Yaml::from_str(key), Yaml::from_str(v)))
                })
                .collect();

            Yaml::Hash(hash)
        }
    }

    fn explain() -> &'static str {
        "a name string or a dictionary with a \"name\" key"
    }
}

impl<T: HayagrivaValue> HayagrivaValue for Vec<T> {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, super::DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::Array(arr) => arr.iter().map(|v| T::from_yaml(v, ctx)).collect(),
            _ => T::from_yaml(yaml, ctx).map(|p| vec![p]),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        if self.len() == 1 {
            self[0].to_yaml()
        } else {
            Yaml::Array(self.iter().map(|p| p.to_yaml()).collect())
        }
    }

    fn explain() -> &'static str {
        "a list or a single value"
    }
}

impl HayagrivaValue for PersonsWithRoles {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, super::DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::Hash(h) => {
                let names = <Vec<Person>>::from_yaml(h.get_with_str("names", ctx)?, ctx)?;
                ctx.pop_dict_key();

                let role = PersonRole::from_str(
                    h.get_with_str("role", ctx)?.as_deserialized_str()?,
                )
                .map_err(|_| PersonError::UnknownRole)?;
                ctx.pop_dict_key();

                Ok((names, role))
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        let mut hash = yaml::Hash::new();

        hash.insert_with_str("names", self.0.to_yaml());
        hash.insert(Yaml::from_str("role"), Yaml::from_str(&self.1.to_string()));

        Yaml::Hash(hash)
    }

    fn explain() -> &'static str {
        "a dictionary with a \"names\" list and a \"role\" key"
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
