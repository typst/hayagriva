//! Hayagriva provides a YAML-backed format and data model for various
//! bibliography items as well as formatting for both in-text citations and
//! reference lists based on these literature databases.
//!
//! The crate is intended to assist scholarly writing and reference management
//! and can be used both through a CLI and an API.

#![warn(missing_docs)]

use std::collections::HashMap;
use std::str::FromStr;

pub mod lang;
pub mod output;
pub mod selectors;
pub mod types;

use lang::CaseTransformer;
use types::{
    get_range, Date, Duration, EntryType, FormattableString, FormattedString,
    FormattedTitle, NumOrStr, Person, PersonRole, QualifiedUrl, Title,
};

use linked_hash_map::LinkedHashMap;
use paste::paste;
use std::convert::TryFrom;
use strum_macros::Display;
use thiserror::Error;
use unic_langid::LanguageIdentifier;
use url::Url;
use yaml_rust::{Yaml, YamlLoader};

/// The field type enum variants describe what data types can possibly be held
/// by the various content items of an [Entry].
#[derive(Clone, Debug, Display, PartialEq)]
pub enum FieldType {
    /// A [Title] containing a canonical value and optionally translations and
    /// shorthands, all of which are formattable.
    Title(Title),
    /// A [FormattableString] with which the user can override various
    /// automatic formatters.
    FormattableString(FormattableString),
    /// A [FormattedString] is a [FormattableString] to which all desired
    /// formatters have been applied.
    FormattedString(FormattedString),
    /// A string to be reproduced as-is.
    Text(String),
    /// An integer.
    Integer(i64),
    /// A date, possibly only a year.
    Date(Date),
    /// A number of [Person]s.
    Persons(Vec<Person>),
    /// A list of [roles](PersonRole) with their assigned [Person]s.
    PersonsWithRoles(Vec<(Vec<Person>, PersonRole)>),
    /// This could be both an Integer or a Number.
    IntegerOrText(NumOrStr),
    /// A range between two integers.
    Range(std::ops::Range<i64>),
    /// A duration (of a song or an performance for example).
    Duration(Duration),
    /// A part of a period.
    TimeRange(std::ops::Range<Duration>),
    /// An [URL, possibly with a date of when it has been consulted](QualifiedUrl).
    Url(QualifiedUrl),
    /// A [Unicode Language Identifier](LanguageIdentifier).
    Language(LanguageIdentifier),
    /// Other [entries](Entry).
    Entries(Vec<Entry>),
}

/// A citable item in a bibliography.
#[derive(Clone, Debug, PartialEq)]
pub struct Entry {
    /// A string by which the Entry can be identified.
    key: String,
    /// The kind of media this Entry represents.
    entry_type: EntryType,
    /// Information about the Entry.
    content: HashMap<String, FieldType>,
}

impl Entry {
    /// Construct a new, empty entry.
    pub fn new(key: &str, entry_type: EntryType) -> Self {
        Self {
            key: key.to_string(),
            entry_type,
            content: HashMap::new(),
        }
    }

    /// Get the unconverted value of a certain key.
    pub fn get(&self, key: &str) -> Option<&FieldType> {
        self.content.get(key)
    }

    /// Set [any data type](FieldType) as value for a given key.
    pub fn set(&mut self, key: String, value: FieldType) -> Result<(), EntrySetError> {
        let valid = match key.as_ref() {
            "parent" => matches!(value, FieldType::Entries(_)),
            "title" => matches!(value, FieldType::Title(_)),
            "location" | "publisher" | "archive" | "archive-location" => {
                matches!(value, FieldType::FormattableString(_))
            }
            "author" | "editor" => matches!(value, FieldType::Persons(_)),
            "date" => matches!(value, FieldType::Date(_)),
            "affiliated" => matches!(value, FieldType::PersonsWithRoles(_)),
            "organization" | "issn" | "isbn" | "doi" | "serial-number" | "note" => {
                matches!(value, FieldType::Text(_))
            }
            "issue" | "edition" => matches!(value, FieldType::IntegerOrText(_)),
            "volume" | "page-range" => matches!(value, FieldType::Range(_)),
            "volume-total" | "page-total" => matches!(value, FieldType::Integer(_)),
            "time-range" => matches!(value, FieldType::TimeRange(_)),
            "runtime" => matches!(value, FieldType::Duration(_)),
            "url" => matches!(value, FieldType::Url(_)),
            "language" => matches!(value, FieldType::Language(_)),
            _ => true,
        };

        if valid {
            self.content.insert(key, value);
            Ok(())
        } else {
            Err(EntrySetError::WrongType(key, value))
        }
    }
}

/// Error which occurs if the value of an entry field could not be retrieved.
#[derive(Clone, Error, Debug, PartialEq)]
pub enum EntrySetError {
    /// The [field type enum](FieldType) does not match the expected variant(s).
    #[error("Type `{1}` not allowed in field `{0}`")]
    WrongType(String, FieldType),
}

macro_rules! fields {
    ($($name:ident: $field_name:expr => FormattableString),* $(,)*) => {
        $(
            fields!(fmt $name: $field_name => FormattableString, FormattedString);
            paste! {
                #[doc = "Get and parse the `" $field_name "` field's value.'"]
                pub fn [<get_ $name>](&self) -> Option<&str> {
                    self.get($field_name)
                        .map(|item| <&FormattableString>::try_from(item).unwrap().value.as_ref())
                }
            }
        )*
    };
    ($($name:ident: $field_name:expr => Title),* $(,)*) => {
        $(
            fields!(fmt $name: $field_name => Title, FormattedTitle);
            paste! {
                #[doc = "Get and parse the `" $field_name "` field's value.'"]
                pub fn [<get_ $name>](&self) -> Option<&str> {
                    self.get($field_name)
                        .map(|item| <&Title>::try_from(item).unwrap().value.value.as_ref())
                }
            }
        )*
    };
    (fmt $($name:ident: $field_name:expr => $src_type:ty, $dst_type:ty),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field as it stands (no formatting applied)."]
                pub fn [<get_ $name _raw>](&self) -> Option<&$src_type> {
                    self.get($field_name)
                        .map(|item| <&$src_type>::try_from(item).unwrap())
                }

                #[doc = "Get, parse, and format the `" $field_name "` field."]
                pub fn [<get_ $name _fmt>](
                    &self,
                    title: Option<&dyn CaseTransformer>,
                    sentence: Option<&dyn CaseTransformer>,
                ) -> Option<$dst_type> {
                    self.get($field_name)
                        .map(|item| <$src_type>::try_from(item.clone()).unwrap().format(title, sentence))
                }

                fields!(single_set $name => $field_name, $src_type);
            }
        )*
    };

    ($($name:ident: $field_name:expr $(=> $res:ty)?),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn [<get_ $name>](&self) -> Option<fields!(@type ref $($res)?)> {
                    self.get($field_name)
                        .map(|item| <fields!(@type ref $($res)?)>::try_from(item).unwrap())
                }

                fields!(single_set $name => $field_name, fields!(@type $($res)?));
            }
        )*
    };

    ($($name:ident: $field_name:expr => $res:ty, $res_ref:ty),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn [<get_ $name>](&self) -> Option<$res_ref> {
                    self.get($field_name)
                        .map(|item| <$res_ref>::try_from(item).unwrap())
                }

                fields!(single_set $name => $field_name, $res);
            }
        )*
    };

    (single_set $name:ident => $field_name:expr, $other_type:ty) => {
        paste! {
            #[doc = "Set a value in the `" $field_name "` field."]
            pub fn [<set_ $name>](&mut self, item: $other_type) {
                self.set($field_name.to_string(), FieldType::from(item)).unwrap();
            }
        }
    };

    (@type) => {String};
    (@type $res:ty) => {$res};
    (@type ref) => {&str};
    (@type ref $res:ty) => {&$res};
}

impl Entry {
    fields!(parents: "parent" => Vec<Entry>, &[Entry]);
    fields!(title: "title" => Title);

    /// Get and parse the `author` field. This will always return a result
    pub fn get_authors(&self) -> &[Person] {
        self.get("author")
            .map(|item| <&[Person]>::try_from(item).unwrap())
            .unwrap_or_default()
    }

    /// Get and parse the `author` field.
    /// This will fail if there are no authors.
    pub fn get_authors_fallible(&self) -> Option<&[Person]> {
        self.get("author").map(|item| <&[Person]>::try_from(item).unwrap())
    }

    fields!(single_set authors => "author", Vec<Person>);

    fields!(date: "date" => Date);
    fields!(
        editors: "editor" => Vec<Person>, &[Person],
        affiliated_persons: "affiliated" => Vec<(Vec<Person>, PersonRole)>, &[(Vec<Person>, PersonRole)]
    );

    /// Get and parse the `affiliated` field and only return persons of a given
    /// [role](PersonRole).
    pub fn get_affiliated_filtered(&self, role: PersonRole) -> Vec<Person> {
        self.get_affiliated_persons()
            .into_iter()
            .flat_map(|v| v)
            .filter_map(|(v, erole)| if erole == &role { Some(v) } else { None })
            .flatten()
            .cloned()
            .collect::<Vec<Person>>()
    }

    fields!(
        organization: "organization",
        issue: "issue" => NumOrStr,
        edition: "edition" => NumOrStr,
        volume: "volume" => std::ops::Range<i64>,
        total_volumes: "volume-total" => i64,
        page_range: "page-range" => std::ops::Range<i64>
    );

    /// Will recursively get a date off either the entry or its parents.
    pub fn get_any_date(&self) -> Option<&Date> {
        self.get_date().or_else(|| {
            self.get_parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.get_any_date())
                .next()
        })
    }

    /// Will recursively get an url off either the entry or its parents.
    pub fn get_any_url(&self) -> Option<&QualifiedUrl> {
        self.get_url().or_else(|| {
            self.get_parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.get_any_url())
                .next()
        })
    }

    /// Get and parse the `page-total` field, falling back on
    /// `page-range` if not specified.
    pub fn get_page_total(&self) -> Option<i64> {
        self.get("page-total")
            .map(|ft| ft.clone())
            .or_else(|| self.get_page_range().map(|r| FieldType::from(r.end - r.start)))
            .map(|item| i64::try_from(item).unwrap())
    }

    fields!(single_set total_pages => "page-total", i64);
    fields!(time_range: "time-range" => std::ops::Range<Duration>);

    /// Get and parse the `runtime` field, falling back on
    /// `time-range` if not specified.
    pub fn get_runtime(&self) -> Option<Duration> {
        self.get("runtime")
            .map(|ft| ft.clone())
            .or_else(|| self.get_time_range().map(|r| FieldType::from(r.end - r.start)))
            .map(|item| Duration::try_from(item).unwrap())
    }

    fields!(single_set runtime => "runtime", Duration);

    fields!(
        issn: "issn",
        isbn: "isbn",
        doi: "doi",
        serial_number: "serial-number",
        url: "url" => QualifiedUrl,
        language: "language" => LanguageIdentifier,
        note: "note"
    );
    fields!(
        location: "location" => FormattableString,
        publisher: "publisher" => FormattableString,
        archive: "archive" => FormattableString,
        archive_location: "archive-location" => FormattableString,
    );
}

/// Errors which could occur when reading a Yaml bibliography file.
#[derive(Clone, Error, Debug)]
pub enum YamlBibliographyError {
    /// The file is no valid Yaml.
    #[error("string could not be read as yaml")]
    Scan(#[from] yaml_rust::ScanError),
    /// The file does not contain a hash map at the top level.
    #[error("file has no top-level hash map")]
    Structure,
    /// An entry is not structured as a hash map.
    #[error("the entry with key `{0}` does not contain a hash map")]
    EntryStructure(String),
    /// A field name cannot be read as a string.
    #[error("a field name in the entry with key `{0}` cannot be read as a string")]
    FieldNameUnparsable(String),
    /// An entry key is not a string.
    #[error("a entry key cannot be parsed as a string")]
    KeyUnparsable,
    /// An entry field is not of the right data type.
    #[error(
        "wrong data type for field `{field}` in entry `{key}` (expected {expected:?})"
    )]
    DataTypeMismatch {
        /// Key of the offending entry.
        key: String,
        /// Name of the offending field.
        field: String,
        /// Expected data type.
        expected: String,
    },
    /// An error occured when trying to parse the data in an entry field
    /// to a given type.
    #[error("error when parsing data for field `{field}` in entry `{key}` ({source})")]
    DataType {
        /// Key of the offending entry.
        key: String,
        /// Name of the offending field.
        field: String,
        /// Error that occurred when parsing.
        #[source]
        source: YamlDataTypeError,
    },
}

/// Errors that can occur when reading a [FormattableString] from the yaml
/// bibliography file
#[derive(Clone, Error, Debug)]
pub enum YamlFormattableStringError {
    /// A key of a sub-field cannot be parsed as a string.
    #[error("key cannot be parsed as a string")]
    KeyIsNoString,
    /// A value could not be read as a string.
    #[error("value cannot be parsed as a string")]
    ValueIsNoString,
    /// Missing value.
    #[error("no value was found")]
    NoValue,
    /// The `verbatim` property must be `true` or `false`.
    #[error("the `verbatim` property must be boolean")]
    VerbatimNotBool,
}

/// An error that can occur for each of the parsable data types.
#[derive(Clone, Error, Debug)]
pub enum YamlDataTypeError {
    /// [Error](YamlFormattableStringError) with parsing a [FormattableString].
    #[error("formattable string structurally malformed")]
    FormattableString(#[from] YamlFormattableStringError),
    /// [Error](types::DateError) with parsing a [Date].
    #[error("date string structurally malformed")]
    Date(#[from] types::DateError),
    /// [Error](types::PersonError) with parsing a [Person].
    #[error("person string structurally malformed")]
    Person(#[from] types::PersonError),
    /// [Error](types::DurationError) with parsing a [Duration].
    #[error("duration string structurally malformed")]
    Duration(#[from] types::DurationError),
    /// [Error](url::ParseError) with parsing an [QualifiedUrl].
    #[error("invalid url")]
    Url(#[from] url::ParseError),
    /// A string does not represent a [std::ops::Range].
    #[error("string is not a range")]
    Range,
    /// An element of an array is unexpectedly empty.
    #[error("array element empty")]
    EmptyArrayElement,
    /// A required field is missing.
    #[error("missing required field in details hash map")]
    MissingRequiredField,
    /// Wrong primitive yaml type in entry field.
    #[error("mismatched primitive type")]
    MismatchedPrimitive,
}

impl YamlBibliographyError {
    fn new_data_type_error(key: &str, field: &str, expected: &str) -> Self {
        Self::DataTypeMismatch {
            key: key.to_string(),
            field: field.to_string(),
            expected: expected.to_string(),
        }
    }

    fn new_data_type_src_error(
        key: &str,
        field: &str,
        dtype_err: YamlDataTypeError,
    ) -> Self {
        Self::DataType {
            key: key.to_string(),
            field: field.to_string(),
            source: dtype_err,
        }
    }
}

/// This loads a string as a yaml bibliography file.
///
/// ```
/// use hayagriva::load_yaml_structure;
///
/// let yaml = r#"
/// crazy-rich:
///     type: Book
///     title: Crazy Rich Asians
///     author: Kwan, Kevin
///     date: 2014
///     publisher: Anchor Books
///     location: New York, NY, US
/// "#;
/// let bib = load_yaml_structure(yaml).unwrap();
/// assert_eq!(bib[0].get_date().unwrap().year, 2014);
/// ```
pub fn load_yaml_structure(file: &str) -> Result<Vec<Entry>, YamlBibliographyError> {
    let docs = YamlLoader::load_from_str(file)?;
    let doc = docs[0].clone().into_hash().ok_or(YamlBibliographyError::Structure)?;
    let mut entries = vec![];
    for (key, fields) in doc.into_iter() {
        let key = key.into_string().ok_or(YamlBibliographyError::KeyUnparsable)?;
        entries.push(entry_from_yaml(key, fields, EntryType::Misc)?);
    }

    Ok(entries)
}

fn yaml_hash_map_with_string_keys(
    map: LinkedHashMap<Yaml, Yaml>,
) -> LinkedHashMap<String, Yaml> {
    map.into_iter()
        .filter_map(|(k, v)| {
            if let Some(k) = k.into_string() {
                Some((k, v))
            } else {
                None
            }
        })
        .collect()
}

fn title_from_hash_map(
    map: LinkedHashMap<Yaml, Yaml>,
) -> Result<Title, YamlFormattableStringError> {
    let value = formattable_str_from_hash_map(map.clone())?;
    let map = yaml_hash_map_with_string_keys(map);

    let shorthand = if let Some(sh) = map.get("shorthand") {
        let sh = if let Some(s) = sh.as_str() {
            FormattableString::new_shorthand(s.into())
        } else if let Some(hm) = sh.clone().into_hash() {
            formattable_str_from_hash_map(hm)?
        } else {
            return Err(YamlFormattableStringError::NoValue);
        };

        Some(sh)
    } else {
        None
    };

    let translated = if let Some(tl) = map.get("translation") {
        let tl = if let Some(s) = tl.as_str() {
            FormattableString::new_shorthand(s.into())
        } else if let Some(hm) = tl.clone().into_hash() {
            formattable_str_from_hash_map(hm)?
        } else {
            return Err(YamlFormattableStringError::NoValue);
        };

        Some(tl)
    } else {
        None
    };

    Ok(Title { value, shorthand, translated })
}

fn formattable_str_from_hash_map(
    map: LinkedHashMap<Yaml, Yaml>,
) -> Result<FormattableString, YamlFormattableStringError> {
    let map = yaml_hash_map_with_string_keys(map);

    let fields = ["value", "sentence-case", "title-case"];
    let mut fields: Vec<String> = fields
        .iter()
        .filter_map(|&f| map.get(f).and_then(|v| v.clone().into_string()))
        .collect();

    if fields.is_empty() {
        return Err(YamlFormattableStringError::NoValue);
    }

    let value = fields.remove(0);
    let verbatim = if let Some(verbatim) = map.get("verbatim") {
        verbatim
            .as_bool()
            .ok_or(YamlFormattableStringError::VerbatimNotBool)?
    } else {
        false
    };

    let sentence_case = if let Some(sentence_case) = map.get("sentence-case") {
        Some(
            sentence_case
                .clone()
                .into_string()
                .ok_or(YamlFormattableStringError::ValueIsNoString)?,
        )
    } else {
        None
    };

    let title_case = if let Some(title_case) = map.get("title-case") {
        Some(
            title_case
                .clone()
                .into_string()
                .ok_or(YamlFormattableStringError::ValueIsNoString)?,
        )
    } else {
        None
    };

    Ok(FormattableString::new(
        value,
        title_case,
        sentence_case,
        verbatim,
    ))
}

fn person_from_yaml(
    item: Yaml,
    key: &str,
    field_name: &str,
) -> Result<Person, YamlBibliographyError> {
    if let Some(map) = item.clone().into_hash() {
        let mut map = yaml_hash_map_with_string_keys(map);
        let name = map.remove("name").and_then(|v| v.into_string()).ok_or_else(|| {
            YamlBibliographyError::new_data_type_src_error(
                key,
                field_name,
                YamlDataTypeError::MissingRequiredField,
            )
        })?;

        let optionals = ["given-name", "prefix", "suffix", "alias"];
        let mut values = vec![];

        for &field in optionals.iter() {
            values.push(map.remove(field).and_then(|v| v.into_string()));
        }

        Ok(Person {
            name,
            alias: values.pop().unwrap(),
            suffix: values.pop().unwrap(),
            prefix: values.pop().unwrap(),
            given_name: values.pop().unwrap(),
        })
    } else if let Some(s) = item.into_string() {
        Ok(
            Person::from_strings(&s.split(',').collect::<Vec<&str>>()).map_err(|e| {
                YamlBibliographyError::new_data_type_src_error(
                    key,
                    field_name,
                    YamlDataTypeError::Person(e),
                )
            })?,
        )
    } else {
        Err(YamlBibliographyError::new_data_type_error(
            key, field_name, "person",
        ))
    }
}

fn persons_from_yaml(
    value: Yaml,
    key: &str,
    field_name: &str,
) -> Result<Vec<Person>, YamlBibliographyError> {
    let mut persons = vec![];
    if value.is_array() {
        for item in value {
            persons.push(person_from_yaml(item, key, field_name)?);
        }
    } else {
        persons.push(person_from_yaml(value, key, field_name)?);
    }

    Ok(persons)
}

fn entry_from_yaml(
    key: String,
    yaml: Yaml,
    default_type: EntryType,
) -> Result<Entry, YamlBibliographyError> {
    let mut entry = Entry {
        key: key.clone(),
        content: HashMap::new(),
        entry_type: default_type,
    };
    for (field_name, value) in yaml
        .into_hash()
        .ok_or_else(|| YamlBibliographyError::EntryStructure(key.clone()))?
        .into_iter()
    {
        let field_name = field_name
            .into_string()
            .ok_or_else(|| YamlBibliographyError::FieldNameUnparsable(key.clone()))?;
        let fname_str = field_name.as_str();

        if fname_str == "type" {
            let val = value.into_string().ok_or_else(|| {
                YamlBibliographyError::new_data_type_src_error(
                    &key,
                    &field_name,
                    YamlDataTypeError::MismatchedPrimitive,
                )
            })?;

            if let Ok(tp) = EntryType::from_str(&val.to_lowercase()) {
                entry.entry_type = tp;
            }

            continue;
        }

        let value = match fname_str {
            "title" => match value {
                Yaml::Hash(map) => {
                    FieldType::Title(title_from_hash_map(map).map_err(|e| {
                        YamlBibliographyError::new_data_type_src_error(
                            &key,
                            &field_name,
                            YamlDataTypeError::FormattableString(e),
                        )
                    })?)
                }
                Yaml::String(t) => FieldType::Title(Title {
                    value: FormattableString::new_shorthand(t),
                    shorthand: None,
                    translated: None,
                }),
                _ => {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "text or formattable string or title",
                    ));
                }
            },
            "publisher" | "location" | "archive" | "archive-location" => match value {
                Yaml::Hash(map) => FieldType::FormattableString(
                    formattable_str_from_hash_map(map).map_err(|e| {
                        YamlBibliographyError::new_data_type_src_error(
                            &key,
                            &field_name,
                            YamlDataTypeError::FormattableString(e),
                        )
                    })?,
                ),
                Yaml::String(t) => {
                    FieldType::FormattableString(FormattableString::new_shorthand(t))
                }
                _ => {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "text or formattable string",
                    ));
                }
            },
            "author" | "editor" => {
                FieldType::Persons(persons_from_yaml(value, &key, &field_name)?)
            }
            "affiliated" => {
                let mut res = vec![];
                if !value.is_array() {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "affiliated person",
                    ));
                }

                for item in value {
                    let mut map = yaml_hash_map_with_string_keys(
                        item.into_hash().ok_or_else(|| {
                            YamlBibliographyError::new_data_type_error(
                                &key,
                                &field_name,
                                "affiliated person",
                            )
                        })?,
                    );

                    let persons = map
                        .remove("names")
                        .ok_or_else(|| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::MissingRequiredField,
                            )
                        })
                        .and_then(|value| persons_from_yaml(value, &key, &field_name))?;

                    let role = map
                        .remove("role")
                        .ok_or_else(|| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::MissingRequiredField,
                            )
                        })
                        .and_then(|t| {
                            t.into_string().ok_or_else(|| {
                                YamlBibliographyError::new_data_type_src_error(
                                    &key,
                                    &field_name,
                                    YamlDataTypeError::MismatchedPrimitive,
                                )
                            })
                        })?;

                    let role = PersonRole::from_str(&role.to_lowercase())
                        .unwrap_or_else(|_| PersonRole::Unknown(role));

                    res.push((persons, role))
                }

                FieldType::PersonsWithRoles(res)
            }
            "date" => FieldType::Date(match value {
                Yaml::Integer(value) => Date::from_year(value as i32),
                Yaml::String(value) => Date::from_str(&value).map_err(|e| {
                    YamlBibliographyError::new_data_type_src_error(
                        &key,
                        &field_name,
                        YamlDataTypeError::Date(e),
                    )
                })?,
                _ => {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "date",
                    ));
                }
            }),
            "issue" | "edition" => {
                let as_int = value.as_i64();
                let as_str = if as_int == None { value.into_string() } else { None };

                if let Some(i) = as_int {
                    FieldType::IntegerOrText(NumOrStr::Number(i))
                } else if let Some(t) = as_str {
                    FieldType::IntegerOrText(NumOrStr::Str(t))
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer or text",
                    ));
                }
            }
            "volume-total" | "page-total" => {
                FieldType::Integer(value.into_i64().ok_or_else(|| {
                    YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer",
                    )
                })?)
            }
            "volume" | "page-range" => FieldType::Range(match value {
                Yaml::Integer(value) => (value .. value),
                Yaml::String(value) => get_range(&value).ok_or_else(|| {
                    YamlBibliographyError::new_data_type_src_error(
                        &key,
                        &field_name,
                        YamlDataTypeError::Range,
                    )
                })?,
                _ => {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer range",
                    ));
                }
            }),
            "runtime" => {
                let v = value
                    .into_string()
                    .ok_or_else(|| {
                        YamlBibliographyError::new_data_type_error(
                            &key,
                            &field_name,
                            "duration",
                        )
                    })
                    .and_then(|s| {
                        Duration::from_str(&s).map_err(|e| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::Duration(e),
                            )
                        })
                    })?;

                FieldType::Duration(v)
            }
            "time-range" => {
                let v = value
                    .into_string()
                    .ok_or_else(|| {
                        YamlBibliographyError::new_data_type_error(
                            &key,
                            &field_name,
                            "duration",
                        )
                    })
                    .and_then(|s| {
                        Duration::range_from_str(&s).map_err(|e| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::Duration(e),
                            )
                        })
                    })?;

                FieldType::TimeRange(v)
            }
            "url" => {
                let (url, date) = match value {
                    Yaml::String(s) => (
                        Url::parse(&s).map_err(|e| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::Url(e),
                            )
                        })?,
                        None,
                    ),
                    Yaml::Hash(map) => {
                        let mut map = yaml_hash_map_with_string_keys(map);
                        let url = map
                            .remove("value")
                            .ok_or_else(|| {
                                YamlBibliographyError::new_data_type_src_error(
                                    &key,
                                    &field_name,
                                    YamlDataTypeError::MissingRequiredField,
                                )
                            })
                            .and_then(|value| {
                                value
                                    .into_string()
                                    .ok_or_else(|| {
                                        YamlBibliographyError::new_data_type_src_error(
                                            &key,
                                            &field_name,
                                            YamlDataTypeError::MismatchedPrimitive,
                                        )
                                    })
                                    .and_then(|s| {
                                        Url::parse(&s).map_err(|e| {
                                            YamlBibliographyError::new_data_type_src_error(
                                                &key,
                                                &field_name,
                                                YamlDataTypeError::Url(e),
                                            )
                                        })
                                    })
                            })?;

                        let date = if let Some(date) = map.remove("date") {
                            if let Some(year) = date.as_i64() {
                                Some(Date::from_year(year as i32))
                            } else if let Some(s) = date.into_string() {
                                Some(Date::from_str(&s).map_err(|e| {
                                    YamlBibliographyError::new_data_type_src_error(
                                        &key,
                                        &field_name,
                                        YamlDataTypeError::Date(e),
                                    )
                                })?)
                            } else {
                                return Err(
                                    YamlBibliographyError::new_data_type_src_error(
                                        &key,
                                        &field_name,
                                        YamlDataTypeError::MismatchedPrimitive,
                                    ),
                                );
                            }
                        } else {
                            None
                        };

                        (url, date)
                    }
                    _ => {
                        return Err(YamlBibliographyError::new_data_type_error(
                            &key,
                            &field_name,
                            "qualified url",
                        ));
                    }
                };

                FieldType::Url(QualifiedUrl { value: url, visit_date: date })
            }
            "language" => FieldType::Language(
                value.into_string().and_then(|f| f.parse().ok()).ok_or_else(|| {
                    YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "unicode language identifier",
                    )
                })?,
            ),
            "parent" => {
                if value.is_array() {
                    let mut entries = vec![];

                    for item in value {
                        entries.push(entry_from_yaml(
                            key.clone(),
                            item,
                            entry.entry_type.default_parent(),
                        )?)
                    }

                    FieldType::Entries(entries)
                } else {
                    FieldType::Entries(vec![entry_from_yaml(
                        key.clone(),
                        value,
                        entry.entry_type.default_parent(),
                    )?])
                }
            }
            _ => {
                if let Some(t) = value.clone().into_string() {
                    FieldType::Text(t)
                } else if let Some(i) = value.as_i64() {
                    FieldType::Text(i.to_string())
                } else if let Some(i) = value.as_f64() {
                    FieldType::Text(i.to_string())
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "text",
                    ));
                }
            }
        };

        entry.content.insert(field_name, value);
    }

    Ok(entry)
}

#[cfg(test)]
mod tests {
    use super::load_yaml_structure;
    use crate::output::{apa, chicago, ieee, mla, AtomicCitation, BibliographyFormatter};
    use crate::selectors::parse;
    use std::fs;

    #[test]
    fn apa() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let apa = apa::ApaBibliographyFormatter::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = apa.get_reference(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn ieee() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let ieee = ieee::IeeeBibliographyFormatter::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = ieee.get_reference(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn mla() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let mla = mla::MlaBibliographyFormatter::new();
        let mut last_entry = None;

        for entry in &entries {
            let refs = mla.get_reference(&entry, last_entry);
            println!("{}", refs.print_ansi_vt100());
            last_entry = Some(entry);
        }
    }

    #[test]
    fn chicago_n() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let chicago = chicago::nb::notes::NoteCitationFormatter::new(entries.iter());

        for entry in &entries {
            let citation = AtomicCitation::new(&entry.key, None, None);

            let refs = chicago
                .get_note(citation, chicago::nb::notes::NoteType::Full)
                .unwrap();
            println!("{}", refs.print_ansi_vt100());
        }
    }

    #[test]
    fn chicago_b() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let chicago = chicago::nb::bibliography::BibliographyFormatter::new();

        for entry in &entries {
            let refs = chicago.format(&entry);
            println!("{}", refs.print_ansi_vt100());
        }
    }

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
            let expr = parse($select).unwrap();
            for entry in &$entries {
                let res = expr.apply(entry);
                if keys.contains(&entry.key.as_str()) {
                    if res.is_none() {
                        panic!("Key {} not found in results", entry.key);
                    }
                } else {
                    if res.is_some() {
                        panic!("Key {} found in results", entry.key);
                    }
                }
            }
        }
    }

    macro_rules! select {
        ($select:expr, $entries:tt >> $entry_key:expr, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
            let entry = $entries.iter().filter_map(|i| if i.key == $entry_key {Some(i)} else {None}).next().unwrap();
            let expr = parse($select).unwrap();
            let res = expr.apply(entry).unwrap();
            if !keys.into_iter().all(|k| res.get(k).is_some()) {
                panic!("Results do not contain binding");
            }
        }
    }

    #[test]
    fn selectors() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();

        select_all!("Article > Proceedings", entries, ["zygos"]);
        select_all!("Article > (Periodical | Newspaper)", entries, [
            "omarova-libra",
            "kinetics",
            "house",
            "swedish",
        ]);
        select_all!("(Chapter | Anthos) > (Anthology | Book)", entries, [
            "harry", "gedanken"
        ]);
        select_all!("*[url]", entries, [
            "omarova-libra",
            "science-e-issue",
            "oiseau",
            "georgia",
            "really-habitable",
            "electronic-music",
            "mattermost",
            "worth",
            "wrong",
            "un-hdr",
            "audio-descriptions",
            "camb",
            "logician",
            "dns-encryption",
        ]);
        select_all!("!(*[url] | (* > *[url]))", entries, [
            "zygos",
            "harry",
            "terminator-2",
            "interior",
            "wire",
            "kinetics",
            "house",
            "plaque",
            "renaissance",
            "gedanken",
            "donne",
            "roe-wade",
            "foia",
            "drill",
            "swedish",
        ]);
    }

    #[test]
    fn selector_bindings() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();

        select!(
            "a:Article > (b:Conference & c:(Video|Blog|Web))",
            entries >> "wwdc-network",
            ["a", "b", "c"]
        );
    }
}
