//! Reading and writing YAML bibliographies.

use std::collections::HashMap;
use std::str::FromStr;

use linked_hash_map::LinkedHashMap;
use thiserror::Error;
use unic_langid::LanguageIdentifier;
use url::Url;
use yaml_rust::{Yaml, YamlEmitter, YamlLoader};

use crate::lang::{Case, SentenceCase};
use crate::types::{
    get_range, Date, DateError, Duration, DurationError, EntryType, FmtString, NumOrStr,
    Person, PersonError, PersonRole, QualifiedUrl, Title,
};
use crate::{Entry, Value};

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
    #[error("the entry with key `{key}` does not contain a hash map")]
    EntryStructure {
        /// Key of the offending entry.
        key: String,
    },
    /// A field name cannot be read as a string.
    #[error("a field name in the entry with key `{key}` cannot be read as a string")]
    FieldNameNoStr {
        /// Key of the offending entry.
        key: String,
    },
    /// An entry key is not a string.
    #[error("a entry key cannot be parsed as a string")]
    KeyNoStr,
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

/// Errors that can occur when reading a [FmtString] from the yaml
/// bibliography file
#[derive(Clone, Error, Debug)]
pub enum YamlFmtStringError {
    /// A key of a sub-field cannot be parsed as a string.
    #[error("key cannot be parsed as a string")]
    KeyNoString,
    /// Missing value.
    #[error("no value was found")]
    NoValue,
    /// A value could not be read as a string.
    #[error("value cannot be parsed as a string")]
    ValueNoString,
    /// The `verbatim` property must be `true` or `false`.
    #[error("the `verbatim` property must be boolean")]
    VerbatimNoBool,
}

/// An error that can occur for each of the parsable data types.
#[derive(Clone, Error, Debug)]
pub enum YamlDataTypeError {
    /// Error when parsing a [FmtString].
    #[error("formattable string structurally malformed")]
    FmtString(#[from] YamlFmtStringError),
    /// Error when parsing a [Date].
    #[error("date string structurally malformed")]
    Date(#[from] DateError),
    /// Error when parsing a [Person].
    #[error("person string structurally malformed")]
    Person(#[from] PersonError),
    /// Error when parsing a [Duration].
    #[error("duration string structurally malformed")]
    Duration(#[from] DurationError),
    /// Error when parsing an [QualifiedUrl].
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

/// Parse a bibliography from a YAML string.
///
/// ```
/// use hayagriva::io::from_yaml_str;
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
/// let bib = from_yaml_str(yaml).unwrap();
/// assert_eq!(bib[0].date().unwrap().year, 2014);
/// ```
pub fn from_yaml_str(string: &str) -> Result<Vec<Entry>, YamlBibliographyError> {
    let yaml = YamlLoader::load_from_str(string)?
        .into_iter()
        .next()
        .ok_or(YamlBibliographyError::Structure)?;
    from_yaml(yaml)
}

/// Parse a bibliography from a loaded YAML file.
pub fn from_yaml(yaml: Yaml) -> Result<Vec<Entry>, YamlBibliographyError> {
    let doc = yaml.into_hash().ok_or(YamlBibliographyError::Structure)?;
    let mut entries = vec![];
    for (key, fields) in doc.into_iter() {
        let key = key.into_string().ok_or(YamlBibliographyError::KeyNoStr)?;
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
) -> Result<Title, YamlFmtStringError> {
    let canonical = fmt_str_from_hash_map(map.clone())?;
    let map = yaml_hash_map_with_string_keys(map);

    let shorthand = if let Some(sh) = map.get("shorthand") {
        let sh = if let Some(s) = sh.as_str() {
            FmtString::new(s)
        } else if let Some(hm) = sh.clone().into_hash() {
            fmt_str_from_hash_map(hm)?
        } else {
            return Err(YamlFmtStringError::NoValue);
        };

        Some(sh)
    } else {
        None
    };

    let translated = if let Some(tl) = map.get("translation") {
        let tl = if let Some(s) = tl.as_str() {
            FmtString::new(s)
        } else if let Some(hm) = tl.clone().into_hash() {
            fmt_str_from_hash_map(hm)?
        } else {
            return Err(YamlFmtStringError::NoValue);
        };

        Some(tl)
    } else {
        None
    };

    Ok(Title { canonical, shorthand, translated })
}

fn fmt_str_from_hash_map(
    map: LinkedHashMap<Yaml, Yaml>,
) -> Result<FmtString, YamlFmtStringError> {
    let map = yaml_hash_map_with_string_keys(map);

    let fields = ["value", "sentence-case", "title-case"];
    let mut fields: Vec<String> = fields
        .iter()
        .filter_map(|&f| map.get(f).and_then(|v| v.clone().into_string()))
        .collect();

    if fields.is_empty() {
        return Err(YamlFmtStringError::NoValue);
    }

    let value = fields.remove(0);
    let mut fmt = FmtString::new(value);

    if let Some(verbatim) = map.get("verbatim") {
        fmt = fmt.verbatim(verbatim.as_bool().ok_or(YamlFmtStringError::VerbatimNoBool)?);
    }

    if let Some(sentence_case) = map.get("sentence-case") {
        fmt = fmt.sentence_case(
            sentence_case
                .clone()
                .into_string()
                .ok_or(YamlFmtStringError::ValueNoString)?,
        );
    }

    if let Some(title_case) = map.get("title-case") {
        fmt = fmt.title_case(
            title_case
                .clone()
                .into_string()
                .ok_or(YamlFmtStringError::ValueNoString)?,
        );
    }

    Ok(fmt)
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

fn affiliated_from_yaml(
    item: Yaml,
    key: &str,
    field_name: &str,
) -> Result<(Vec<Person>, PersonRole), YamlBibliographyError> {
    let mut map = yaml_hash_map_with_string_keys(item.into_hash().ok_or_else(|| {
        YamlBibliographyError::new_data_type_error(&key, &field_name, "affiliated person")
    })?);

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

    Ok((persons, role))
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
    for (field_name, yaml) in yaml
        .into_hash()
        .ok_or_else(|| YamlBibliographyError::EntryStructure { key: key.clone() })?
        .into_iter()
    {
        let field_name = field_name
            .into_string()
            .ok_or_else(|| YamlBibliographyError::FieldNameNoStr { key: key.clone() })?;
        let fname_str = field_name.as_str();

        if fname_str == "type" {
            let val = yaml.into_string().ok_or_else(|| {
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
            "title" => match yaml {
                Yaml::Hash(map) => {
                    Value::Title(title_from_hash_map(map).map_err(|e| {
                        YamlBibliographyError::new_data_type_src_error(
                            &key,
                            &field_name,
                            YamlDataTypeError::FmtString(e),
                        )
                    })?)
                }
                Yaml::String(t) => Value::Title(Title {
                    canonical: FmtString::new(t),
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
            "publisher" | "location" | "archive" | "archive-location" => match yaml {
                Yaml::Hash(map) => {
                    Value::FmtString(fmt_str_from_hash_map(map).map_err(|e| {
                        YamlBibliographyError::new_data_type_src_error(
                            &key,
                            &field_name,
                            YamlDataTypeError::FmtString(e),
                        )
                    })?)
                }
                Yaml::String(t) => Value::FmtString(FmtString::new(t)),
                _ => {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "text or formattable string",
                    ));
                }
            },
            "author" | "editor" => {
                Value::Persons(persons_from_yaml(yaml, &key, &field_name)?)
            }
            "affiliated" => {
                let mut res = vec![];
                if yaml.is_array() {
                    for item in yaml {
                        res.push(affiliated_from_yaml(item, &key, &field_name)?);
                    }
                } else if let Yaml::Hash(item) = yaml {
                    res.push(affiliated_from_yaml(Yaml::Hash(item), &key, &field_name)?);
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "affiliated person",
                    ));
                }

                Value::PersonsWithRoles(res)
            }
            "date" => Value::Date(match yaml {
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
                let as_int = yaml.as_i64();
                let as_str = if as_int == None { yaml.into_string() } else { None };

                if let Some(i) = as_int {
                    Value::IntegerOrText(NumOrStr::Number(i))
                } else if let Some(t) = as_str {
                    Value::IntegerOrText(NumOrStr::Str(t))
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer or text",
                    ));
                }
            }
            "volume-total" | "page-total" => {
                Value::Integer(yaml.into_i64().ok_or_else(|| {
                    YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer",
                    )
                })?)
            }
            "volume" | "page-range" => Value::Range(match yaml {
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
                let v = yaml
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

                Value::Duration(v)
            }
            "time-range" => {
                let v = yaml
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

                Value::TimeRange(v)
            }
            "url" => {
                let (url, date) = match yaml {
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

                Value::Url(QualifiedUrl { value: url, visit_date: date })
            }
            "language" => Value::Language(
                yaml.into_string().and_then(|f| f.parse().ok()).ok_or_else(|| {
                    YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "unicode language identifier",
                    )
                })?,
            ),
            "parent" => {
                if yaml.is_array() {
                    let mut entries = vec![];

                    for item in yaml {
                        entries.push(entry_from_yaml(
                            key.clone(),
                            item,
                            entry.entry_type.default_parent(),
                        )?)
                    }

                    Value::Entries(entries)
                } else {
                    Value::Entries(vec![entry_from_yaml(
                        key.clone(),
                        yaml,
                        entry.entry_type.default_parent(),
                    )?])
                }
            }
            _ => {
                if let Some(t) = yaml.clone().into_string() {
                    Value::Text(t)
                } else if let Some(i) = yaml.as_i64() {
                    Value::Text(i.to_string())
                } else if let Some(i) = yaml.as_f64() {
                    Value::Text(i.to_string())
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

impl Into<Yaml> for Date {
    fn into(self) -> Yaml {
        let s = match (self.month.is_some(), self.day.is_some()) {
            (true, true) => format!(
                "{:04}-{:02}-{:02}",
                self.year,
                self.month.unwrap() + 1,
                self.day.unwrap() + 1
            ),
            (true, false) => format!("{:04}-{:02}", self.year, self.month.unwrap() + 1),
            (false, _) => return Yaml::Integer(self.year as i64),
        };

        Yaml::String(s)
    }
}

impl Into<Yaml> for Duration {
    fn into(self) -> Yaml {
        if self.days == 0 {
            if self.hours == 0 {
                if self.milliseconds == 0.0 {
                    Yaml::String(format!("{:02}:{:02}", self.minutes, self.seconds))
                } else {
                    Yaml::String(format!(
                        "{:02}:{:02},{:03}",
                        self.minutes, self.seconds, self.milliseconds
                    ))
                }
            } else {
                Yaml::String(format!(
                    "{:02}:{:02}:{:02},{:03}",
                    self.hours, self.minutes, self.seconds, self.milliseconds
                ))
            }
        } else {
            if self.milliseconds == 0.0 {
                Yaml::String(format!(
                    "{}:{:02}:{:02}:{:02}",
                    self.days, self.hours, self.minutes, self.seconds
                ))
            } else {
                Yaml::String(format!(
                    "{}:{:02}:{:02}:{:02},{:03}",
                    self.days, self.hours, self.minutes, self.seconds, self.milliseconds
                ))
            }
        }
    }
}

impl Into<Yaml> for EntryType {
    fn into(self) -> Yaml {
        Yaml::String(self.to_string())
    }
}

impl Into<Yaml> for FmtString {
    fn into(self) -> Yaml {
        if !self.verbatim && self.title_case.is_none() && self.sentence_case.is_none() {
            Yaml::String(self.value)
        } else {
            let mut hm = LinkedHashMap::new();

            let bt_equal = if let Some(sentence) = self.sentence_case.as_ref() {
                self.title_case.is_none()
                    && &SentenceCase::new().apply(&self.value) == sentence
            } else {
                false
            };

            let same = self.title_case.as_ref().map_or(true, |case| case == &self.value)
                && self.sentence_case.as_ref().map_or(true, |case| case == &self.value);

            hm.insert(Yaml::String("value".into()), Yaml::String(self.value));

            if (self.verbatim || same) && !bt_equal {
                hm.insert(Yaml::String("verbatim".into()), Yaml::Boolean(true));
            }

            if !same && !bt_equal {
                if let Some(title) = self.title_case {
                    hm.insert(Yaml::String("title-case".into()), Yaml::String(title));
                }
                if let Some(sentence) = self.sentence_case {
                    hm.insert(
                        Yaml::String("sentence-case".into()),
                        Yaml::String(sentence),
                    );
                }
            }

            if hm.len() == 1 {
                hm.remove(&Yaml::String("value".into())).unwrap()
            } else {
                Yaml::Hash(hm)
            }
        }
    }
}

impl Into<Yaml> for NumOrStr {
    fn into(self) -> Yaml {
        match self {
            NumOrStr::Number(i) => Yaml::Integer(i),
            NumOrStr::Str(s) => Yaml::String(s),
        }
    }
}

impl Into<Yaml> for Person {
    fn into(self) -> Yaml {
        if let Some(alias) = self.alias {
            let mut hm = LinkedHashMap::new();

            hm.insert(Yaml::String("name".into()), Yaml::String(self.name));
            if let Some(given_name) = self.given_name {
                hm.insert(Yaml::String("given-name".into()), Yaml::String(given_name));
            }
            if let Some(prefix) = self.prefix {
                hm.insert(Yaml::String("prefix".into()), Yaml::String(prefix));
            }
            if let Some(suffix) = self.suffix {
                hm.insert(Yaml::String("suffix".into()), Yaml::String(suffix));
            }
            hm.insert(Yaml::String("alias".into()), Yaml::String(alias));

            Yaml::Hash(hm)
        } else {
            let mut res = if let Some(prefix) = self.prefix {
                format!("{} {}", prefix, self.name)
            } else {
                self.name
            };

            if let Some(gn) = self.given_name {
                res += ", ";
                res += &gn;

                if let Some(suffix) = self.suffix {
                    res += ", ";
                    res += &suffix;
                }
            }

            Yaml::String(res)
        }
    }
}

impl Into<Yaml> for PersonRole {
    fn into(self) -> Yaml {
        Yaml::String(self.to_string())
    }
}

impl Into<Yaml> for QualifiedUrl {
    fn into(self) -> Yaml {
        if let Some(date) = self.visit_date {
            let mut hm = LinkedHashMap::new();
            hm.insert(
                Yaml::String("value".into()),
                Yaml::String(self.value.to_string()),
            );
            hm.insert(Yaml::String("date".into()), date.into());
            Yaml::Hash(hm)
        } else {
            Yaml::String(self.value.to_string())
        }
    }
}

impl Into<Yaml> for Title {
    fn into(self) -> Yaml {
        if self.translated.is_none() && self.shorthand.is_none() {
            self.canonical.into()
        } else {
            let mut map = match self.canonical.into() {
                Yaml::Hash(map) => map,
                Yaml::String(s) => {
                    let mut map = LinkedHashMap::new();
                    map.insert(Yaml::String("value".into()), Yaml::String(s));
                    map
                }
                _ => unreachable!(),
            };

            if let Some(translated) = self.translated {
                map.insert(Yaml::String("translation".into()), translated.into());
            }

            if let Some(shorthand) = self.shorthand {
                map.insert(Yaml::String("shorthand".into()), shorthand.into());
            }

            Yaml::Hash(map)
        }
    }
}

fn range_into_yaml(range: &std::ops::Range<i64>) -> Yaml {
    if range.start != range.end {
        Yaml::String(format!("{}-{}", range.start, range.end))
    } else {
        Yaml::Integer(range.start)
    }
}

fn affiliated_into_yaml(affiliated: (Vec<Person>, PersonRole)) -> Yaml {
    let persons = Yaml::Array(affiliated.0.into_iter().map(|p| p.into()).collect());
    let mut hm = LinkedHashMap::new();
    hm.insert(Yaml::String("names".into()), persons);
    hm.insert(Yaml::String("role".into()), affiliated.1.into());
    Yaml::Hash(hm)
}

fn time_range_into_yaml(range: std::ops::Range<Duration>) -> Yaml {
    if range.start != range.end {
        let start = if let Yaml::String(s) = range.start.into() {
            s
        } else {
            unreachable!()
        };
        let end = if let Yaml::String(s) = range.end.into() {
            s
        } else {
            unreachable!()
        };
        Yaml::String(format!("{}-{}", start, end))
    } else {
        range.start.into()
    }
}

fn language_into_yaml(lang: LanguageIdentifier) -> Yaml {
    Yaml::String(lang.to_string())
}

fn persons_into_yaml(pers: Vec<Person>) -> Yaml {
    let mut persons: Vec<_> = pers.into_iter().map(|p| p.into()).collect();
    match persons.len() {
        1 => persons.pop().unwrap(),
        _ => Yaml::Array(persons),
    }
}

fn affiliateds_into_yaml(pers: Vec<(Vec<Person>, PersonRole)>) -> Yaml {
    let mut persons: Vec<_> = pers.into_iter().map(|p| affiliated_into_yaml(p)).collect();
    match persons.len() {
        1 => persons.pop().unwrap(),
        _ => Yaml::Array(persons),
    }
}

impl Into<Yaml> for Entry {
    fn into(self) -> Yaml {
        let mut hm = LinkedHashMap::new();
        hm.insert(Yaml::String("type".into()), self.kind().clone().into());

        for (s, item) in self.content {
            let content = match item {
                Value::Title(i) => i.into(),
                Value::FmtString(i) => i.into(),
                Value::Text(i) => Yaml::String(i),
                Value::Integer(i) => Yaml::Integer(i),
                Value::Date(i) => i.into(),
                Value::Persons(i) => persons_into_yaml(i),
                Value::PersonsWithRoles(i) => affiliateds_into_yaml(i),
                Value::IntegerOrText(i) => i.into(),
                Value::Range(i) => range_into_yaml(&i),
                Value::Duration(i) => i.into(),
                Value::TimeRange(i) => time_range_into_yaml(i),
                Value::Url(i) => i.into(),
                Value::Language(i) => language_into_yaml(i),
                Value::Entries(i) => {
                    Yaml::Array(i.into_iter().map(|e| e.into()).collect())
                }
            };

            hm.insert(Yaml::String(s), content);
        }

        Yaml::Hash(hm)
    }
}

/// Serialize a bibliography into the YAML format.
pub fn to_yaml(entries: impl IntoIterator<Item = Entry>) -> Yaml {
    let mut items = LinkedHashMap::new();
    for entry in entries {
        items.insert(Yaml::String(entry.key().into()), entry.into());
    }
    Yaml::Hash(items)
}

/// Serialize a bibliography into a YAML string.
pub fn to_yaml_str(entries: impl IntoIterator<Item = Entry>) -> Option<String> {
    let mut out_str = String::new();
    let mut emitter = YamlEmitter::new(&mut out_str);
    emitter.dump(&to_yaml(entries)).ok()?;
    Some(out_str)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn person_initials() {
        let contents = fs::read_to_string("tests/basic.yml").unwrap();
        let entries = from_yaml_str(&contents).unwrap();
        let yaml = to_yaml_str(entries.clone()).unwrap();
        let reconstructed = from_yaml_str(&yaml).unwrap();
        assert_eq!(entries.len(), reconstructed.len());

        for entry in entries {
            let match_e = reconstructed.iter().find(|x| x.key() == entry.key()).unwrap();
            assert_eq!(match_e, &entry);
        }
    }
}
