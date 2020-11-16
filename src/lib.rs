use std::collections::HashMap;
use std::str::FromStr;

pub mod lang;
pub mod output;
pub mod selectors;
pub mod types;

use lang::CaseTransformer;
use types::{
    get_range, Date, Duration, EntryType, EntryTypeSpec, FormattableString,
    FormattedString, NumOrStr, Person, PersonRole, QualifiedUrl,
};

use linked_hash_map::LinkedHashMap;
use paste::paste;
use std::convert::TryFrom;
use thiserror::Error;
use unic_langid::LanguageIdentifier;
use url::Url;
use yaml_rust::{Yaml, YamlLoader};

#[derive(Clone, Debug, PartialEq)]
pub enum FieldTypes {
    FormattableString(FormattableString),
    FormattedString(FormattedString),
    Text(String),
    Integer(i64),
    Date(Date),
    Persons(Vec<Person>),
    PersonsWithRoles(Vec<(Vec<Person>, PersonRole)>),
    IntegerOrText(NumOrStr),
    Range(std::ops::Range<i64>),
    Duration(Duration),
    TimeRange(std::ops::Range<Duration>),
    Url(QualifiedUrl),
    Language(LanguageIdentifier),
    Entries(Vec<Entry>),
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub struct Entry {
    key: String,
    entry_type: EntryType,
    content: HashMap<String, FieldTypes>,
}

impl Entry {
    pub fn new(key: &str, entry_type: EntryType) -> Self {
        Self {
            key: key.to_string(),
            entry_type,
            content: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&FieldTypes> {
        self.content.get(key)
    }

    pub fn set(&mut self, key: String, value: FieldTypes) {
        self.content.insert(key, value);
    }
}

#[derive(Clone, Error, Debug)]
pub enum EntryAccessError {
    #[error("the queried field is not present")]
    NoSuchField,
    #[error("datatype mismatch in the queried field")]
    WrongType,
}

macro_rules! fields {
    ($($name:ident: $field_name:expr => &FormattableString),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field as it stands (no formatting applied)."]
                pub fn [<get_ $name>](&self) -> Result<&FormattableString, EntryAccessError> {
                    self.get($field_name)
                        .ok_or(EntryAccessError::NoSuchField)
                        .and_then(|item| <&FormattableString>::try_from(item))
                }

                #[doc = "Get, parse, and format the `" $field_name "` field."]
                pub fn [<get_ $name _fmt>](
                    &self,
                    title: Option<&dyn CaseTransformer>,
                    sentence: Option<&dyn CaseTransformer>,
                ) -> Result<FormattedString, EntryAccessError> {
                    self.get($field_name)
                        .ok_or(EntryAccessError::NoSuchField)
                        .and_then(|item| <FormattableString>::try_from(item.clone()))
                        .map(|fstr| fstr.format(title, sentence))
                }

                fields!(single_set $name => $field_name, FormattableString);
            }
        )*
    };

    ($($name:ident: $field_name:expr $(=> $res:ty)?),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn [<get_ $name>](&self) -> Result<fields!(@type ref $($res)?), EntryAccessError> {
                    self.get($field_name)
                        .ok_or(EntryAccessError::NoSuchField)
                        .and_then(|item| <fields!(@type ref $($res)?)>::try_from(item))
                }

                fields!(single_set $name => $field_name, fields!(@type $($res)?));
            }
        )*
    };

    ($($name:ident: $field_name:expr => $res:ty, $res_ref:ty),* $(,)*) => {
        $(
            paste! {
                #[doc = "Get and parse the `" $field_name "` field."]
                pub fn [<get_ $name>](&self) -> Result<$res_ref, EntryAccessError> {
                    self.get($field_name)
                        .ok_or(EntryAccessError::NoSuchField)
                        .and_then(|item| <$res_ref>::try_from(item))
                }

                fields!(single_set $name => $field_name, $res);
            }
        )*
    };

    (single_set $name:ident => $field_name:expr, $other_type:ty) => {
        paste! {
            #[doc = "Set a value in the `" $field_name "` field."]
            pub fn [<set_ $name>](&mut self, item: $other_type) {
                self.set($field_name.to_string(), FieldTypes::from(item));
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

    fields!(title: "title" => &FormattableString);

    /// Get and parse the `author` field. This will always return a result
    pub fn get_authors(&self) -> &[Person] {
        self.get("author")
            .and_then(|item| <&[Person]>::try_from(item).ok())
            .unwrap_or_default()
    }

    /// Get and parse the `author` field.
    /// This will fail if there are no authors.
    pub fn get_authors_fallible(&self) -> Option<&[Person]> {
        self.get("author").and_then(|item| <&[Person]>::try_from(item).ok())
    }

    fields!(single_set authors => "author", Vec<Person>);

    fields!(date: "date" => Date);
    fields!(
        editors: "editor" => Vec<Person>, &[Person],
        affiliated_persons: "affiliated" => Vec<(Vec<Person>, PersonRole)>, &[(Vec<Person>, PersonRole)]
    );
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
        self.get_date().ok().or_else(|| {
            self.get_parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.get_any_date())
                .next()
        })
    }

    /// Will recursively get an url off either the entry or its parents.
    pub fn get_any_url(&self) -> Option<&QualifiedUrl> {
        self.get_url().ok().or_else(|| {
            self.get_parents()
                .into_iter()
                .flat_map(|v| v)
                .filter_map(|p| p.get_any_url())
                .next()
        })
    }

    /// Get and parse the `page-total` field, falling back on
    /// `page-range` if not specified.
    pub fn get_page_total(&self) -> Result<i64, EntryAccessError> {
        self.get("page-total")
            .ok_or(EntryAccessError::NoSuchField)
            .map(|ft| ft.clone())
            .or_else(|_| self.get_page_range().map(|r| FieldTypes::from(r.end - r.start)))
            .and_then(|item| i64::try_from(item.clone()))
    }

    fields!(single_set total_pages => "page-total", i64);
    fields!(time_range: "time-range" => std::ops::Range<Duration>);

    /// Get and parse the `runtime` field, falling back on
    /// `time-range` if not specified.
    pub fn get_runtime(&self) -> Result<Duration, EntryAccessError> {
        self.get("runtime")
            .ok_or(EntryAccessError::NoSuchField)
            .map(|ft| ft.clone())
            .or_else(|_| {
                self.get_time_range()
                    .map(|r| FieldTypes::from(r.end.clone() - r.start.clone()))
            })
            .and_then(|item| Duration::try_from(item.clone()))
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

    /// Recursively checks if `EntryTypeSpec` is applicable.
    /// Will return list of matching parent indices if spec applies.
    pub(crate) fn check_with_spec(
        &self,
        constraint: EntryTypeSpec,
    ) -> Result<Vec<usize>, ()> {
        if !self.entry_type.check(constraint.here) {
            return Err(());
        }

        let parents = self.get_parents().unwrap_or_default();
        let mut matching_parents = vec![];

        for pc in &constraint.parents {
            let mut found = false;

            for (index, p) in parents.iter().enumerate() {
                if p.check_with_spec(pc.clone()).is_ok() {
                    matching_parents.push(index);
                    found = true;
                    break;
                }
            }

            if !found {
                return Err(());
            }
        }

        Ok(matching_parents)
    }
}

#[derive(Clone, Error, Debug)]
pub enum YamlBibliographyError {
    #[error("string could not be read as yaml")]
    Scan(#[from] yaml_rust::ScanError),
    #[error("file has no top-level hash map")]
    Structure,
    #[error("the entry with key `{0}` does not contain a hash map")]
    EntryStructure(String),
    #[error("a field name in the entry with key `{0}` cannot be read as a string")]
    FieldNameUnparsable(String),
    #[error("a entry key cannot be parsed as a string")]
    KeyUnparsable,
    #[error(
        "wrong data type for field `{field}` in entry `{key}` (expected {expected:?})"
    )]
    DataTypeMismatch {
        key: String,
        field: String,
        expected: String,
    },
    #[error("error when parsing data for field `{field}` in entry `{key}` ({source})")]
    DataType {
        key: String,
        field: String,
        #[source]
        source: YamlDataTypeError,
    },
}

#[derive(Clone, Error, Debug)]
pub enum YamlFormattableStringError {
    #[error("key cannot be parsed as a string")]
    KeyIsNoString,
    #[error("value cannot be parsed as a string")]
    ValueIsNoString,
    #[error("no value was found")]
    NoValue,
    #[error("the `verbatim` property must be boolean")]
    VerbatimNotBool,
}

#[derive(Clone, Error, Debug)]
pub enum YamlDataTypeError {
    #[error("formattable string structurally malformed")]
    FormattableString(#[from] YamlFormattableStringError),
    #[error("date string structurally malformed")]
    Date(#[from] types::DateError),
    #[error("person string structurally malformed")]
    Person(#[from] types::PersonError),
    #[error("duration string structurally malformed")]
    Duration(#[from] types::DurationError),
    #[error("invalid url")]
    Url(#[from] url::ParseError),
    #[error("string is not a range")]
    Range,
    #[error("array element empty")]
    EmptyArrayElement,
    #[error("missing required field in details hash map")]
    MissingRequiredField,
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
            "title" | "publisher" | "location" | "archive" | "archive-location" => {
                if let Some(map) = value.clone().into_hash() {
                    FieldTypes::FormattableString(
                        formattable_str_from_hash_map(map).map_err(|e| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::FormattableString(e),
                            )
                        })?,
                    )
                } else if let Some(t) = value.into_string() {
                    FieldTypes::FormattableString(FormattableString::new_shorthand(t))
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "text or formattable string",
                    ));
                }
            }
            "author" | "editor" => {
                FieldTypes::Persons(persons_from_yaml(value, &key, &field_name)?)
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

                FieldTypes::PersonsWithRoles(res)
            }
            "date" => FieldTypes::Date(if let Some(value) = value.as_i64() {
                Date::from_year(value as i32)
            } else if let Some(value) = value.into_string() {
                Date::from_str(&value).map_err(|e| {
                    YamlBibliographyError::new_data_type_src_error(
                        &key,
                        &field_name,
                        YamlDataTypeError::Date(e),
                    )
                })?
            } else {
                return Err(YamlBibliographyError::new_data_type_error(
                    &key,
                    &field_name,
                    "date",
                ));
            }),
            "issue" | "edition" => {
                let as_int = value.as_i64();
                let as_str = if as_int == None { value.into_string() } else { None };

                if let Some(i) = as_int {
                    FieldTypes::IntegerOrText(NumOrStr::Number(i))
                } else if let Some(t) = as_str {
                    FieldTypes::IntegerOrText(NumOrStr::Str(t))
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer or text",
                    ));
                }
            }
            "volume-total" | "page-total" => {
                FieldTypes::Integer(value.into_i64().ok_or_else(|| {
                    YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer",
                    )
                })?)
            }
            "volume" | "page-range" => {
                FieldTypes::Range(if let Some(value) = value.as_i64() {
                    value .. value
                } else if let Some(value) = value.into_string() {
                    get_range(&value).ok_or_else(|| {
                        YamlBibliographyError::new_data_type_src_error(
                            &key,
                            &field_name,
                            YamlDataTypeError::Range,
                        )
                    })?
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "integer range",
                    ));
                })
            }
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

                FieldTypes::Duration(v)
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

                FieldTypes::TimeRange(v)
            }
            "url" => {
                let (url, date) = if let Some(s) = value.as_str() {
                    (
                        Url::parse(&s).map_err(|e| {
                            YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::Url(e),
                            )
                        })?,
                        None,
                    )
                } else if let Some(map) = value.into_hash() {
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
                            return Err(YamlBibliographyError::new_data_type_src_error(
                                &key,
                                &field_name,
                                YamlDataTypeError::MismatchedPrimitive,
                            ));
                        }
                    } else {
                        None
                    };

                    (url, date)
                } else {
                    return Err(YamlBibliographyError::new_data_type_error(
                        &key,
                        &field_name,
                        "qualified url",
                    ));
                };

                FieldTypes::Url(QualifiedUrl { value: url, visit_date: date })
            }
            "language" => FieldTypes::Language(
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

                    FieldTypes::Entries(entries)
                } else {
                    FieldTypes::Entries(vec![entry_from_yaml(
                        key.clone(),
                        value,
                        entry.entry_type.default_parent(),
                    )?])
                }
            }
            _ => {
                if let Some(t) = value.clone().into_string() {
                    FieldTypes::Text(t)
                } else if let Some(i) = value.as_i64() {
                    FieldTypes::Text(i.to_string())
                } else if let Some(i) = value.as_f64() {
                    FieldTypes::Text(i.to_string())
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

    // TODO derive total pages from page range

    Ok(entry)
}

#[cfg(test)]
mod tests {
    use super::load_yaml_structure;
    use crate::output::{apa, ieee, BibliographyGenerator};
    use crate::selectors::parse;
    use std::fs;

    #[test]
    fn apa() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let apa = apa::ApaBibliographyGenerator::new();

        for entry in &entries {
            let refs = apa.get_reference(&entry);
            println!("{}", refs.print_ansi_vt100());
        }
    }

    #[test]
    fn ieee() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();
        let ieee = ieee::IeeeBibliographyGenerator::new();

        for entry in &entries {
            let refs = ieee.get_reference(&entry);
            println!("{}", refs.print_ansi_vt100());
        }
    }

    macro_rules! select_all {
        ($select:expr, $entries:tt, [$($key:expr),* $(,)*] $(,)*) => {
            let keys = vec![ $( $key , )* ];
            let expr = parse($select).output.unwrap();
            for entry in &$entries {
                let res = expr.apply(entry, true);
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
            let expr = parse($select).output.unwrap();
            let res = expr.apply(entry, true).unwrap();
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
        select_all!("Article > (Periodical | NewspaperIssue)", entries, [
            "omarova-libra",
            "kinetics",
            "house"
        ]);
        select_all!("(Chapter | InAnthology) > (Anthology | Book)", entries, [
            "harry", "gedanken"
        ]);
        select_all!("*[url]", entries, [
            "omarova-libra",
            "science-e-issue",
            "oiseau",
            "georgia",
            "really-habitable",
            "electronic-music"
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
            "gedanken"
        ]);
    }

    #[test]
    fn selector_bindings() {
        let contents = fs::read_to_string("test/basic.yml").unwrap();
        let entries = load_yaml_structure(&contents).unwrap();

        select!(
            "a:Article > (b:Conference & c:(Video|Blog|WebItem))",
            entries >> "wwdc-network",
            ["a", "b", "c"]
        );
    }
}
