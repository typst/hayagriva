//! Helpers for serializing and deserializing.

use serde::{Deserialize, Deserializer, Serialize, de::Visitor};

/// Generic wrapper that allow one or more occurrences of specified type.
///
/// In YAML it will presented or as a value, or as an array:
/// ```yaml
/// one: just a string
/// many:
///   - 1st string
///   - 2nd string
/// ```
///
/// Extracted from [`ksc-rs`](https://github.com/Mingun/ksc-rs/) under the MIT
/// license.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum OneOrMany<T> {
    /// Single value
    One(Box<T>),
    /// Array of values
    Vec(Vec<T>),
}

impl<T> From<OneOrMany<T>> for Vec<T> {
    fn from(from: OneOrMany<T>) -> Self {
        match from {
            OneOrMany::One(val) => vec![*val],
            OneOrMany::Vec(vec) => vec,
        }
    }
}

impl<T> Default for OneOrMany<T> {
    fn default() -> Self {
        OneOrMany::Vec(Vec::new())
    }
}

impl<T> IntoIterator for OneOrMany<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            OneOrMany::One(val) => vec![*val].into_iter(),
            OneOrMany::Vec(vec) => vec.into_iter(),
        }
    }
}

/// Function that uses [`OneOrMany`] to serialize.
pub fn serialize_one_or_many<T, S>(value: &[T], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
{
    if value.len() == 1 {
        value[0].serialize(serializer)
    } else {
        value.serialize(serializer)
    }
}

/// Function that uses [`OneOrMany`] to serialize for options.
pub fn serialize_one_or_many_opt<T, S>(
    value: &Option<Vec<T>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
{
    if let Some(value) = value {
        serialize_one_or_many(value, serializer)
    } else {
        serializer.serialize_none()
    }
}

/// Function that always serializes as a list, even for single items.
/// This is used for fields like `affiliated` that should only accept lists.
pub fn serialize_list_only_opt<T, S>(
    value: &Option<Vec<T>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
{
    if let Some(value) = value {
        value.serialize(serializer)
    } else {
        serializer.serialize_none()
    }
}

/// This is a wrapper for [`OneOrMany`] that assumes that the single
/// representation isn't a sequence. This allows better error messages.
#[derive(Clone, Debug, PartialEq, Eq)]
struct MapOneOrMany<T>(OneOrMany<T>);

impl<T> From<MapOneOrMany<T>> for OneOrMany<T> {
    fn from(from: MapOneOrMany<T>) -> Self {
        from.0
    }
}

impl<T> From<MapOneOrMany<T>> for Vec<T> {
    fn from(from: MapOneOrMany<T>) -> Self {
        from.0.into()
    }
}

impl<'de, T> Deserialize<'de> for MapOneOrMany<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<MapOneOrMany<T>, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct MapOneOrManyVisitor<T>(std::marker::PhantomData<T>);

        impl<'de, T> Visitor<'de> for MapOneOrManyVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = MapOneOrMany<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a map, a string, or a list")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(MapOneOrMany(OneOrMany::One(Box::new(T::deserialize(
                    serde::de::value::StrDeserializer::new(v),
                )?))))
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                Ok(MapOneOrMany(OneOrMany::One(Box::new(T::deserialize(
                    serde::de::value::MapAccessDeserializer::new(map),
                )?))))
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                Ok(MapOneOrMany(OneOrMany::Vec(Vec::deserialize(
                    serde::de::value::SeqAccessDeserializer::new(seq),
                )?)))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(MapOneOrMany(OneOrMany::One(Box::new(T::deserialize(
                    serde::de::value::U64Deserializer::new(v),
                )?))))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(MapOneOrMany(OneOrMany::One(Box::new(T::deserialize(
                    serde::de::value::I64Deserializer::new(v),
                )?))))
            }
        }

        deserializer.deserialize_any(MapOneOrManyVisitor(std::marker::PhantomData))
    }
}

/// Function that uses [`MapOneOrMany`] to deserialize.
pub fn deserialize_one_or_many<'de, T, D>(deserializer: D) -> Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    <MapOneOrMany<T>>::deserialize(deserializer).map(|v| v.into())
}

/// Function that uses [`MapOneOrMany`] to deserialize for options.
pub fn deserialize_one_or_many_opt<'de, T, D>(
    deserializer: D,
) -> Result<Option<Vec<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    <Option<MapOneOrMany<T>>>::deserialize(deserializer).map(|v| v.map(|v| v.into()))
}

/// Wrapper that only accepts sequences (lists), rejecting maps (single objects).
#[derive(Clone, Debug, PartialEq, Eq)]
struct ListOnly<T>(Vec<T>);

impl<'de, T> Deserialize<'de> for ListOnly<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ListOnlyVisitor<T>(std::marker::PhantomData<T>);

        impl<'de, T> Visitor<'de> for ListOnlyVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = ListOnly<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a list")
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                Vec::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))
                    .map(ListOnly)
            }

            fn visit_map<A>(self, _map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                Err(serde::de::Error::custom(
                    "expected a list, found a single object. `affiliated` must be a list of objects, not a single object.",
                ))
            }
        }

        deserializer.deserialize_any(ListOnlyVisitor(std::marker::PhantomData))
    }
}

impl<T> From<ListOnly<T>> for Vec<T> {
    fn from(list_only: ListOnly<T>) -> Self {
        list_only.0
    }
}

/// Function that only accepts a list (sequence) for deserialization, rejecting single objects.
/// This is used for fields like `affiliated` that should only accept a list of objects.
pub fn deserialize_list_only_opt<'de, T, D>(
    deserializer: D,
) -> Result<Option<Vec<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    <Option<ListOnly<T>>>::deserialize(deserializer).map(|opt| opt.map(Into::into))
}
