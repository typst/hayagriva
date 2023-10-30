//! Optional archive of included CSL styles.

use citationberg::{Locale, Style};
use rkyv::{Archive, Deserialize, Serialize};
use serde::de::DeserializeOwned;
use std::collections::{BTreeMap, HashMap};

#[repr(align(8))]
struct Data<T: ?Sized>(T);

static ARCHIVE: &Data<[u8]> = &Data(*include_bytes!("../../styles.cbor.rkyv"));

/// In-memory representation of a CSL archive.
#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct Lookup {
    /// Maps from a CSL style name to an index into the `styles` vector.
    pub map: BTreeMap<String, StyleMatch>,
    /// Maps from a CSL ID to an index into the `styles` vector.
    pub id_map: HashMap<String, usize>,
    /// The CSL styles in the archive as CBOR-encoded bytes.
    pub styles: Vec<Vec<u8>>,
    /// The locales in the archive as CBOR-encoded bytes.
    pub locales: Vec<Vec<u8>>,
}

/// A match between a style name and a style.
#[derive(Debug, Clone, Archive, Serialize, Deserialize, PartialEq, Eq)]
pub struct StyleMatch {
    /// A full, descriptive name of the style.
    pub full_name: String,
    /// Whether this is an alias.
    pub alias: bool,
    /// Whether the style contains a bibliography.
    pub bibliography: bool,
    /// The style index.
    pub index: usize,
}

impl StyleMatch {
    /// Create a new style match.
    pub fn new(full_name: String, alias: bool, bibliography: bool, index: usize) -> Self {
        Self { full_name, alias, bibliography, index }
    }
}

/// Read an archive
fn read() -> &'static <Lookup as Archive>::Archived {
    unsafe { rkyv::archived_root::<Lookup>(&ARCHIVE.0) }
}

/// An archived CSL style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArchiveStyle {
    /// Name of the style.
    pub name: &'static str,
    /// A full, descriptive name of the style.
    pub full_name: &'static str,
    /// Whether this is an alias.
    pub alias: bool,
    index: u32,
}

impl ArchiveStyle {
    /// Whether a style is an alias of another style.
    pub fn is_alias(self, other: Self) -> bool {
        self.index == other.index
    }
}

/// Retrieve a list of styles.
pub fn styles() -> impl Iterator<Item = ArchiveStyle> {
    read().map.iter().map(|(k, v)| ArchiveStyle {
        name: k.as_str(),
        index: v.index,
        full_name: &v.full_name,
        alias: v.alias,
    })
}

/// Retrieve a style from the archive
pub fn style(s: ArchiveStyle) -> Style {
    from_cbor::<Style>(&read().styles[s.index as usize]).unwrap()
}

/// Retrieve a style by name.
pub fn style_by_name(n: &str) -> Option<Style> {
    let lookup = read();
    let index = lookup.map.get(n)?.index;
    Some(from_cbor::<Style>(&lookup.styles[index as usize]).unwrap())
}

/// Retrieve a style by name.
pub fn style_by_id(n: &str) -> Option<Style> {
    let lookup = read();
    let index = lookup.id_map.get(n)?;
    Some(from_cbor::<Style>(&lookup.styles[*index as usize]).unwrap())
}

/// Retrieve the locales.
pub fn locales() -> Vec<Locale> {
    let lookup = read();
    let res: Result<Vec<_>, _> =
        lookup.locales.iter().map(|l| from_cbor::<Locale>(l)).collect();
    res.unwrap()
}

fn from_cbor<T: DeserializeOwned>(
    reader: &[u8],
) -> Result<T, ciborium::de::Error<std::io::Error>> {
    ciborium::de::from_reader(reader)
}
