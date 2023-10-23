//! Optional archive of included CSL styles.

use citationberg::{Locale, Style};
use rkyv::{AlignedBytes, Archive, Deserialize, Serialize};
use std::collections::HashMap;

const ARCHIVE: AlignedBytes<1693160> =
    AlignedBytes(*include_bytes!("../../styles.cbor.rkyv"));

/// In-memory representation of a CSL archive.
#[derive(Debug, Clone, Archive, Serialize, Deserialize)]
pub struct Lookup {
    /// Maps from a CSL style name to an index into the `styles` vector.
    pub map: HashMap<String, usize>,
    /// The CSL styles in the archive as CBOR-encoded bytes.
    pub styles: Vec<Vec<u8>>,
    /// The locales in the archive as CBOR-encoded bytes.
    pub locales: Vec<Vec<u8>>,
}

/// Read an archive
unsafe fn read(buf: &[u8]) -> &<Lookup as Archive>::Archived {
    rkyv::archived_root::<Lookup>(buf)
}

/// An archived CSL style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArchiveStyle {
    /// Name of the style.
    pub name: &'static str,
    index: u32,
}

impl ArchiveStyle {
    /// Whether a style is an alias of another style.
    pub fn is_alias(self, other: Self) -> bool {
        self.index == other.index
    }
}

/// Retrieve a list of styles.
pub fn styles() -> Vec<ArchiveStyle> {
    unsafe { read(&ARCHIVE.0) }
        .map
        .iter()
        .map(|(k, v)| ArchiveStyle { name: k.as_str(), index: *v })
        .collect()
}

/// Retrieve a style from the archive
pub fn style(s: ArchiveStyle) -> Style {
    Style::from_cbor(&unsafe { read(&ARCHIVE.0) }.styles[s.index as usize]).unwrap()
}

/// Retrieve a style by name.
pub fn style_by_name(n: &str) -> Option<Style> {
    let lookup = unsafe { read(&ARCHIVE.0) };
    let idx = *lookup.map.get(n)?;
    Some(Style::from_cbor(&lookup.styles[idx as usize]).unwrap())
}

/// Retrieve the locales.
pub fn locales() -> Vec<Locale> {
    let lookup = unsafe { read(&ARCHIVE.0) };
    let res: Result<Vec<_>, _> =
        lookup.locales.iter().map(|l| Locale::from_cbor(l)).collect();
    res.unwrap()
}
