//! Formats citations as footnotes.
use crate::output::DisplayString;
use crate::Entry;
use std::collections::HashMap;

/// Describes the desired note type. This normally depends on the
/// previously cited keys, depending on the behavior. Also see the Chicago
/// Manual of Style, 17. ed., 14.20, 14.30, 14.34.
pub enum NoteType {
    /// Creates a full citation. Should always be used if this is the first
    /// occurrance of the key in a section and the work contains no
    /// bibliography. E.g. "Barack Obama, A Promised Land (London: Penguin
    /// Books 2020), 364-371."
    Full,
    /// Creates a compact citation. This can be used if either the source was
    /// already cited in the section or if there is a bibliography.
    /// E.g. "Obama, Promised Land, 292."
    Short,
    /// Creates a minimal citation, ommitting the title. This should only be
    /// used if the same source is cited multiple times without citing another
    /// source in-between. Also compare the `ibid`-Option of the
    /// [NoteCitationFormatter]. E.g. "Obama, 517."
    OnlyAuthor,
}

/// The struct doing the formatting.
pub struct NoteCitationFormatter<'s> {
    /// Entries within the database.
    entries: HashMap<String, &'s Entry>,
    /// Use ibid. instead of the repetition of the source.
    /// Discouraged by Chicago, 14.34.
    pub ibid: bool,
}

impl<'s> NoteCitationFormatter<'s> {
    /// Create a new [NoteCitationFormatter].
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        Self {
            entries: entries.map(|e| (e.key.clone(), e)).collect(),
            ibid: false,
        }
    }

    /// Format a citation as a note.
    pub fn get_note(&self, key: &str, kind: NoteType) -> DisplayString {
        todo!()
    }
}
