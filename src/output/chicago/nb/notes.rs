//! Formats citations as footnotes.
use crate::selectors::{Bind, Id, Neg};
use crate::types::EntryType::*;
use crate::types::PersonRole;
use crate::{attrs, sel, Entry};
use crate::{lang::TitleCase, output::DisplayString};
use std::collections::HashMap;

/// Describes the desired note type. This normally depends on the
/// previously cited keys, depending on the behavior. Also see the Chicago
/// Manual of Style, 17. ed., 14.20, 14.30, 14.34.
#[derive(Copy, Clone, Debug, PartialEq)]
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
    tc_formatter: TitleCase,
    /// Use ibid. instead of the repetition of the source.
    /// Discouraged by Chicago, 14.34.
    pub ibid: bool,
    /// If there is greater or equal to this number of authors, they will be
    /// abbreviated using et. al. after the first name.
    pub et_al_limit: Option<usize>,
}

impl<'s> NoteCitationFormatter<'s> {
    /// Create a new [NoteCitationFormatter].
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        let tc_formatter = TitleCase::new();
        Self {
            entries: entries.map(|e| (e.key.clone(), e)).collect(),
            tc_formatter,
            ibid: false,
            et_al_limit: Some(4),
        }
    }

    fn and_list(&self, names: Vec<String>, oxford: bool) -> String {
        let name_len = names.len();
        let mut res = String::new();
        let threshold = self.et_al_limit.unwrap_or(0);

        for (index, name) in names.into_iter().enumerate() {
            if threshold > 0 && index > 0 && name_len >= threshold {
                break;
            }

            res += &name;

            if (index as i32) <= name_len as i32 - 2
                && (threshold == 0 || name_len < threshold)
            {
                if oxford || name_len > 2 {
                    res += ", ";
                } else {
                    res.push(' ');
                }
            }
            if (index as i32) == name_len as i32 - 2 {
                res += "and ";
            }
        }

        if threshold > 0 && name_len >= threshold {
            res += " et al."
        }

        res
    }

    fn get_title(&self, entry: &Entry, short: bool) -> String {
        todo!()
    }

    fn get_author(&self, entry: &Entry, short: bool) -> String {
        enum AuthorRole {
            Normal,
            Editor,
            Translator,
            Director,
            ExecutiveProducer,
        }

        let mut add = AuthorRole::Normal;
        let authors = if let Some(authors) = entry.get_authors_fallible() {
            authors.to_vec()
        } else if let Some(eds) = entry.get_editors() {
            add = AuthorRole::Editor;
            eds.to_vec()
        } else if entry.entry_type == Video {
            let tv_series =
                sel!(attrs!(Id(Video), "issue", "volume") => Bind("p", Id(Video)));
            if let Some(mut hm) = tv_series.apply(entry) {
                let mut affs = entry.get_affiliated_filtered(PersonRole::Director);
                affs.extend(
                    entry.get_affiliated_filtered(PersonRole::Writer).into_iter(),
                );
                if !affs.is_empty() {
                    affs
                } else {
                    let parent = hm.remove("p").unwrap();
                    add = AuthorRole::ExecutiveProducer;
                    parent.get_affiliated_filtered(PersonRole::ExecutiveProducer)
                }
            } else {
                let dir = entry.get_affiliated_filtered(PersonRole::Director);
                if !dir.is_empty() {
                    add = AuthorRole::Director;
                    dir
                } else {
                    add = AuthorRole::ExecutiveProducer;
                    entry.get_affiliated_filtered(PersonRole::ExecutiveProducer)
                }
            }
        } else {
            let translators = entry.get_affiliated_filtered(PersonRole::Translator);
            if !translators.is_empty() {
                add = AuthorRole::Translator;
                translators
            } else {
                vec![]
            }
        };

        if authors.is_empty() {
            return String::new();
        }

        let count = authors.len();
        let authors = if short {
            authors
                .into_iter()
                .map(|p| {
                    if let Some(prefix) = p.prefix {
                        format!("{} {}", prefix, p.name)
                    } else {
                        p.name.clone()
                    }
                })
                .collect::<Vec<_>>()
        } else {
            authors
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>()
        };
        let mut res = self.and_list(authors, false);

        if !short {
            let add = match add {
                AuthorRole::Editor if count > 1 => "eds.",
                AuthorRole::Editor => "ed.",
                AuthorRole::Translator => "trans.",
                AuthorRole::Director if count > 1 => "directors",
                AuthorRole::Director => "director",
                AuthorRole::ExecutiveProducer if count > 1 => "exec. producers",
                AuthorRole::ExecutiveProducer => "exec. producer",
                AuthorRole::Normal => "",
            };

            if !add.is_empty() {
                if !res.is_empty() && res.chars().last().unwrap_or('a') != ',' {
                    res.push(',');
                }

                res.push(' ');
                res += add;
            }
        }

        res
    }

    /// Format a citation as a note.
    pub fn get_note(&self, key: &str, kind: NoteType) -> Option<DisplayString> {
        let res = self.get_author(self.entries.get(key)?, kind != NoteType::Full).into();
        Some(res)
    }
}
