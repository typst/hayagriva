//! Formats citations as footnotes for Chicago's Notes and Bibliography style.

use std::collections::HashMap;

use super::{
    and_list, entry_date, format_date, get_chunk_title, get_creators, get_info_element,
    get_title, is_authoritative, web_creator, AuthorRole, CommonChicagoConfig, DateMode,
};
use crate::style::{
    abbreviate_publisher, delegate_titled_entry, format_range, push_comma_quote_aware,
    AtomicCitation, Bracket, BracketMode, BracketPreference, CitationError,
    CitationFormatter, DisplayString, Formatting,
};
use crate::types::{EntryType::*, FmtOptionExt};
use crate::Entry;

/// Describes the desired note type. This normally depends on the
/// previously cited keys, depending on the behavior. Also see the Chicago
/// Manual of Style, 17. ed., 14.20, 14.30, 14.34.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    /// [Note]. E.g. "Obama, 517."
    OnlyAuthor,
}

/// The struct doing the formatting.
pub struct Note<'s> {
    /// Entries within the database.
    entries: HashMap<String, &'s Entry>,
    /// Use ibid. instead of the repetition of the source.
    /// Discouraged by Chicago, 14.34.
    pub ibid: bool,
    /// Properties shared with the bibliography.
    pub common: CommonChicagoConfig,
}

impl<'s> Note<'s> {
    /// Create a new [Note].
    pub fn new(entries: impl Iterator<Item = &'s Entry>) -> Self {
        Self {
            entries: entries.map(|e| (e.key.clone(), e)).collect(),
            ibid: false,
            common: CommonChicagoConfig::new(),
        }
    }

    fn get_author(&self, entry: &Entry, short: bool) -> String {
        let (authors, add) = get_creators(entry);
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
                        p.name
                    }
                })
                .collect::<Vec<_>>()
        } else {
            authors
                .into_iter()
                .enumerate()
                .map(|(i, p)| {
                    let name = p.given_first(false);
                    if entry.entry_type == Tweet {
                        if let Some(pseud) = entry.twitter_handle(i) {
                            format!("{} ({})", name, pseud)
                        } else {
                            name
                        }
                    } else {
                        if let Some(pseud) = p.alias {
                            format!("{} [{}]", pseud, name)
                        } else {
                            name
                        }
                    }
                })
                .collect::<Vec<_>>()
        };
        let mut res = and_list(authors, false, self.common.et_al_limit);

        if !short {
            let add = match add {
                AuthorRole::Editor if count > 1 => "eds.",
                AuthorRole::Editor => "ed.",
                AuthorRole::Compiler if count > 1 => "comps.",
                AuthorRole::Compiler => "comp.",
                AuthorRole::Translator => "trans.",
                AuthorRole::Director if count > 1 => "dirs.",
                AuthorRole::Director => "dir.",
                AuthorRole::ExecutiveProducer if count > 1 => "exec. producers",
                AuthorRole::ExecutiveProducer => "exec. producer",
                AuthorRole::Normal => "",
            };

            if !add.is_empty() {
                if !res.is_empty() && res.chars().last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
                res += add;
            }
        }

        res
    }

    fn get_publication_info(&self, entry: &Entry) -> String {
        let conference = select!(* > ("p":Conference)).bound(entry, "p");
        let mut res = if entry.entry_type == Thesis {
            "thesis".to_string()
        } else if conference.is_some() {
            "conference presentation".into()
        } else {
            String::new()
        };

        let published_entry = select!(* > ("p":(*["publisher"]))).bound(entry, "p");
        if let Some(loc) = entry
            .location()
            .or_else(|| published_entry.and_then(|e| e.location()))
        {
            if !res.is_empty() {
                res += ", ";
            }
            res += &loc.value;
        } else if matches!(&entry.entry_type, Book | Anthology)
            && entry.any_date().map(|d| d.year).unwrap_or(2020) < 1981
        {
            if !res.is_empty() {
                res += ", ";
            }
            res += "n.p.";
        }

        if entry.entry_type == Tweet {
            if let Some(host) = entry
                .any_url()
                .and_then(|u| u.value.host_str())
                .map(|h| h.to_lowercase())
            {
                let service = match host.as_ref() {
                    "twitter.com" => "Twitter",
                    "facebook.com" => "Facebook",
                    "instagram.com" => "Instagram",
                    "vt.tiktok.com" => "TikTok",
                    _ => "",
                };

                if !service.is_empty() && !res.is_empty() {
                    res += ", ";
                }
                res += service;
            }
        }

        let preprint = select!(Article > Repository).matches(entry);
        if entry.entry_type == Manuscript || preprint {
            if !res.is_empty() {
                res += ": ";
            }
            if preprint {
                res += "preprint";
            } else {
                res += "unpublished manuscript";
            }
        } else if entry.entry_type == Artwork {
            // Intentionally empty: We do the publisher stuff later
        } else if let Some(conf) = conference {
            if let Some(org) = conf.organization() {
                res += ", ";
                res += org;
            }

            let conf_name = get_chunk_title(conf, false, false, &self.common).value;
            if !conf_name.is_empty() {
                res += ", ";
                res += &conf_name;
            }

            if let Some(loc) = conf.location() {
                res += ", ";
                res += &loc.value;
            }
        } else if let Some(publisher) = entry
            .publisher()
            .value()
            .or_else(|| published_entry.map(|e| e.publisher().value().unwrap()))
            .or_else(|| {
                if matches!(&entry.entry_type, Report | Thesis)
                    || (matches!(&entry.entry_type, Case | Legislation)
                        && entry.serial_number().is_some())
                {
                    entry.organization()
                } else {
                    None
                }
            })
            .map(Into::into)
            .or_else(|| {
                if entry.entry_type == Reference && entry.volume().is_none() {
                    entry.authors_fallible().map(|a| {
                        and_list(
                            a.into_iter().map(|p| p.given_first(false)),
                            false,
                            self.common.et_al_limit,
                        )
                    })
                } else {
                    None
                }
            })
            .map(|p| abbreviate_publisher(&p, false))
        {
            if !res.is_empty() {
                res += ": ";
            }
            res += &publisher;
        } else if let Some(creator) = web_creator(entry, false, self.common.et_al_limit) {
            if !res.is_empty() && !creator.is_empty() {
                res += ", ";
            }

            res += &creator;
        }

        let date = entry_date(entry, false);

        if !date.is_empty() && !res.is_empty() {
            res += ", ";
        }
        res += &date;

        if entry.entry_type == Artwork {
            let mut items: Vec<String> = vec![];

            items.extend(entry.note().map(Into::into));

            let parent = select!(* > ("p":Exhibition)).bound(entry, "p");
            items.extend(
                entry
                    .organization()
                    .or_else(|| entry.publisher().value())
                    .or_else(|| parent.and_then(|p| p.organization()))
                    .or_else(|| parent.and_then(|p| p.publisher().value()))
                    .map(Into::into),
            );

            items.extend(
                entry
                    .location()
                    .or_else(|| parent.and_then(|p| p.location()))
                    .value()
                    .map(Into::into),
            );

            let items = items.join(", ");
            if !items.is_empty() && !res.is_empty() {
                res += ", ";
            }
            res += &items;
        }

        if entry.entry_type == Exhibition {
            if !res.is_empty() {
                res += ", ";
            }

            res += "exhibition catalog"
        }

        res
    }

    /// Format a citation as a note.
    pub fn get_note(
        &self,
        citation: AtomicCitation,
        kind: NoteType,
    ) -> Option<DisplayString> {
        Some(self.get_entry_note(
            self.entries.get(citation.key).map(|o| o.clone())?,
            citation.supplement,
            kind,
        ))
    }

    /// Format a citation as a note given an entry.
    pub fn get_entry_note(
        &self,
        mut entry: &Entry,
        supplement: Option<&str>,
        kind: NoteType,
    ) -> DisplayString {
        let short = kind != NoteType::Full;

        entry = delegate_titled_entry(entry);

        let web_thing = select!(Web | ((Misc | Web) > Web)).matches(entry);

        let mut res: DisplayString = if (!web_thing
            && (entry.entry_type != Reference
                || entry.publisher().is_some()
                || entry.volume().is_some()))
            || short
        {
            self.get_author(entry, short).into()
        } else {
            DisplayString::new()
        };

        let no_author = res.is_empty();

        let dictionary = select!(Entry > ("p": Reference)).bound(entry, "p");
        let database = select!(Entry > ("p": Repository)).bound(entry, "p");
        if (no_author && dictionary.is_some()) || database.is_some() {
            let dictionary = dictionary.or(database).unwrap();
            let title = get_title(dictionary, short, &self.common, ',');
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        } else if kind != NoteType::OnlyAuthor || no_author {
            let mut title = get_title(entry, short, &self.common, ',');
            if (entry.entry_type == Case || entry.entry_type == Legislation) && !short {
                title.clear_formatting();
            }
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        }

        let mut colon = false;
        let journal =
            select!((Article | Entry) > (Periodical | Newspaper)).matches(entry);

        if !short {
            let add = if no_author && dictionary.is_some() {
                get_info_element(dictionary.unwrap(), &self.common, false)
            } else {
                get_info_element(entry, &self.common, false)
            };
            if !add.is_empty() {
                push_comma_quote_aware(&mut res.value, ',', true);
            }
            res += add;

            let publ = self.get_publication_info(if no_author {
                dictionary.unwrap_or(entry)
            } else {
                entry
            });
            let brackets = is_authoritative(entry)
                || entry.entry_type == Manuscript
                || (no_author && dictionary.is_some());
            if !publ.is_empty() && !res.is_empty() {
                if brackets {
                    res.push(' ');
                } else {
                    push_comma_quote_aware(&mut res.value, ',', true);
                }
            }

            if !publ.is_empty() {
                if brackets {
                    res += &format!("({})", publ);
                } else {
                    res += &publ;
                }
            }

            if journal && !publ.is_empty() {
                colon = true;
            }
        } else if database.is_some() {
            let title = get_chunk_title(entry, true, false, &self.common).value;
            let db_entry = if title.is_empty() {
                entry.serial_number().unwrap_or_default().into()
            } else {
                title
            };
            if !db_entry.is_empty() {
                push_comma_quote_aware(&mut res.value, ',', true);
                res += &db_entry;
            }
        }

        if no_author && dictionary.is_some() && entry.authors_fallible().is_none() {
            push_comma_quote_aware(&mut res.value, ',', true);
            res += "s.v. ";
            res += get_chunk_title(entry, false, true, &self.common);
        }

        if let Some(supplement) = supplement {
            if !res.is_empty() {
                if colon {
                    res.push(':');
                } else {
                    push_comma_quote_aware(&mut res.value, ',', false);
                }
                res.push(' ');
            }

            res += supplement;
        } else if let Some(pr) = entry.page_range() {
            if !res.is_empty() {
                if colon {
                    res.push(':');
                } else {
                    push_comma_quote_aware(&mut res.value, ',', false);
                }
                res.push(' ');
            }

            res += &format_range("", "", pr);
        }

        if journal && !short {
            if let Some(sn) = entry.serial_number() {
                if !sn.is_empty() {
                    push_comma_quote_aware(&mut res.value, ',', false);
                }

                if !sn.is_empty() && !res.is_empty() {
                    res.push(' ');
                }

                res += sn;
            }
        }

        if !short {
            let url = if let Some(doi) = entry.doi() {
                let mut res = DisplayString::new();
                res.start_format(Formatting::NoHyphenation);
                res += &format!("https://doi.org/{}", doi);
                res.commit_formats();
                res
            } else if let Some(qurl) = entry.any_url() {
                let mut res = DisplayString::new();
                if let Some(date) = qurl.visit_date.as_ref() {
                    if database.is_none() && self.common.url_access_date.needs_date(entry)
                    {
                        res +=
                            &format!("accessed {}, ", format_date(date, DateMode::Day));
                    }
                }

                res.start_format(Formatting::NoHyphenation);
                res += qurl.value.as_str();
                res.commit_formats();
                res
            } else {
                DisplayString::new()
            };

            if journal {
                if !url.is_empty() && !res.is_empty() && res.last() != Some('.') {
                    res.push('.');
                }
            } else if database.is_some() {
                let mut brack_content =
                    get_chunk_title(entry, false, false, &self.common);
                if let Some(sn) = entry.serial_number() {
                    push_comma_quote_aware(&mut brack_content.value, ',', true);
                    brack_content += sn;
                }
                if self.common.url_access_date.needs_date(entry) {
                    if let Some(date) =
                        entry.any_url().and_then(|u| u.visit_date.as_ref())
                    {
                        push_comma_quote_aware(&mut brack_content.value, ';', true);
                        brack_content +=
                            &format!("accessed {}", format_date(date, DateMode::Day));
                    }
                }
                if !brack_content.is_empty() {
                    if !res.is_empty() {
                        res.push(' ');
                    }

                    res.push('(');
                    res += brack_content;
                    res += ").";
                }
            } else {
                if !url.is_empty() {
                    push_comma_quote_aware(&mut res.value, ',', false);
                }
            }

            if !url.is_empty() && !res.is_empty() {
                res.push(' ');
            }

            let no_url = url.is_empty();
            res += url;

            let preprint = select!(Article > Repository).matches(entry);
            if no_url || entry.entry_type == Manuscript || preprint {
                if let Some(archive) = entry.archive() {
                    push_comma_quote_aware(&mut res.value, ',', true);

                    res += &archive.value;

                    if let Some(al) = entry.archive_location() {
                        res += ", ";
                        res += &al.value;
                    }
                }
            }
        }

        if !res.is_empty() && res.last() != Some('.') {
            res.push('.');
        }

        res
    }
}

impl<'s> CitationFormatter<'s> for Note<'s> {
    /// This implementation will always create full notes.
    /// Use [`Note::get_note`] to set the
    /// [`NoteType`].
    fn format(
        &self,
        citation: impl IntoIterator<Item = AtomicCitation<'s>>,
    ) -> Result<DisplayString, CitationError> {
        let mut items = vec![];
        for c in citation {
            items.push(
                self.get_note(c, NoteType::Full)
                    .ok_or_else(|| CitationError::KeyNotFound(c.key.into()))?,
            )
        }

        Ok(DisplayString::join(&items, "\n"))
    }
}

impl<'s> BracketPreference for Note<'s> {
    fn default_brackets() -> Bracket {
        Bracket::None
    }

    fn default_bracket_mode() -> BracketMode {
        BracketMode::Unwrapped
    }
}
