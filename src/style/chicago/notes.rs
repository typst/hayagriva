//! Formats citations as footnotes for Chicago'a Notes and Bibliography style.

use super::{
    and_list, bibliography::Bibliography, entry_date, format_date, get_chunk_title,
    get_creators, get_info_element, get_title, is_authoritative, web_creator, AuthorRole,
    ChicagoConfig, DateMode, Mode,
};
use crate::style::{
    abbreviate_publisher, delegate_titled_entry, format_range, push_comma_quote_aware,
    sorted_bibliography, BibliographyOrdering, BibliographyStyle, Brackets, Citation,
    CitationStyle, Database, DisplayCitation, DisplayReference, DisplayString,
    Formatting, Record,
};
use crate::types::{EntryType::*, FmtOptionExt};
use crate::Entry;

/// Verbosity of Chicago _Notes_.
///
/// # Reference
/// See the 17th edition of the Chicago Manual of Style (CMoS), Chapters 14,
/// Sections 20, 30, and 34.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ChicagoNoteStyle {
    /// Uses [`Full`](Self::Full) for the first and [`Short`](Self::Short) for
    /// all subsequent notes, except if the same entry is cited twice in a row.
    /// Then, depending on the value of [`ibid`](Self::Automatic::ibid), either
    /// the string "ibid" or the very short [`Author`](Self::Author) mode is
    /// used.
    ///
    /// Please note that the CMoS 14.34 discourages "ibid" citations in favor of
    /// the shorter author style.
    Automatic {
        /// Whether to shorten repeating entries with the latin abbreviation
        /// "ibid".
        ibid: bool,
    },
    /// Very short note style.
    Author,
    /// Relatively short note style.
    Short,
    /// Long and verbose note style.
    Full,
}

impl Default for ChicagoNoteStyle {
    fn default() -> Self {
        Self::Automatic { ibid: false }
    }
}

/// Citations and bibliographies following the Chicago _Notes and Bibliography_
/// style.
///
/// # Examples
/// Notes with differing verbosity:
/// - [Author]: Tolkien, p. 129.
/// - [Short]: Tolkien, _Lord of the Rings_, p. 129.
/// - [Full]: J. R. R. Tolkien, _The Lord of the Rings,_ vol. 1, _The Fellowship
///   of the Ring_ (London: Allen & Unwin, 1954), p. 129.
///
/// Bibliography:
/// - Tolkien, J. R. R. _The Lord of the Rings._ Vol. 1. _The Fellowship of the
///   Ring._ London: Allen & Unwin, 1954.
/// - Darnton, Robert. “What Is the History of Books.” _Dædalus_ 111, no. 3. MIT
///   Press, June 1982: 65–83.
///
/// # Reference
/// See the 17th edition of the Chicago Manual of Style, Chapter 14, for details
/// on how Chicago advises you to format citations and bibliographies.
///
/// If you're unsure on which Chicago style to use, the manual generally
/// recommends [_Notes and Bibliography_](ChicagoNotes) for the humanities and
/// social sciences whereas [_Author
/// Date_](super::author_date::ChicagoAuthorDate) is recommended for natural
/// sciences, mathematics, and engineering.
///
/// [Author]: ChicagoNoteStyle::Author
/// [Short]: ChicagoNoteStyle::Short
/// [Full]: ChicagoNoteStyle::Full
#[derive(Clone, Debug, PartialEq)]
pub struct ChicagoNotes<'a> {
    /// Properties shared with the bibliography.
    pub config: ChicagoConfig,
    /// Which form of notes to produce.
    pub style: ChicagoNoteStyle,
    cited: Vec<&'a Entry>,
}

impl<'a> Default for ChicagoNotes<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ChicagoNotes<'a> {
    /// Create the struct using a configuration struct.
    ///
    /// You can use [the default trait implementation](Self::default) to
    /// initialize with the standard configuration.
    pub fn new() -> Self {
        Self {
            config: ChicagoConfig::default(),
            style: ChicagoNoteStyle::default(),
            cited: vec![],
        }
    }

    /// Reset the citation shortening state.
    ///
    /// Does not affect references and is only applicable if the note type
    /// is [`Automatic`](ChicagoNoteStyle::Automatic).
    pub fn reset_shortening(&mut self) {
        self.cited.clear();
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
                    } else if let Some(pseud) = p.alias {
                        format!("{} [{}]", pseud, name)
                    } else {
                        name
                    }
                })
                .collect::<Vec<_>>()
        };
        let mut res = and_list(authors, false, self.config.et_al_limit);

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
                if !res.is_empty() && !res.ends_with(',') {
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
            && entry.date_any().map(|d| d.year).unwrap_or(2020) < 1981
        {
            if !res.is_empty() {
                res += ", ";
            }
            res += "n.p.";
        }

        if entry.entry_type == Tweet {
            if let Some(host) = entry
                .url_any()
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

            let conf_name = get_chunk_title(conf, false, false, &self.config).value;
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
                    entry.authors().map(|a| {
                        and_list(
                            a.iter().map(|p| p.given_first(false)),
                            false,
                            self.config.et_al_limit,
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
        } else if let Some(creator) = web_creator(entry, false, self.config.et_al_limit) {
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

    /// Format a citation as a note given an entry.
    fn get_note(&mut self, citation: Citation<'a>) -> DisplayString {
        let mut entry = citation.entry;
        let short = self.style == ChicagoNoteStyle::Short
            || self.style == ChicagoNoteStyle::Author
            || (matches!(self.style, ChicagoNoteStyle::Automatic { ibid: _ })
                && self.cited.contains(&entry));

        entry = delegate_titled_entry(entry);

        let web_thing = select!(Web | ((Misc | Web) > Web)).matches(entry);

        let ibid = matches!(self.style, ChicagoNoteStyle::Automatic { ibid: true })
            && self.cited.last() == Some(&entry);
        let mut res: DisplayString = if ibid {
            "Ibid".into()
        } else if (!web_thing
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
            let title = get_title(dictionary, short, &self.config, ',');
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        } else if !(self.style == ChicagoNoteStyle::Author
            || (matches!(self.style, ChicagoNoteStyle::Automatic { ibid: _ })
                && self.cited.last() == Some(&entry)))
            || no_author
        {
            let mut title = get_title(entry, short, &self.config, ',');
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
                get_info_element(dictionary.unwrap(), &self.config, false)
            } else {
                get_info_element(entry, &self.config, false)
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
            let title = get_chunk_title(entry, true, false, &self.config).value;
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

        if no_author && dictionary.is_some() && entry.authors().is_none() {
            push_comma_quote_aware(&mut res.value, ',', true);
            res += "s.v. ";
            res += get_chunk_title(entry, false, true, &self.config);
        }

        if let Some(supplement) = citation.supplement {
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
                let link = format!("https://doi.org/{}", doi);
                res.start_format(Formatting::Link(link.clone()));
                res += &link;
                res.commit_formats();
                res
            } else if let Some(qurl) = entry.url_any() {
                let mut res = DisplayString::new();
                if let Some(date) = qurl.visit_date.as_ref() {
                    if database.is_none() && self.config.url_access_date.needs_date(entry)
                    {
                        res +=
                            &format!("accessed {}, ", format_date(date, DateMode::Day));
                    }
                }

                res.start_format(Formatting::Link(qurl.value.to_string()));
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
                    get_chunk_title(entry, false, false, &self.config);
                if let Some(sn) = entry.serial_number() {
                    push_comma_quote_aware(&mut brack_content.value, ',', true);
                    brack_content += sn;
                }
                if self.config.url_access_date.needs_date(entry) {
                    if let Some(date) =
                        entry.url_any().and_then(|u| u.visit_date.as_ref())
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
            } else if !url.is_empty() {
                push_comma_quote_aware(&mut res.value, ',', false);
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

        self.cited.push(citation.entry);

        res
    }
}

impl<'a> CitationStyle<'a> for ChicagoNotes<'a> {
    fn citation(
        &mut self,
        _: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut items = vec![];
        for &c in parts {
            items.push(self.get_note(c))
        }

        DisplayCitation::new(DisplayString::join(&items, "\n"), true)
    }

    fn brackets(&self) -> Brackets {
        Brackets::None
    }

    fn wrapped(&self) -> bool {
        false
    }
}

impl<'a> BibliographyStyle<'a> for ChicagoNotes<'a> {
    fn bibliography(
        &self,
        db: &Database<'a>,
        ordering: BibliographyOrdering,
    ) -> Vec<DisplayReference<'a>> {
        let bib_format = Bibliography::new(Mode::NotesAndBibliography, self.config);
        let mut items = vec![];

        for record in db.records() {
            let (bib, al) = bib_format.format(record.entry, record.disambiguation);
            items.push((
                DisplayReference {
                    display: bib,
                    entry: record.entry,
                    prefix: record.prefix.clone().map(Into::into),
                },
                al,
            ))
        }

        sorted_bibliography(items, ordering)
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        let bib_format = Bibliography::new(Mode::NotesAndBibliography, self.config);
        let (bib, _) = bib_format.format(record.entry, record.disambiguation);
        DisplayReference {
            display: bib,
            entry: record.entry,
            prefix: record.prefix.clone().map(Into::into),
        }
    }

    fn ordering(&self) -> BibliographyOrdering {
        BibliographyOrdering::ByAuthor
    }
}
