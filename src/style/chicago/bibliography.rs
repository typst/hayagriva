//! Formats citations as bibliographies. This module supports both the
//! _Notes and Bibliography_ and the _Author-Date_ style. The style can
//! be set through the [`Bibliography`] struct.

use super::{
    and_list, entry_date, format_date, get_chunk_title, get_creators, get_info_element,
    get_title, AuthorRole, ChicagoConfig, DateMode, Mode,
};
use crate::style::{
    abbreviate_publisher, alph_designator, chicago::web_creator, delegate_titled_entry,
    format_range, push_comma_quote_aware, DisplayString, Formatting,
};
use crate::types::{EntryType::*, FmtOptionExt, Person};
use crate::Entry;

use unicode_segmentation::UnicodeSegmentation;

/// The struct doing the formatting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Bibliography {
    /// Properties shared with the bibliography.
    pub config: ChicagoConfig,
    /// Selects the bibliography mode.
    pub mode: Mode,
}

impl Bibliography {
    /// Create a new Bibliography.
    pub fn new(mode: Mode, config: ChicagoConfig) -> Self {
        Self { config, mode }
    }

    fn get_author(&self, entry: &Entry) -> String {
        let (authors, add) = get_creators(entry);
        if authors.is_empty() {
            return String::new();
        }

        let count = authors.len();
        let authors = authors.into_iter().enumerate().map(|(i, p)| {
            let name =
                if i == 0 { p.name_first(false, true) } else { p.given_first(false) };
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
        });
        let mut res = and_list(authors, true, self.config.et_al_limit);

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

        if res.is_empty() {
            if let Some(creator) = web_creator(entry, true, self.config.et_al_limit) {
                res += &creator;
            } else if self.mode == Mode::AuthorDate
                && matches!(
                    entry.entry_type,
                    Report | Patent | Legislation | Conference | Exhibition
                )
            {
                if let Some(org) = entry.organization() {
                    res += org
                }
            }
        }

        res
    }

    fn get_publication_info(&self, entry: &Entry) -> String {
        let conference = select!(* > ("p":Conference)).bound(entry, "p");
        let mut res =
            if entry.entry_type == Thesis { "Thesis".to_string() } else { String::new() };

        let published_entry = select!(* > ("p":(*["publisher"]))).bound(entry, "p");
        if conference.is_none() {
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
                res += "Preprint";
            } else {
                res += "Unpublished manuscript";
            }
        } else if entry.entry_type == Artwork {
            // Intentionally empty: We do the publisher stuff later
        } else if let Some(conf) = conference {
            let conf_name = get_chunk_title(conf, false, false, &self.config).value;
            if !conf_name.is_empty() {
                res += "Conference Presentation at ";
                res += &conf_name;
            } else {
                res += "Conference presentation";
            }

            if let Some(org) = conf.organization() {
                res += ", ";
                res += org;
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
        }

        let journal = select!((Article | Entry) > Periodical).matches(entry);
        let date = if self.mode == Mode::NotesAndBibliography
            || select!((* > Newspaper) | Tweet | Thread | (* > Thread)).matches(entry)
        {
            entry_date(entry, false)
        } else {
            String::new()
        };

        if !date.is_empty() && !res.is_empty() {
            if !journal || published_entry.is_some() {
                res.push(',');
            }
            res.push(' ');
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

    /// Format a reference.
    pub fn format(
        &self,
        mut entry: &Entry,
        num: Option<usize>,
    ) -> (DisplayString, Vec<Person>) {
        entry = delegate_titled_entry(entry);

        let mut res: DisplayString = if entry.entry_type != Reference
            || entry.publisher().is_some()
            || entry.volume().is_some()
        {
            self.get_author(entry).into()
        } else {
            DisplayString::new()
        };

        let no_author = res.is_empty();

        let ad_date = if self.mode == Mode::AuthorDate {
            let designator = num.map(alph_designator);
            let mut date = String::new();

            date += &entry_date(entry, true);
            if let Some(designator) = designator {
                date.push(designator);
            }

            date
        } else {
            String::new()
        };

        if !no_author && !ad_date.is_empty() {
            if res.last() != Some('.') {
                res.push('.');
            }
            res.push(' ');
            res += &ad_date;
        }

        let dictionary = select!(Entry > ("p": Reference)).bound(entry, "p");
        let database = select!(Entry > ("p": Repository)).bound(entry, "p");
        if (no_author && dictionary.is_some()) || database.is_some() {
            let dictionary = dictionary.or(database).unwrap();
            let title = get_title(dictionary, false, &self.config, '.');
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        } else {
            let mut title = get_title(entry, false, &self.config, '.');
            if entry.entry_type == Case || entry.entry_type == Legislation {
                title.clear_formatting();
            }
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some('.') {
                    res.push('.');
                }

                res.push(' ');
            }
            res += title;
        }

        if no_author && !ad_date.is_empty() {
            if res.last() != Some('.') {
                res.push('.');
            }
            res.push(' ');
            res += &ad_date;
        }

        let mut colon = false;
        let journal =
            select!((Article | Entry) > (Periodical | Newspaper)).matches(entry);

        let dict = if no_author { dictionary.unwrap_or(entry) } else { entry };
        let mut add = get_info_element(dict, &self.config, true);

        add.value = add
            .value
            .graphemes(true)
            .into_iter()
            .enumerate()
            .map(|(i, e)| if i == 0 { e.to_uppercase() } else { e.into() })
            .collect::<Vec<_>>()
            .join("");

        if !add.is_empty() {
            push_comma_quote_aware(&mut res.value, '.', true);
        }
        res += add;

        let publ = self.get_publication_info(dict);
        if !publ.is_empty() && !res.is_empty() {
            push_comma_quote_aware(&mut res.value, '.', true);
        }

        if !publ.is_empty() {
            res += &publ;
        }

        if journal && !publ.is_empty() {
            colon = true;
        }

        if no_author && dictionary.is_some() && entry.authors().is_none() {
            push_comma_quote_aware(&mut res.value, ',', true);
            res += "s.v. ";
            res += get_chunk_title(entry, false, true, &self.config);
        }

        if let Some(pr) = entry.page_range() {
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

        if journal {
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
                if database.is_none() && self.config.url_access_date.needs_date(entry) {
                    res += &format!("accessed {}, ", format_date(date, DateMode::Day));
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
            let mut brack_content = get_chunk_title(entry, false, false, &self.config);
            if let Some(sn) = entry.serial_number() {
                push_comma_quote_aware(&mut brack_content.value, ',', true);
                brack_content += sn;
            }
            if self.config.url_access_date.needs_date(entry) {
                if let Some(date) = entry.url_any().and_then(|u| u.visit_date.as_ref()) {
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

        if !res.is_empty() && res.last() != Some('.') {
            res.push('.');
        }

        (res, get_creators(entry).0)
    }
}
