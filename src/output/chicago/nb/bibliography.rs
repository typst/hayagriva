//! Formats citations as bibliographies.
use crate::output::{
    abbreviate_publisher, delegate_titled_entry, format_range, push_comma_quote_aware,
    DisplayString, Formatting,
};
use crate::selectors::{Bind, Id, Wc};
use crate::types::EntryType::*;
use crate::{attrs, sel, Entry};

use super::{
    super::is_authoritative, and_list, common_author_handling, format_date,
    get_chunk_title, get_info_element, get_title, AuthorRole, CommonChicagoNbConfig,
    DateMode,
};

use unicode_segmentation::UnicodeSegmentation;

/// The struct doing the formatting.
pub struct BibliographyFormatter {
    /// Properties shared with the bibliography.
    pub common: CommonChicagoNbConfig,
}

impl BibliographyFormatter {
    /// Create a new [NoteCitationFormatter].
    pub fn new() -> Self {
        Self { common: CommonChicagoNbConfig::new() }
    }

    fn get_author(&self, entry: &Entry) -> String {
        let (authors, add) = common_author_handling(entry);
        if authors.is_empty() {
            return String::new();
        }

        let count = authors.len();
        let authors = authors.into_iter().enumerate().map(|(i, p)| {
            let name = if i == 0 {
                p.get_name_first(false, true)
            } else {
                p.get_given_name_initials_first(false)
            };
            if entry.entry_type == Tweet {
                if let Some(pseud) = entry.get_twitter_handle(i) {
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
        });
        let mut res = and_list(authors, true, self.common.et_al_limit);

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

        if res.is_empty() {
            let web_thing = sel!(alt
                Id(Web),
                sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
            )
            .apply(entry);
            if let Some(wt) = web_thing {
                let creator = if let Some(org) = entry.get_organization() {
                    org.into()
                } else if wt.get("p").and_then(|e| e.get_authors_fallible()).is_some() {
                    self.get_author(wt.get("p").unwrap())
                } else if let Some(org) = wt.get("p").and_then(|e| e.get_organization()) {
                    org.into()
                } else {
                    "".into()
                };

                if !res.is_empty() && !creator.is_empty() {
                    res += ", ";
                }

                res += &creator;
            }
        }

        res
    }

    fn get_publication_info(&self, entry: &Entry) -> String {
        let conference =
            sel!(Wc() => Bind("p", Id(Conference))).bound_element(entry, "p");
        let mut res = if entry.entry_type == Thesis {
            "Thesis".to_string()
        } else {
            String::new()
        };

        let published_entry =
            sel!(Wc() => Bind("p", attrs!(Wc(), "publisher"))).bound_element(entry, "p");
        if !conference.is_some() {
            if let Some(loc) = entry
                .get_location()
                .or_else(|| published_entry.and_then(|e| e.get_location()))
            {
                if !res.is_empty() {
                    res += ", ";
                }
                res += loc;
            } else if matches!(&entry.entry_type, Book | Anthology)
                && entry.get_any_date().map(|d| d.year).unwrap_or(2020) < 1981
            {
                if !res.is_empty() {
                    res += ", ";
                }
                res += "n.p.";
            }
        }

        if entry.entry_type == Tweet {
            if let Some(host) = entry
                .get_any_url()
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
        let preprint = sel!(Id(Article) => Id(Repository)).matches(entry);
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
            let conf_name = get_chunk_title(conf, false, false, &self.common).value;
            if !conf_name.is_empty() {
                res += "Conference Presentation at ";
                res += &conf_name;
            } else {
                res += "Conference presentation";
            }

            if let Some(org) = conf.get_organization() {
                res += ", ";
                res += org;
            }

            if let Some(loc) = conf.get_location() {
                res += ", ";
                res += loc;
            }
        } else if let Some(publisher) = entry
            .get_publisher()
            .or_else(|| published_entry.map(|e| e.get_publisher().unwrap()))
            .or_else(|| {
                if matches!(&entry.entry_type, Report | Thesis)
                    || (matches!(&entry.entry_type, Case | Legislation)
                        && entry.get_serial_number().is_some())
                {
                    entry.get_organization()
                } else {
                    None
                }
            })
            .map(Into::into)
            .or_else(|| {
                if entry.entry_type == Reference && entry.get_volume().is_none() {
                    entry.get_authors_fallible().map(|a| {
                        and_list(
                            a.into_iter().map(|p| p.get_given_name_initials_first(false)),
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
        }

        let journal = sel!(
            sel!(alt Id(Article), Id(Entry)) => Id(Periodical)
        )
        .matches(entry);
        let date = if let Some(date) = entry.get_any_date() {
            let conf = sel!(alt
                sel!(Wc() => Id(Conference)),
                Id(Conference),
                Id(Exhibition),
            )
            .matches(entry);

            let mut mode = match (journal, is_authoritative(entry)) {
                (true, _) => DateMode::Month,
                (false, true) => DateMode::Year,
                _ => DateMode::Day,
            };

            if conf {
                mode = DateMode::Day;
            }

            let date = format_date(date, mode);
            if journal { format!("({})", date) } else { date }
        } else if matches!(&entry.entry_type, Book | Anthology)
            && entry.get_any_url().and_then(|url| url.visit_date.as_ref()).is_none()
        {
            "n.d.".to_string()
        } else {
            String::new()
        };

        if !date.is_empty() && !res.is_empty() {
            if !journal {
                res.push(',');
            }
            res.push(' ');
        }
        res += &date;

        if entry.entry_type == Artwork {
            let mut items: Vec<String> = vec![];

            items.extend(entry.get_note().map(Into::into));

            let parent =
                sel!(Wc() => Bind("p", Id(Exhibition))).bound_element(entry, "p");
            items.extend(
                entry
                    .get_organization()
                    .or_else(|| entry.get_publisher())
                    .or_else(|| parent.and_then(|p| p.get_organization()))
                    .or_else(|| parent.and_then(|p| p.get_publisher()))
                    .map(Into::into),
            );

            items.extend(
                entry
                    .get_location()
                    .or_else(|| parent.and_then(|p| p.get_location()))
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
    pub fn format(&self, mut entry: &Entry) -> DisplayString {
        entry = delegate_titled_entry(entry);

        let mut res: DisplayString = if entry.entry_type != Reference
            || entry.get_publisher().is_some()
            || entry.get_volume().is_some()
        {
            self.get_author(entry).into()
        } else {
            DisplayString::new()
        };

        let no_author = res.is_empty();

        let dictionary =
            sel!(Id(Entry) => Bind("p", Id(Reference))).bound_element(entry, "p");
        let database =
            sel!(Id(Entry) => Bind("p", Id(Repository))).bound_element(entry, "p");
        if (no_author && dictionary.is_some()) || database.is_some() {
            let dictionary = dictionary.or(database).unwrap();
            let title = get_title(dictionary, false, &self.common, '.');
            if !res.is_empty() && !title.is_empty() {
                if res.last() != Some(',') {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        } else {
            let mut title = get_title(entry, false, &self.common, '.');
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

        let mut colon = false;
        let journal = sel!(
            sel!(alt Id(Article), Id(Entry)) => sel!(alt Id(Periodical), Id(Newspaper))
        )
        .matches(entry);

        let dict = if no_author { dictionary.unwrap_or(entry) } else { entry };
        let mut add = get_info_element(dict, &self.common, true);

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

        if no_author && dictionary.is_some() && entry.get_authors_fallible().is_none() {
            push_comma_quote_aware(&mut res.value, ',', true);
            res += "s.v. ";
            res += get_chunk_title(entry, false, true, &self.common);
        }

        if let Some(pr) = entry.get_page_range() {
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
            if let Some(sn) = entry.get_serial_number() {
                if !sn.is_empty() {
                    push_comma_quote_aware(&mut res.value, ',', false);
                }

                if !sn.is_empty() && !res.is_empty() {
                    res.push(' ');
                }

                res += sn;
            }
        }

        let url = if let Some(doi) = entry.get_doi() {
            let mut res = DisplayString::new();
            res.start_format(Formatting::NoHyphenation);
            res += &format!("https://doi.org/{}", doi);
            res.commit_formats();
            res
        } else if let Some(qurl) = entry.get_any_url() {
            let mut res = DisplayString::new();
            if let Some(date) = qurl.visit_date.as_ref() {
                if database.is_none() && self.common.url_access_date.needs_date(entry) {
                    res += &format!("accessed {}, ", format_date(date, DateMode::Day));
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
            let mut brack_content = get_chunk_title(entry, false, false, &self.common);
            if let Some(sn) = entry.get_serial_number() {
                push_comma_quote_aware(&mut brack_content.value, ',', true);
                brack_content += sn;
            }
            if self.common.url_access_date.needs_date(entry) {
                if let Some(date) =
                    entry.get_any_url().and_then(|u| u.visit_date.as_ref())
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

        let preprint = sel!(Id(Article) => Id(Repository)).matches(entry);
        if no_url || entry.entry_type == Manuscript || preprint {
            if let Some(archive) = entry.get_archive() {
                push_comma_quote_aware(&mut res.value, ',', true);

                res += archive;

                if let Some(al) = entry.get_archive_location() {
                    res += ", ";
                    res += al;
                }
            }
        }

        if !res.is_empty() && res.last() != Some('.') {
            res.push('.');
        }

        res
    }
}
