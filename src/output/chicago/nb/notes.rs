//! Formats citations as footnotes.
use crate::lang::{en::get_month_name, en::get_ordinal, TitleCase};
use crate::output::{
    abbreviate_publisher, format_range, push_comma_quote_aware, AtomicCitation,
    DisplayString, Formatting,
};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{Date, NumOrStr, PersonRole};
use crate::{attrs, sel, Entry};
use std::collections::HashMap;

use super::shorthand;
use super::{super::is_authoritative, super::AccessDateConfig, omit_initial_articles};

/// Which parts of the day should be printed.
#[derive(Copy, Clone, Debug, PartialEq)]
enum DateMode {
    Year,
    Month,
    Day,
}

fn format_date(date: &Date, mode: DateMode) -> String {
    let mut res = String::new();

    if mode != DateMode::Year {
        let day = if mode == DateMode::Day { date.day } else { None };

        if let Some(month) = date.month {
            res.push_str(&if let Some(day) = day {
                format!("{} {}, ", get_month_name(month).unwrap(), day + 1)
            } else {
                format!("{} ", get_month_name(month).unwrap())
            });
        }
    }

    if date.year > 0 {
        res += &date.year.to_string();
    } else {
        res += &format!("{} B.C.E.", -(date.year - 1));
    }
    res
}

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
    /// When to print URL access dates.
    pub url_access_date: AccessDateConfig,
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
            url_access_date: AccessDateConfig::NotFormallyPublished,
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

    /// Indicates whether a kind of work is its own container.
    fn self_contained(entry: &Entry) -> bool {
        let kinds = [
            Book,
            Report,
            Thesis,
            Newspaper,
            Periodical,
            Proceedings,
            Book,
            Blog,
            Reference,
            Conference,
            Anthology,
            Thread,
            Exhibition,
        ];

        let always_quote = [Web];

        kinds.contains(&entry.entry_type)
            || (!always_quote.contains(&entry.entry_type)
                && sel!(alt
                    attrs!(Wc(), "publisher"),
                    sel!(Wc() => Neg(Wc())),
                )
                .apply(entry)
                .is_some())
    }

    fn get_title(&self, entry: &Entry, short: bool) -> DisplayString {
        let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());


        let mut res = DisplayString::new();
        if let Some(parent) = mv_title {
            res += self.get_chunk_title(parent, short, true);
        } else {
            res += self.get_chunk_title(entry, short, true);
        }

        if !short && mv_title.is_none() {
            let chapter = sel!(Id(Chapter) => Bind("p", attrs!(Wc(), "title")))
                .apply(entry)
                .map(|mut hm| hm.remove("p").unwrap());

            let edited_book = sel!(alt
                sel!(Wc() => Bind("p", attrs!(sel!(alt Id(Book), Id(Proceedings)), "editor"))),
                sel!(sel!(alt Id(Anthos), Id(Entry)) => Bind("p", sel!(alt Id(Anthology), Id(Reference)))),
                sel!(Id(Article) => Bind("p", Id(Proceedings))),
            ).apply(entry).map(|mut hm| hm.remove("p").unwrap());

            if let Some(parent) = edited_book.or(chapter) {
                res.value = push_comma_quote_aware(res.value, ',', true);
                if chapter.is_some() {
                    if let Some(sn) = entry.get_serial_number() {
                        let num = sn.chars().all(char::is_numeric);
                        if num {
                            res += &format!("chap. {} ", sn);
                        } else {
                            res += sn;
                            res.push(' ');
                        }
                    }
                }

                res += "in ";
                res += self.get_chunk_title(parent, false, true);
            }

            let web_parent = sel!(alt
                sel!(Id(Web) => Bind("p", Wc())),
                sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
            )
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());

            if web_parent.and_then(|p| p.get_title()).is_some() {
                res.value = push_comma_quote_aware(res.value, ',', true);
                res += &self.get_chunk_title(web_parent.unwrap(), false, false).value;
            }

            let blog_parent = sel!(Wc() => Bind("p", Id(Blog)))
                .apply(entry)
                .map(|mut hm| hm.remove("p").unwrap());
            if let Some(parent) = blog_parent {
                let titles = self.get_title(parent, false);

                if !titles.is_empty() {
                    res.value = push_comma_quote_aware(res.value, ',', true);
                }

                res += titles;
            } else if entry.entry_type == Blog {
                let title_parent = sel!(Wc() => Bind("p", attrs!(Wc(), "title")))
                    .apply(entry)
                    .map(|mut hm| hm.remove("p").unwrap());
                if let Some(parent) = title_parent {
                    let titles = self.get_title(parent, false);
                    res.value = push_comma_quote_aware(res.value, ',', true);
                    res += titles;
                }
            }
        }

        res
    }

    fn get_info_element(&self, mut entry: &Entry) -> DisplayString {
        let mut res = vec![];
        let journal = sel!(
            sel!(alt Id(Article), Id(Entry)) => Bind("p", sel!(alt Id(Periodical), Id(Newspaper)))
        ).apply(entry).map(|mut hm| hm.remove("p").unwrap());
        let newspaper = sel!(Wc() => Bind("p", Id(Newspaper)))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());

        let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());

        let orig_entry = entry;
        if let Some(mv_p) = mv_title {
            entry = mv_p;
        }

        let affs = sel!(alt
            Bind("tl", attrs!(Wc(), "affiliated")),
            sel!(Wc() => Bind("tl", attrs!(Wc(), "affiliated"))),
        )
        .apply(entry)
        .map(|mut hm| hm.remove("tl").unwrap());

        let eds = sel!(alt
            Bind("ed", attrs!(Wc(), "editor")),
            sel!(Wc() => Bind("ed", attrs!(Wc(), "editor"))),
        )
        .apply(entry)
        .map(|mut hm| hm.remove("ed").unwrap().get_editors().unwrap());

        let translators = affs.and_then(|e| {
            let ts = e.get_affiliated_filtered(PersonRole::Translator);
            if ts.is_empty() { None } else { Some(ts) }
        });

        let compilers = affs.and_then(|e| {
            let ts = e.get_affiliated_filtered(PersonRole::Compiler);
            if ts.is_empty() { None } else { Some(ts) }
        });

        if let Some(trans) = translators {
            if orig_entry.get_authors_fallible().is_some()
                || orig_entry.get_editors().is_some()
            {
                let trans_names = trans
                    .into_iter()
                    .map(|p| p.get_given_name_initials_first(false))
                    .collect::<Vec<_>>();

                let mut local = "trans. ".to_string();

                local += &self.and_list(trans_names, false);
                res.push(local.into())
            }
        }

        if let Some(eds) = eds {
            if orig_entry.get_authors_fallible().is_some() {
                let ed_names = eds
                    .into_iter()
                    .map(|p| p.get_given_name_initials_first(false))
                    .collect::<Vec<_>>();

                let mut local =
                    if ed_names.len() > 1 { "eds. " } else { "ed. " }.to_string();

                local += &self.and_list(ed_names, false);
                res.push(local.into())
            }
        }

        if let Some(journal) = journal {
            if let Some(volume) = entry.get_volume() {
                res.push(format_range("pt.", "pts.", volume).into());
            }

            let mut local = self.get_chunk_title(journal, false, true);

            if let Some(paper) = newspaper {
                if let Some(location) = paper.get_location() {
                    if !local.is_empty() {
                        local.push(' ');
                    }

                    local += &format!("({})", location);
                }
            }

            if let Some(ed) = journal.get_edition() {
                let applied = match ed {
                    NumOrStr::Number(n) if *n > 1 => format!("{} ser.", get_ordinal(*n)),
                    NumOrStr::Str(s) => {
                        let s =
                            s.to_lowercase().replace("new", "n.").replace("series", "s.");
                        let s = if s.trim() == "n. s." { "n.s.".into() } else { s };

                        s
                    }
                    _ => String::new(),
                };

                if !applied.is_empty() {
                    if !local.is_empty() {
                        local += ", ";
                    }

                    local += &applied;

                    if journal.get_volume().is_some() {
                        local.push(',')
                    }
                }
            }

            if let Some(vol) = journal.get_volume() {
                if !local.is_empty() {
                    local.push(' ');
                }
                local += &format_range("", "", vol);
            }

            if let Some(iss) = journal.get_issue() {
                if !local.is_empty() {
                    local += ", ";
                }

                local += &match iss {
                    NumOrStr::Number(i) => format!("no. {}", i),
                    NumOrStr::Str(s) => s.clone(),
                };
            }
            res.push(local);
        }

        if let Some(comp) = compilers {
            if orig_entry.get_authors_fallible().is_some()
                || orig_entry.get_editors().is_some()
            {
                let comp_names = comp
                    .into_iter()
                    .map(|p| p.get_given_name_initials_first(false))
                    .collect::<Vec<_>>();

                let mut local =
                    if comp_names.len() > 1 { "comps. " } else { "comp. " }.to_string();

                local += &self.and_list(comp_names, false);
                res.push(local.into())
            }
        }

        if let Some(edition) = entry.get_edition() {
            match edition {
                NumOrStr::Number(i) if *i <= 1 => {}
                NumOrStr::Number(i) => {
                    res.push(format!("{} ed.", get_ordinal(*i)).into())
                }
                NumOrStr::Str(s) => {
                    res.push(
                        s.split(' ')
                            .map(|i| match i.to_lowercase().as_ref() {
                                "edition" => "ed.",
                                "revised" => "rev.",
                                _ => i,
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                            .into(),
                    );
                }
            }
        }

        if journal.is_none() {
            if let Some(vols) = orig_entry.get_volume() {
                res.push(format_range("vol.", "vols.", vols).into())
            }

            if let Some(vtotal) = orig_entry.get_total_volumes() {
                if *vtotal > 1 {
                    res.push(format!("{} vols.", vtotal).into())
                }
            }

            if let Some(_parent) = mv_title {
                let mut title = DisplayString::new();
                title.start_format(Formatting::Italic);
                if let Some(own_title) =
                    orig_entry.get_title_fmt(Some(&self.tc_formatter), None)
                {
                    title += &own_title.value.title_case;
                }
                if let Some(eds) = orig_entry.get_editors() {
                    title.value = push_comma_quote_aware(title.value, ',', true);
                    title.commit_formats();
                    let ed_names = eds
                        .into_iter()
                        .map(|p| p.get_given_name_initials_first(false))
                        .collect::<Vec<_>>();

                    let mut local =
                        if ed_names.len() > 1 { "eds. " } else { "ed. " }.to_string();

                    local += &self.and_list(ed_names, false);
                    title += &local;
                } else {
                    title.commit_formats();
                }
                res.push(title);
            }
        }

        // Series titles: Chicago 14.123
        if mv_title.is_none() {
            let spec = sel!(alt
                sel!(Wc() => sel!(Id(Anthology) => Bind("p", attrs!(Id(Anthology), "title")))),
                sel!(Wc() => sel!(Id(Book) => Bind("p", attrs!(Id(Book), "title")))),
                sel!(Id(Book) => Bind("p", attrs!(Wc(), "title"))),
            );
            if let Some(mut hm) = spec.apply(orig_entry) {
                let par_anth = hm.remove("p").unwrap();
                let mut title = self.get_chunk_title(par_anth, false, false).value.into();

                let issue = if let Some(issue) = par_anth.get_issue() {
                    Some(match issue {
                        NumOrStr::Str(s) => (s.clone(), None),
                        NumOrStr::Number(i) => (format!("no. {}", i), Some(*i)),
                    })
                } else {
                    None
                };

                let volume = par_anth.get_volume().map(|v| {
                    let val = if v.start == v.end { Some(v.start) } else { None };
                    (format_range("vol.", "vols.", v), val)
                });

                if issue.is_some() && volume.is_some() {
                    res.push(title);
                    res.push(volume.unwrap().0.into());
                    res.push(issue.unwrap().0.into());
                } else {
                    let item = issue.or(volume);
                    if let Some((_, Some(v))) = item {
                        title.push(' ');
                        title += &v.to_string();
                        res.push(title);
                    } else if let Some((s, _)) = item {
                        res.push(title);
                        res.push(s.into());
                    } else {
                        res.push(title);
                    }
                }
            }
        }

        DisplayString::join(&res, ", ")
    }

    fn get_chunk_title(&self, entry: &Entry, short: bool, fmt: bool) -> DisplayString {
        let mut res = DisplayString::new();
        let sc = fmt && NoteCitationFormatter::self_contained(entry);

        if sc {
            res.start_format(Formatting::Italic);
        } else if fmt {
            res += "“";
        }

        let np = entry.entry_type == Newspaper;

        if short {
            if let Some(title) = entry.get_title_raw().map(|t| shorthand(t)) {
                res += &title.format(Some(&self.tc_formatter), None).title_case;
            }
        } else {
            if let Some(title) = entry.get_title_fmt(Some(&self.tc_formatter), None) {
                res += &title.value.title_case;
            }
        }

        if np {
            res.value = omit_initial_articles(&res.value);
        }

        if sc {
            res.commit_formats();
        } else if fmt {
            res += "”";
        }

        if !short && entry.entry_type == Blog {
            let brackets = !res.is_empty();
            if brackets {
                res.push(' ');
                res.push('(');
            }
            res += "blog";
            if brackets {
                res.push(')');
            }
        }

        res
    }

    fn get_author(&self, entry: &Entry, short: bool) -> String {
        enum AuthorRole {
            Normal,
            Editor,
            Compiler,
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
            let compilers = entry.get_affiliated_filtered(PersonRole::Compiler);
            let translators = entry.get_affiliated_filtered(PersonRole::Translator);
            if !compilers.is_empty() {
                add = AuthorRole::Compiler;
                compilers
            } else if !translators.is_empty() {
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
                .enumerate()
                .map(|(i, p)| {
                    let name = p.get_given_name_initials_first(false);
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
                })
                .collect::<Vec<_>>()
        };
        let mut res = self.and_list(authors, false);

        if !short {
            let add = match add {
                AuthorRole::Editor if count > 1 => "eds.",
                AuthorRole::Editor => "ed.",
                AuthorRole::Compiler if count > 1 => "comps.",
                AuthorRole::Compiler => "comp.",
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

    fn get_publication_info(&self, entry: &Entry) -> String {
        let mut res = String::new();
        let published_entry = sel!(Wc() => Bind("p", attrs!(Wc(), "publisher")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());
        if let Some(loc) = entry
            .get_location()
            .or_else(|| published_entry.and_then(|e| e.get_location()))
        {
            res += loc;
        } else if [Book, Anthology].contains(&entry.entry_type)
            && entry.get_any_date().map(|d| d.year).unwrap_or(2020) < 1981
        {
            res += "n.p.";
        }

        let web_thing = sel!(alt
            Id(Web),
            sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
        )
        .apply(entry);

        if entry.entry_type == Tweet {
            if let Some(host) = entry.get_any_url().and_then(|u| u.value.host_str()).map(|h| h.to_lowercase()) {
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

        if let Some(publisher) = entry
            .get_publisher()
            .or_else(|| published_entry.map(|e| e.get_publisher().unwrap()))
            .map(|p| abbreviate_publisher(p, false))
        {
            if !res.is_empty() {
                res += ": ";
            }
            res += &publisher;
        } else if let Some(wt) = web_thing {
            let creator = if entry.get_authors_fallible().is_some() {
                self.get_author(entry, false)
            } else if let Some(org) = entry.get_organization() {
                org.into()
            } else if wt.get("p").and_then(|e| e.get_authors_fallible()).is_some() {
                self.get_author(wt.get("p").unwrap(), false)
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

        let date = if let Some(date) = entry.get_any_date() {
            let journal = sel!(
                sel!(alt Id(Article), Id(Entry)) => Id(Periodical)
            )
            .apply(entry)
            .is_some();

            let mode = match (journal, is_authoritative(entry)) {
                (true, _) => DateMode::Month,
                (false, true) => DateMode::Year,
                _ => DateMode::Day,
            };

            format_date(date, mode)
        } else if [Book, Anthology].contains(&entry.entry_type)
            && entry.get_any_url().and_then(|url| url.visit_date.as_ref()).is_none()
        {
            "n.d.".to_string()
        } else {
            String::new()
        };

        if !date.is_empty() && !res.is_empty() {
            res += ", ";
        }
        res += &date;

        res
    }

    /// Format a citation as a note.
    pub fn get_note(
        &self,
        citation: AtomicCitation,
        kind: NoteType,
    ) -> Option<DisplayString> {
        let short = kind != NoteType::Full;
        let &entry = self.entries.get(citation.key)?;

        let web_thing = sel!(alt
            Id(Web),
            sel!(sel!(alt Id(Misc), Id(Web)) => Id(Web)),
        )
        .apply(entry)
        .is_some();

        let mut res: DisplayString = if !web_thing || short {
            self.get_author(entry, short).into()
        } else {
            DisplayString::new()
        };

        if kind != NoteType::OnlyAuthor {
            let title = self.get_title(entry, short);
            if !res.is_empty() && !title.is_empty() {
                if res.last().unwrap_or('a') != ',' {
                    res.push(',');
                }

                res.push(' ');
            }
            res += title;
        }

        let mut colon = false;
        let journal = sel!(
            sel!(alt Id(Article), Id(Entry)) => sel!(alt Id(Periodical), Id(Newspaper))
        )
        .apply(entry)
        .is_some();

        if !short {
            let add = self.get_info_element(entry);
            if !add.is_empty() {
                res.value = push_comma_quote_aware(res.value, ',', true);
            }
            res += add;

            let publ = self.get_publication_info(entry);
            let brackets = is_authoritative(entry);
            if !publ.is_empty() && !res.is_empty() {
                if brackets {
                    res.push(' ');
                } else {
                    res.value = push_comma_quote_aware(res.value, ',', true);
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
        }

        if let Some(supplement) = citation.supplement {
            if !res.is_empty() {
                if colon {
                    res.push(':');
                } else {
                    res.value = push_comma_quote_aware(res.value, ',', false);
                }
                res.push(' ');
            }

            res += supplement;
        } else if let Some(pr) = entry.get_page_range() {
            if !res.is_empty() {
                if colon {
                    res.push(':');
                } else {
                    res.value = push_comma_quote_aware(res.value, ',', false);
                }
                res.push(' ');
            }

            res += &format_range("", "", pr);
        }

        if journal && !short {
            if let Some(sn) = entry.get_serial_number() {
                if !sn.is_empty() {
                    res.value = push_comma_quote_aware(res.value, ',', false);
                }

                if !sn.is_empty() && !res.is_empty() {
                    res.push(' ');
                }

                res += sn;
            }
        }

        if !short {
            let url = if let Some(doi) = entry.get_doi() {
                format!("https://doi.org/{}", doi)
            } else if let Some(qurl) = entry.get_any_url() {
                let mut res = String::new();
                if let Some(date) = qurl.visit_date.as_ref() {
                    if self.url_access_date.needs_date(entry) {
                        res +=
                            &format!("accessed {}, ", format_date(date, DateMode::Day));
                    }
                }

                res += qurl.value.as_str();
                res
            } else {
                String::new()
            };

            if journal {
                if !url.is_empty() && !res.is_empty() && res.last() != Some('.') {
                    res.push('.');
                }
            } else {
                if !url.is_empty() {
                    res.value = push_comma_quote_aware(res.value, ',', false);
                }
            }

            if !url.is_empty() && !res.is_empty() {
                res.push(' ');
            }

            res.start_format(Formatting::NoHyphenation);
            res += &url;
            res.commit_formats();

            if url.is_empty() {
                if let Some(archive) = entry.get_archive() {
                    res.value = push_comma_quote_aware(res.value, ',', true);

                    res += archive;

                    if let Some(al) = entry.get_archive_location() {
                        res += ", ";
                        res += al;
                    }
                }
            }
        }

        if !res.is_empty() && res.last() != Some('.') {
            res.push('.');
        }

        Some(res)
    }
}
