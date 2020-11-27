//! Formats citations as footnotes.
use crate::lang::{en::get_ordinal, TitleCase};
use crate::output::{format_range, push_comma_quote_aware, DisplayString, Formatting};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{NumOrStr, PersonRole};
use crate::{attrs, sel, Entry};
use std::collections::HashMap;

use super::shorthand;

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

        kinds.contains(&entry.entry_type)
            || sel!(alt
                attrs!(Wc(), "editor"),
                attrs!(Wc(), "publisher"),
                sel!(Wc() => Neg(Wc())),
            )
            .apply(entry)
            .is_some()
    }

    fn get_title(&self, entry: &Entry, short: bool) -> DisplayString {
        let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());


        let mut res = DisplayString::new();
        if let Some(parent) = mv_title {
            res += self.get_chunk_title(parent, short);
        } else {
            res += self.get_chunk_title(entry, short);
        }

        if !short && mv_title.is_none() {
            let chapter = sel!(Id(Chapter) => Bind("p", attrs!(Wc(), "title")))
                .apply(entry)
                .map(|mut hm| hm.remove("p").unwrap());

            let edited_book = sel!(alt
                sel!(Wc() => Bind("p", attrs!(Id(Book), "editor"))),
                sel!(sel!(alt Id(Anthos), Id(Entry)) => Bind("p", sel!(alt Id(Anthology), Id(Reference))))
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
                res += self.get_chunk_title(parent, false);
            }
        }

        res
    }

    fn get_info_element(&self, mut entry: &Entry) -> DisplayString {
        let mut res = vec![];
        let journal = sel!(
            sel!(alt Id(Article), Id(Entry)) => Bind("p", Id(Periodical))
        )
        .apply(entry)
        .map(|mut hm| hm.remove("p").unwrap());

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

        if let Some(journal) = journal {
            let mut local = self.get_chunk_title(journal, false);
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
            res.push(local)
        }

        let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());

        let orig_entry = entry;
        if let Some(mv_p) = mv_title {
            entry = mv_p;
        }

        if let Some(eds) = eds {
            let ed_names = eds
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>();

            let mut local = if ed_names.len() > 1 { "eds. " } else { "ed. " }.to_string();

            local += &self.and_list(ed_names, false);
            res.push(local.into())
        }

        if let Some(trans) = translators {
            let trans_names = trans
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>();

            let mut local = "trans. ".to_string();

            local += &self.and_list(trans_names, false);
            res.push(local.into())
        }

        if let Some(comp) = compilers {
            let comp_names = comp
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>();

            let mut local =
                if comp_names.len() > 1 { "comps. " } else { "comp. " }.to_string();

            local += &self.and_list(comp_names, false);
            res.push(local.into())
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

            if let Some(parent) = mv_title {
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
                let mut title = self.get_chunk_title(par_anth, false).value.into();

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

    fn get_chunk_title(&self, entry: &Entry, short: bool) -> DisplayString {
        let mut res = DisplayString::new();
        let sc = NoteCitationFormatter::self_contained(entry);

        if sc {
            res.start_format(Formatting::Italic);
        } else {
            res += "“";
        }

        if short {
            if let Some(title) = entry.get_title_raw().map(|t| shorthand(t)) {
                res += &title.format(Some(&self.tc_formatter), None).title_case;
            }
        } else {
            if let Some(title) = entry.get_title_fmt(Some(&self.tc_formatter), None) {
                res += &title.value.title_case;
            }
        }

        if sc {
            res.commit_formats();
        } else {
            res += "”";
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
                .map(|p| {
                    let name = p.get_given_name_initials_first(false);
                    if let Some(pseud) = p.alias {
                        format!("{} [{}]", pseud, name)
                    } else {
                        name
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

    /// Format a citation as a note.
    pub fn get_note(&self, key: &str, kind: NoteType) -> Option<DisplayString> {
        let short = kind != NoteType::Full;
        let entry = self.entries.get(key)?;
        let mut res: DisplayString = self.get_author(entry, short).into();
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

        if !short {
            let add = self.get_info_element(entry);
            if !add.is_empty() {
                res.value = push_comma_quote_aware(res.value, ',', true);
            }
            res += add;
        }

        Some(res)
    }
}
