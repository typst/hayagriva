//! Both the author / date and notes / bibliograpghy citation styles as defined
//! in the 17th edition of the Chicago Manual of Style.

pub mod author_date;
pub mod bibliography;
pub mod notes;

use super::{format_range, push_comma_quote_aware, DisplayString, Formatting};
use crate::lang::{en::get_month_name, en::get_ordinal, SentenceCase, TitleCase};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{Date, FormattableString, NumOrStr, Person, PersonRole, Title};
use crate::{attrs, sel, Entry};

use isolang::Language;

/// Determines the mode of the bibliography and citation.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mode {
    /// Use references in footnotes in addition to a detailed bibliography.
    /// See chap. 14 of the CMoS.
    NotesAndBibliography,
    /// Use short bracketed author date expressions to refer to a detailed
    /// bibliography. See chap. 15 of the CMoS.
    AuthorDate,
}

/// Configure when to print access dates. Also see Chicago 14.12.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AccessDateConfig {
    /// Never print access dates.
    Never,
    /// Print if no other date was found.
    NoDate,
    /// Print if no other date was found or if this is a inherently online
    /// media item like `Web`, `Blog`, or `Repository`.
    InherentlyOnline,
    /// Print if no other date was found or if the media item is not
    /// traditionally formally published, like `Thesis`, `Manuscript`, `Report`.
    NotFormallyPublished,
    /// Always print access dates.
    Always,
}

impl AccessDateConfig {
    /// Will determine, given the value of `self` and a [`Entry`], if an access
    /// date should be provided.
    pub fn needs_date(&self, entry: &Entry) -> bool {
        match self {
            AccessDateConfig::Never => false,
            AccessDateConfig::NoDate => entry.any_date().is_none(),
            AccessDateConfig::InherentlyOnline => {
                sel!(alt
                    Id(Web),
                    sel!(sel!(alt Id(Misc), Id(Web)) => Id(Web)),
                )
                .matches(entry)
                    || entry.any_date().is_none()
            }
            AccessDateConfig::NotFormallyPublished => {
                !is_formally_published(entry) || entry.any_date().is_none()
            }
            AccessDateConfig::Always => true,
        }
    }
}

fn is_authoritative(entry: &Entry) -> bool {
    sel!(alt
        Id(Book),
        Id(Proceedings),
        attrs!(Id(Periodical), "volume"),
        attrs!(Id(Periodical), "issue"),
        Id(Patent),
        Id(Thesis),
        Id(Report),
        Id(Reference),
        sel!(Wc() => Id(Conference)),
        sel!(Wc() => Id(Book)),
        sel!(Wc() => Id(Proceedings)),
        sel!(Wc() => attrs!(Id(Periodical), "volume")),
        sel!(Wc() => attrs!(Id(Periodical), "issue")),
        sel!(Id(Article) => Id(Anthology)),
        Id(Case),
        Id(Legislation),
    )
    .matches(entry)
}

fn is_formally_published(entry: &Entry) -> bool {
    sel!(alt
        Id(Book),
        Id(Proceedings),
        Id(Periodical),
        Id(Patent),
        Id(Case),
        Id(Legislation),
        Id(Newspaper),
        sel!(Neg(Id(Manuscript)) => attrs!(Wc(), "publisher")),
        sel!(Wc() => Id(Book)),
        sel!(Wc() => Id(Proceedings)),
        sel!(Wc() => Id(Periodical)),
        sel!(Id(Article) => Id(Newspaper)),
        sel!(sel!(alt Id(Article), Id(Anthos)) => Id(Anthology)),
    )
    .matches(entry)
}

/// Settings that all Chicago styles need.
pub struct CommonChicagoConfig {
    /// If there is greater or equal to this number of authors, they will be
    /// abbreviated using et. al. after the first name.
    pub et_al_limit: Option<usize>,
    /// When to print URL access dates.
    pub url_access_date: AccessDateConfig,
    tc_formatter: TitleCase,
    sc_formatter: SentenceCase,
}

impl CommonChicagoConfig {
    /// Create a new [`CommonChicagoConfig`].
    pub fn new() -> Self {
        let tc_formatter = TitleCase::new();
        Self {
            tc_formatter,
            sc_formatter: SentenceCase::new(),
            et_al_limit: Some(4),
            url_access_date: AccessDateConfig::NotFormallyPublished,
        }
    }
}

/// Describes the role a creator had in terms of what Chicago accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthorRole {
    /// Think about this as an ordinary author.
    Normal,
    /// This person was responsible for the collection and editing of the work.
    Editor,
    /// The work consists of several smaller
    /// items which were compiled by the creator.
    Compiler,
    /// This person translated the work from another language.
    Translator,
    /// This person directed the production of the work.
    Director,
    /// This person oversaw the production of the work creatively.
    ExecutiveProducer,
}

/// Get the creator of an entry.
pub fn get_creators(entry: &Entry) -> (Vec<Person>, AuthorRole) {
    let mut add = AuthorRole::Normal;
    let authors = if let Some(authors) = entry.authors_fallible() {
        authors.to_vec()
    } else if let Some(eds) = entry.editors() {
        add = AuthorRole::Editor;
        eds.to_vec()
    } else if entry.entry_type == Video {
        let tv_series =
            sel!(attrs!(Id(Video), "issue", "volume") => Bind("p", Id(Video)));
        if let Some(mut bindings) = tv_series.apply(entry) {
            let mut affs = entry.affiliated_filtered(PersonRole::Director);
            affs.extend(entry.affiliated_filtered(PersonRole::Writer));
            if !affs.is_empty() {
                affs
            } else {
                let parent = bindings.remove("p").unwrap();
                add = AuthorRole::ExecutiveProducer;
                parent.affiliated_filtered(PersonRole::ExecutiveProducer)
            }
        } else {
            let dir = entry.affiliated_filtered(PersonRole::Director);
            if !dir.is_empty() {
                add = AuthorRole::Director;
                dir
            } else {
                add = AuthorRole::ExecutiveProducer;
                entry.affiliated_filtered(PersonRole::ExecutiveProducer)
            }
        }
    } else {
        let compilers = entry.affiliated_filtered(PersonRole::Compiler);
        let translators = entry.affiliated_filtered(PersonRole::Translator);
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

    (authors, add)
}

pub(super) fn and_list_opt(
    names: impl IntoIterator<Item = String>,
    oxford: bool,
    et_al_limit: Option<usize>,
    et_al_items: usize,
) -> String {
    let names = names.into_iter().collect::<Vec<_>>();
    let name_len = names.len();
    let mut res = String::new();
    let threshold = et_al_limit.unwrap_or(0);

    for (index, name) in names.into_iter().enumerate() {
        if threshold > 0 && index > et_al_items && name_len >= threshold {
            break;
        }

        res += &name;

        if index + 2 <= name_len
            && (threshold == 0 || name_len < threshold || (index < et_al_items))
        {
            if oxford || name_len > 2 {
                res.push(',');
            }
            res.push(' ');
        }
        if index + 2 == name_len && !(threshold > 0 && name_len >= threshold) {
            res += "and ";
        }
    }

    if threshold > 0 && name_len >= threshold {
        if et_al_items > 0 {
            res.push(',');
        }
        res += " et al."
    }

    res
}

pub(super) fn and_list(
    names: impl IntoIterator<Item = String>,
    oxford: bool,
    et_al_limit: Option<usize>,
) -> String {
    and_list_opt(names, oxford, et_al_limit, 0)
}

fn get_title(
    entry: &Entry,
    short: bool,
    common: &CommonChicagoConfig,
    comma: char,
) -> DisplayString {
    let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
        .bound_element(entry, "p");

    let mut res = DisplayString::new();
    if let Some(parent) = mv_title {
        res += get_chunk_title(parent, short, true, common);
    } else {
        res += get_chunk_title(entry, short, true, common);
    }

    if !short && mv_title.is_none() {
        let chapter = sel!(Id(Chapter) => Bind("p", attrs!(Wc(), "title")))
            .bound_element(entry, "p");

        let edited_book = sel!(alt
            sel!(Wc() => Bind("p", attrs!(sel!(alt Id(Book), Id(Proceedings)), "editor"))),
            sel!(sel!(alt Id(Anthos), Id(Entry)) => Bind("p", sel!(alt Id(Anthology), Id(Reference)))),
            sel!(Id(Article) => Bind("p", Id(Proceedings))),
            sel!(attrs!(Id(Entry), "author") => Bind("p", Id(Reference))),
        ).bound_element(entry, "p");

        if let Some(parent) = edited_book.or(chapter) {
            push_comma_quote_aware(&mut res.value, comma, true);
            if chapter.is_some() {
                if let Some(sn) = entry.serial_number() {
                    if sn.chars().all(char::is_numeric) {
                        res += &format!("chap. {} ", sn);
                    } else {
                        res += sn;
                        res.push(' ');
                    }
                }
            }

            if comma == '.' {
                res += "In ";
            } else {
                res += "in ";
            }
            res += get_chunk_title(parent, false, true, common);

            if parent.authors_fallible().is_none() {
                if let Some(eds) = parent.editors() {
                    let ed_names =
                        eds.into_iter().map(|p| p.given_first(false)).collect::<Vec<_>>();

                    let mut local =
                        if ed_names.len() > 1 { "eds. " } else { "ed. " }.to_string();

                    local += &and_list(ed_names, false, common.et_al_limit);
                    push_comma_quote_aware(&mut res.value, ',', true);
                    res += &local;
                }
            }
        }

        let web_parent = sel!(alt
            sel!(Id(Web) => Bind("p", Wc())),
            sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
        )
        .bound_element(entry, "p");

        if web_parent.and_then(|p| p.title()).is_some() {
            push_comma_quote_aware(&mut res.value, comma, true);
            res += &get_chunk_title(web_parent.unwrap(), false, false, common).value;
        }

        let blog_parent = sel!(Wc() => Bind("p", Id(Blog))).bound_element(entry, "p");

        if let Some(parent) = blog_parent {
            let titles = get_title(parent, false, common, comma);
            if !titles.is_empty() {
                push_comma_quote_aware(&mut res.value, comma, true);
            }

            res += titles;
        } else if entry.entry_type == Blog {
            let title_parent =
                sel!(Wc() => Bind("p", attrs!(Wc(), "title"))).bound_element(entry, "p");
            if let Some(parent) = title_parent {
                let titles = get_title(parent, false, common, comma);
                push_comma_quote_aware(&mut res.value, comma, true);
                res += titles;
            }
        }
    }

    res
}

pub(super) fn get_chunk_title(
    entry: &Entry,
    short: bool,
    mut fmt: bool,
    common: &CommonChicagoConfig,
) -> DisplayString {
    let mut res = DisplayString::new();

    if entry.title() == Some("Wikipedia") || entry.entry_type == Repository {
        fmt = false;
    }
    let sc = fmt && self_contained(entry);

    if sc {
        res.start_format(Formatting::Italic);
    } else if fmt {
        res += "“";
    }

    let np = entry.entry_type == Newspaper;

    if short {
        if let Some(title) = entry.title_raw().map(|t| shorthand(t)) {
            let title = title.format(Some(&common.tc_formatter), None);
            res += &if entry.entry_type == Entry {
                title.value
            } else {
                title.title_case
            };
        }
    } else {
        if let Some(title) = entry.title_fmt(Some(&common.tc_formatter), None) {
            let title = title.value;
            res += &if entry.entry_type == Entry {
                title.value
            } else if entry.entry_type == Case {
                title.title_case.replace("V.", "v.")
            } else {
                title.title_case
            };
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

    if !short {
        if let Some(translation) = entry
            .title_fmt(None, Some(&common.sc_formatter))
            .and_then(|f| f.translated)
        {
            if !res.is_empty() {
                res.push(' ');
            }

            res.push('[');
            res += &translation.sentence_case;
            res.push(']');
        }
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

/// Indicates whether a kind of work is its own container.
fn self_contained(entry: &Entry) -> bool {
    let sc = matches!(
        entry.entry_type,
        Book | Report
            | Newspaper
            | Periodical
            | Proceedings
            | Blog
            | Reference
            | Conference
            | Anthology
            | Thread
            | Exhibition
            | Case
    );

    sc || (!matches!(entry.entry_type, Web)
        && sel!(alt
            attrs!(Wc(), "publisher"),
            sel!(Wc() => Neg(Wc())),
        )
        .matches(entry))
}

fn omit_initial_articles(s: &str) -> String {
    let parts = s.split(' ').collect::<Vec<_>>();
    if parts.len() < 2 {
        return s.to_string();
    }

    if ["a", "an", "the"].contains(&parts.first().unwrap().to_lowercase().as_ref()) {
        (&parts[1 ..]).join(" ")
    } else {
        s.to_string()
    }
}

fn shorthand(title: &Title) -> FormattableString {
    if let Some(sh) = title.shorthand.as_ref() {
        sh.clone()
    } else {
        FormattableString {
            value: omit_initial_articles(&title.value.value),
            title_case: title
                .value
                .title_case
                .as_ref()
                .map(|tc| omit_initial_articles(tc)),
            sentence_case: title
                .value
                .sentence_case
                .as_ref()
                .map(|tc| omit_initial_articles(tc)),
            verbatim: title.value.verbatim,
        }
    }
}

fn web_creator(
    entry: &Entry,
    invert_first: bool,
    et_al_limit: Option<usize>,
) -> Option<String> {
    let web_thing = sel!(alt
        Id(Web),
        sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
    )
    .apply(entry);
    web_thing.map(|wt| {
        if let Some(org) = entry.organization() {
            org.into()
        } else if wt.get("p").and_then(|e| e.authors_fallible()).is_some() {
            let authors =
                get_creators(wt.get("p").unwrap()).0.into_iter().enumerate().map(
                    |(i, p)| {
                        if i == 0 && invert_first {
                            p.name_first(false, true)
                        } else {
                            p.given_first(false)
                        }
                    },
                );

            and_list(authors, invert_first, et_al_limit)
        } else if let Some(org) = wt.get("p").and_then(|e| e.organization()) {
            org.into()
        } else {
            "".into()
        }
    })
}

/// Which parts of the day should be printed.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

    res += &date.display_year();
    res
}

fn get_info_element(
    mut entry: &Entry,
    common: &CommonChicagoConfig,
    capitals: bool,
) -> DisplayString {
    let series = sel!(alt
        attrs!(Id(Video), "volume-total"),
        attrs!(Id(Audio), "volume-total"),
        sel!(Id(Video) => Id(Video)),
        sel!(Id(Audio) => Id(Audio)),
    )
    .matches(entry);

    let prepend = if let Some(lang) = entry.language() {
        if entry.title_raw().and_then(|t| t.translated.as_ref()).is_none() {
            let mut lingo = if capitals { "[In " } else { "[in " }.to_string();
            lingo += Language::from_639_1(lang.language.as_str()).unwrap().to_name();
            if capitals {
                lingo.push('.');
            }
            lingo += "]";
            lingo
        } else {
            String::new()
        }
    } else {
        String::new()
    };

    let mut res = vec![];

    let journal = sel!(
        sel!(alt Id(Article), Id(Entry)) => Bind("p", sel!(alt Id(Periodical), Id(Newspaper)))
    ).bound_element(entry, "p");

    let newspaper = sel!(Wc() => Bind("p", Id(Newspaper))).bound_element(entry, "p");

    let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
        .bound_element(entry, "p");

    let orig_entry = entry;
    if let Some(mv_p) = mv_title {
        entry = mv_p;
    }

    let affs = sel!(alt
        Bind("tl", attrs!(Wc(), "affiliated")),
        sel!(Wc() => Bind("tl", attrs!(Wc(), "affiliated"))),
    )
    .bound_element(entry, "tl");

    let ed_match = sel!(alt
        Bind("ed", attrs!(Wc(), "editor")),
        sel!(Wc() => Bind("ed", attrs!(Wc(), "editor"))),
    )
    .bound_element(entry, "ed");
    let eds = ed_match.map(|e| e.editors().unwrap());

    let translators = affs.and_then(|e| {
        let ts = e.affiliated_filtered(PersonRole::Translator);
        if ts.is_empty() { None } else { Some(ts) }
    });

    let compilers = affs.and_then(|e| {
        let ts = e.affiliated_filtered(PersonRole::Compiler);
        if ts.is_empty() { None } else { Some(ts) }
    });

    if let Some(trans) = translators {
        if orig_entry.authors_fallible().is_some() || orig_entry.editors().is_some() {
            let trans_names = trans.into_iter().map(|p| p.given_first(false));

            let mut local =
                if capitals { "Translated by " } else { "trans. " }.to_string();

            local += &and_list(trans_names, false, common.et_al_limit);
            res.push(local.into())
        }
    }

    if let Some(eds) = eds {
        let edited_book = sel!(alt
            sel!(Wc() => Bind("p", attrs!(sel!(alt Id(Book), Id(Proceedings)), "editor"))),
            sel!(sel!(alt Id(Anthos), Id(Entry)) => Bind("p", sel!(alt Id(Anthology), Id(Reference)))),
            sel!(Id(Article) => Bind("p", Id(Proceedings))),
            sel!(attrs!(Id(Entry), "author") => Bind("p", Id(Reference))),
        ).bound_element(entry, "p");

        if orig_entry.authors_fallible().is_some() && ed_match != edited_book {
            let ed_names =
                eds.into_iter().map(|p| p.given_first(false)).collect::<Vec<_>>();

            let mut local = if capitals && ed_match != Some(orig_entry) {
                "Edited by "
            } else {
                if ed_names.len() > 1 { "eds. " } else { "ed. " }
            }
            .to_string();

            local += &and_list(ed_names, false, common.et_al_limit);
            res.push(local.into())
        }
    }

    if let Some(journal) = journal {
        if let Some(volume) = entry.volume() {
            res.push(format_range("pt.", "pts.", volume).into());
        }

        let mut local = get_chunk_title(journal, false, true, common);

        if let Some(paper) = newspaper {
            if let Some(location) = paper.location() {
                if !local.is_empty() {
                    local.push(' ');
                }

                local += &format!("({})", location);
            }
        }

        if let Some(ed) = journal.edition() {
            let applied = match ed {
                NumOrStr::Number(n) if *n > 1 => format!("{} ser.", get_ordinal(*n)),
                NumOrStr::Str(s) => {
                    let mut s =
                        s.to_lowercase().replace("new", "n.").replace("series", "s.");
                    if s.trim() == "n. s." {
                        s = "n.s.".into()
                    };

                    s
                }
                _ => String::new(),
            };

            if !applied.is_empty() {
                if !local.is_empty() {
                    local += ", ";
                }

                local += &applied;

                if journal.volume().is_some() {
                    local.push(',')
                }
            }
        }

        if let Some(vol) = journal.volume() {
            if !local.is_empty() {
                local.push(' ');
            }
            local += &format_range("", "", vol);
        }

        if let Some(iss) = journal.issue() {
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
        if (orig_entry.authors_fallible().is_some() || orig_entry.entry_type == Reference)
            || orig_entry.editors().is_some()
        {
            let comp_names =
                comp.into_iter().map(|p| p.given_first(false)).collect::<Vec<_>>();

            let mut local = if capitals {
                "Compiled by "
            } else {
                if comp_names.len() > 1 { "comps. " } else { "comp. " }
            }
            .to_string();

            local += &and_list(comp_names, false, common.et_al_limit);
            res.push(local.into())
        }
    }

    if let Some(edition) = entry.edition() {
        match edition {
            NumOrStr::Number(i) if *i <= 1 => {}
            NumOrStr::Number(i) => res.push(format!("{} ed.", get_ordinal(*i)).into()),
            NumOrStr::Str(s) => {
                res.push(
                    s.split(' ')
                        .flat_map(|i| {
                            match i.to_lowercase().as_ref() {
                                "edition" => "ed.",
                                "revised" => "rev.",
                                _ => i,
                            }
                            .chars()
                        })
                        .collect::<String>()
                        .into(),
                );
            }
        }
    }

    if journal.is_none() {
        if let Some(vols) = orig_entry.volume() {
            if series {
                res.push(format_range("season", "seasons", vols).into())
            } else {
                res.push(format_range("vol.", "vols.", vols).into())
            }
        }

        if let Some(&vtotal) = orig_entry.total_volumes() {
            if vtotal > 1 {
                if series {
                    res.push(format!("{} seasons", vtotal).into())
                } else {
                    res.push(format!("{} vols.", vtotal).into())
                }
            }
        }

        if series {
            if let Some(eps) = orig_entry.issue() {
                match eps {
                    NumOrStr::Str(s) => res.push(s.clone().into()),
                    NumOrStr::Number(n) => res.push(format!("ep. {}", n).into()),
                }
            }
        }

        if mv_title.is_some() {
            let mut title = DisplayString::new();
            title.start_format(Formatting::Italic);
            if let Some(own_title) =
                orig_entry.title_fmt(Some(&common.tc_formatter), None)
            {
                title += &own_title.value.title_case;
            }
            if let Some(eds) = orig_entry.editors() {
                push_comma_quote_aware(&mut title.value, ',', true);
                title.commit_formats();
                let ed_names =
                    eds.into_iter().map(|p| p.given_first(false)).collect::<Vec<_>>();

                let mut local = if capitals {
                    "edited by "
                } else {
                    if ed_names.len() > 1 { "eds. " } else { "ed. " }
                }
                .to_string();

                local += &and_list(ed_names, false, common.et_al_limit);
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
        if let Some(mut bindings) = spec.apply(orig_entry) {
            let par_anth = bindings.remove("p").unwrap();
            let mut title = get_chunk_title(par_anth, false, false, common).value.into();

            let issue = if let Some(issue) = par_anth.issue() {
                Some(match issue {
                    NumOrStr::Str(s) => (s.clone(), None),
                    NumOrStr::Number(i) => (
                        if series {
                            format!("ep. {}", i)
                        } else {
                            format!("no. {}", i)
                        },
                        Some(*i),
                    ),
                })
            } else {
                None
            };

            let volume = par_anth.volume().map(|v| {
                let val = if v.start == v.end { Some(v.start) } else { None };
                (
                    if capitals {
                        format_range("Vol.", "Vols.", v)
                    } else {
                        format_range("vol.", "vols.", v)
                    },
                    val,
                )
            });

            if let (Some(issue), Some(volume)) = (&issue, &volume) {
                res.push(title);
                res.push(volume.clone().0.into());
                res.push(issue.clone().0.into());
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

    if entry.entry_type == Reference {
        if let Some(sn) = entry.serial_number() {
            res.push(sn.into());
        }
    } else if entry.entry_type == Case || entry.entry_type == Legislation {
        if let Some(org) = entry.serial_number().or_else(|| entry.organization()) {
            res.push(org.into());
        }
    }

    let joined = if capitals {
        DisplayString::join(&res, ". ")
    } else {
        DisplayString::join(&res, ", ")
    };
    if !prepend.is_empty() {
        let mut prepend = DisplayString::from_string(prepend);
        if !joined.is_empty() {
            prepend.push(' ');
            prepend += joined;
        }
        prepend
    } else {
        joined
    }
}

fn entry_date(entry: &Entry, force_year: bool) -> String {
    if let Some(date) = entry.any_date() {
        let mode = if force_year {
            DateMode::Year
        } else {
            let journal = sel!(
                sel!(alt Id(Article), Id(Entry)) => Id(Periodical)
            )
            .matches(entry);
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

            mode
        };

        format_date(date, mode)
    } else if force_year
        || (matches!(&entry.entry_type, Book | Anthology)
            && entry.any_url().and_then(|url| url.visit_date.as_ref()).is_none())
    {
        "n.d.".to_string()
    } else {
        String::new()
    }
}
