//! Both the author / date and notes / bibliograpghy citation styles as defined
//! in the 17th edition of the Chicago Manual of Style.

pub mod author_date;
pub mod nb;

use super::{push_comma_quote_aware, DisplayString, Formatting};
use crate::lang::{SentenceCase, TitleCase};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{FormattableString, Person, PersonRole, Title};
use crate::{attrs, sel, Entry};

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
            AccessDateConfig::NoDate => entry.get_any_date().is_none(),
            AccessDateConfig::InherentlyOnline => {
                sel!(alt
                    Id(Web),
                    sel!(sel!(alt Id(Misc), Id(Web)) => Id(Web)),
                )
                .matches(entry)
                    || entry.get_any_date().is_none()
            }
            AccessDateConfig::NotFormallyPublished => {
                !is_formally_published(entry) || entry.get_any_date().is_none()
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

enum AuthorRole {
    Normal,
    Editor,
    Compiler,
    Translator,
    Director,
    ExecutiveProducer,
}

fn get_creators(entry: &Entry) -> (Vec<Person>, AuthorRole) {
    let mut add = AuthorRole::Normal;
    let authors = if let Some(authors) = entry.get_authors_fallible() {
        authors.to_vec()
    } else if let Some(eds) = entry.get_editors() {
        add = AuthorRole::Editor;
        eds.to_vec()
    } else if entry.entry_type == Video {
        let tv_series =
            sel!(attrs!(Id(Video), "issue", "volume") => Bind("p", Id(Video)));
        if let Some(mut bindings) = tv_series.apply(entry) {
            let mut affs = entry.get_affiliated_filtered(PersonRole::Director);
            affs.extend(entry.get_affiliated_filtered(PersonRole::Writer));
            if !affs.is_empty() {
                affs
            } else {
                let parent = bindings.remove("p").unwrap();
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

    (authors, add)
}

fn and_list_opt(
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

fn and_list(
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
                if let Some(sn) = entry.get_serial_number() {
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

            if parent.get_authors_fallible().is_none() {
                if let Some(eds) = parent.get_editors() {
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

        if web_parent.and_then(|p| p.get_title()).is_some() {
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

fn get_chunk_title(
    entry: &Entry,
    short: bool,
    mut fmt: bool,
    common: &CommonChicagoConfig,
) -> DisplayString {
    let mut res = DisplayString::new();

    if entry.get_title() == Some("Wikipedia") || entry.entry_type == Repository {
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
        if let Some(title) = entry.get_title_raw().map(|t| shorthand(t)) {
            let title = title.format(Some(&common.tc_formatter), None);
            res += &if entry.entry_type == Entry {
                title.value
            } else {
                title.title_case
            };
        }
    } else {
        if let Some(title) = entry.get_title_fmt(Some(&common.tc_formatter), None) {
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
            .get_title_fmt(None, Some(&common.sc_formatter))
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
        if let Some(org) = entry.get_organization() {
            org.into()
        } else if wt.get("p").and_then(|e| e.get_authors_fallible()).is_some() {
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
        } else if let Some(org) = wt.get("p").and_then(|e| e.get_organization()) {
            org.into()
        } else {
            "".into()
        }
    })
}
