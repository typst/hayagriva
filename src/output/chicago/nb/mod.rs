//! Notes and Bibliography (nb) referencing as defined in chapter 14 of the
//! 17th edition of the Chicago Manual of Style.

use super::AccessDateConfig;
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::{SentenceCase, TitleCase};
use crate::output::{format_range, push_comma_quote_aware, DisplayString, Formatting};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{Date, FormattableString, Person, PersonRole, Title};
use crate::{attrs, sel, Entry, NumOrStr};

use isolang::Language;

pub mod bibliography;
pub mod notes;

/// Settings that both notes and biliography need.
pub struct CommonChicagoNbConfig {
    /// If there is greater or equal to this number of authors, they will be
    /// abbreviated using et. al. after the first name.
    pub et_al_limit: Option<usize>,
    /// When to print URL access dates.
    pub url_access_date: AccessDateConfig,
    tc_formatter: TitleCase,
    sc_formatter: SentenceCase,
}

impl CommonChicagoNbConfig {
    /// Create a new [CommonChicagoNbConfig].
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
    if let Some(sh) = title.clone().shorthand {
        sh
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

fn and_list(names: Vec<String>, oxford: bool, et_al_limit: Option<usize>) -> String {
    let name_len = names.len();
    let mut res = String::new();
    let threshold = et_al_limit.unwrap_or(0);

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
        Case,
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

fn get_title(
    entry: &Entry,
    short: bool,
    common: &CommonChicagoNbConfig,
    comma: char,
) -> DisplayString {
    let mv_title = sel!(attrs!(Id(Book), "volume") => Bind("p", attrs!(sel!(alt Id(Book), Id(Anthology)), "title")))
        .apply(entry)
        .map(|mut hm| hm.remove("p").unwrap());


    let mut res = DisplayString::new();
    if let Some(parent) = mv_title {
        res += get_chunk_title(parent, short, true, common);
    } else {
        res += get_chunk_title(entry, short, true, common);
    }

    if !short && mv_title.is_none() {
        let chapter = sel!(Id(Chapter) => Bind("p", attrs!(Wc(), "title")))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());

        let edited_book = sel!(alt
            sel!(Wc() => Bind("p", attrs!(sel!(alt Id(Book), Id(Proceedings)), "editor"))),
            sel!(sel!(alt Id(Anthos), Id(Entry)) => Bind("p", sel!(alt Id(Anthology), Id(Reference)))),
            sel!(Id(Article) => Bind("p", Id(Proceedings))),
            sel!(attrs!(Id(Entry), "author") => Bind("p", Id(Reference))),
        ).apply(entry).map(|mut hm| hm.remove("p").unwrap());

        if let Some(parent) = edited_book.or(chapter) {
            res.value = push_comma_quote_aware(res.value, comma, true);
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

            if comma == '.' {
                res += "In ";
            } else {
                res += "in ";
            }
            res += get_chunk_title(parent, false, true, common);

            if parent.get_authors_fallible().is_none() {
                if let Some(eds) = parent.get_editors() {
                    let ed_names = eds
                        .into_iter()
                        .map(|p| p.get_given_name_initials_first(false))
                        .collect::<Vec<_>>();

                    let mut local = if ed_names.len() > 1 { "eds. " } else { "ed. " }.to_string();

                    local += &and_list(ed_names, false, common.et_al_limit);
                    res.value = push_comma_quote_aware(res.value, ',', true);
                    res += &local;
                }
            }
        }

        let web_parent = sel!(alt
            sel!(Id(Web) => Bind("p", Wc())),
            sel!(sel!(alt Id(Misc), Id(Web)) => Bind("p", Id(Web))),
        )
        .apply(entry)
        .map(|mut hm| hm.remove("p").unwrap());

        if web_parent.and_then(|p| p.get_title()).is_some() {
            res.value = push_comma_quote_aware(res.value, comma, true);
            res += &get_chunk_title(web_parent.unwrap(), false, false, common).value;
        }

        let blog_parent = sel!(Wc() => Bind("p", Id(Blog)))
            .apply(entry)
            .map(|mut hm| hm.remove("p").unwrap());
        if let Some(parent) = blog_parent {
            let titles = get_title(parent, false, common, comma);

            if !titles.is_empty() {
                res.value = push_comma_quote_aware(res.value, comma, true);
            }

            res += titles;
        } else if entry.entry_type == Blog {
            let title_parent = sel!(Wc() => Bind("p", attrs!(Wc(), "title")))
                .apply(entry)
                .map(|mut hm| hm.remove("p").unwrap());
            if let Some(parent) = title_parent {
                let titles = get_title(parent, false, common, comma);
                res.value = push_comma_quote_aware(res.value, comma, true);
                res += titles;
            }
        }
    }

    res
}

fn get_info_element(
    mut entry: &Entry,
    common: &CommonChicagoNbConfig,
    capitals: bool,
) -> DisplayString {
    let series = sel!(alt
        attrs!(Id(Video), "volume-total"),
        attrs!(Id(Audio), "volume-total"),
        sel!(Id(Video) => Id(Video)),
        sel!(Id(Audio) => Id(Audio)),
    )
    .apply(entry)
    .is_some();

    let prepend = if let Some(lang) = entry.get_language() {
        if entry.get_title_raw().and_then(|t| t.translated.as_ref()).is_none() {
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

    let ed_match = sel!(alt
        Bind("ed", attrs!(Wc(), "editor")),
        sel!(Wc() => Bind("ed", attrs!(Wc(), "editor"))),
    )
    .apply(entry)
    .map(|mut hm| hm.remove("ed").unwrap());
    let eds = ed_match.map(|e| e.get_editors().unwrap());

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
        ).apply(entry).map(|mut hm| hm.remove("p").unwrap());

        if orig_entry.get_authors_fallible().is_some() && ed_match != edited_book {
            let ed_names = eds
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>();

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
        if let Some(volume) = entry.get_volume() {
            res.push(format_range("pt.", "pts.", volume).into());
        }

        let mut local = get_chunk_title(journal, false, true, common);

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
                    let s = s.to_lowercase().replace("new", "n.").replace("series", "s.");
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
        if (orig_entry.get_authors_fallible().is_some()
            || orig_entry.entry_type == Reference)
            || orig_entry.get_editors().is_some()
        {
            let comp_names = comp
                .into_iter()
                .map(|p| p.get_given_name_initials_first(false))
                .collect::<Vec<_>>();

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

    if let Some(edition) = entry.get_edition() {
        match edition {
            NumOrStr::Number(i) if *i <= 1 => {}
            NumOrStr::Number(i) => res.push(format!("{} ed.", get_ordinal(*i)).into()),
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
            if series {
                res.push(format_range("season", "seasons", vols).into())
            } else {
                res.push(format_range("vol.", "vols.", vols).into())
            }
        }

        if let Some(vtotal) = orig_entry.get_total_volumes() {
            if *vtotal > 1 {
                if series {
                    res.push(format!("{} seasons", vtotal).into())
                } else {
                    res.push(format!("{} vols.", vtotal).into())
                }
            }
        }

        if series {
            if let Some(eps) = orig_entry.get_issue() {
                match eps {
                    NumOrStr::Str(s) => res.push(s.clone().into()),
                    NumOrStr::Number(n) => res.push(format!("ep. {}", n).into()),
                }
            }
        }

        if let Some(_parent) = mv_title {
            let mut title = DisplayString::new();
            title.start_format(Formatting::Italic);
            if let Some(own_title) =
                orig_entry.get_title_fmt(Some(&common.tc_formatter), None)
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
        if let Some(mut hm) = spec.apply(orig_entry) {
            let par_anth = hm.remove("p").unwrap();
            let mut title = get_chunk_title(par_anth, false, false, common).value.into();

            let issue = if let Some(issue) = par_anth.get_issue() {
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

            let volume = par_anth.get_volume().map(|v| {
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

    if entry.entry_type == Reference {
        if let Some(sn) = entry.get_serial_number() {
            res.push(sn.into());
        }
    } else if entry.entry_type == Case || entry.entry_type == Legislation {
        if let Some(org) = entry.get_serial_number().or_else(|| entry.get_organization())
        {
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

fn get_chunk_title(
    entry: &Entry,
    short: bool,
    mut fmt: bool,
    common: &CommonChicagoNbConfig,
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

enum AuthorRole {
    Normal,
    Editor,
    Compiler,
    Translator,
    Director,
    ExecutiveProducer,
}

fn common_author_handling(entry: &Entry) -> (Vec<Person>, AuthorRole) {
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
            affs.extend(entry.get_affiliated_filtered(PersonRole::Writer).into_iter());
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

    (authors, add)
}
