//! Notes and Bibliography (nb) referencing as defined in chapter 14 of the
//! 17th edition of the Chicago Manual of Style.

use super::{and_list, get_chunk_title, CommonChicagoConfig};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::output::{format_range, push_comma_quote_aware, DisplayString, Formatting};
use crate::selectors::{Bind, Id, Wc};
use crate::types::EntryType::*;
use crate::types::{Date, PersonRole};
use crate::{attrs, sel, Entry, NumOrStr};

use isolang::Language;

pub mod bibliography;
pub mod notes;

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

        if orig_entry.get_authors_fallible().is_some() && ed_match != edited_book {
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

    if let Some(edition) = entry.get_edition() {
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
        if let Some(vols) = orig_entry.get_volume() {
            if series {
                res.push(format_range("season", "seasons", vols).into())
            } else {
                res.push(format_range("vol.", "vols.", vols).into())
            }
        }

        if let Some(&vtotal) = orig_entry.get_total_volumes() {
            if vtotal > 1 {
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

        if mv_title.is_some() {
            let mut title = DisplayString::new();
            title.start_format(Formatting::Italic);
            if let Some(own_title) =
                orig_entry.get_title_fmt(Some(&common.tc_formatter), None)
            {
                title += &own_title.value.title_case;
            }
            if let Some(eds) = orig_entry.get_editors() {
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
