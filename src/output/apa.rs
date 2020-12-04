//! Style for entries in a reference list [as defined in the 7th edition of the
//! APA Publication Manual](https://apastyle.apa.org/style-grammar-guidelines/references/).

use super::{
    format_range, name_list, name_list_straight, BibliographyFormatter, DisplayString,
    Formatting,
};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::SentenceCase;
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{NumOrStr, Person, PersonRole};
use crate::{attrs, sel, Entry};

/// Generates APA reference list entries.
#[derive(Clone, Debug)]
pub struct ApaBibliographyFormatter {
    formatter: SentenceCase,
}

#[derive(Clone, Debug)]
enum SourceType<'s> {
    PeriodicalItem(&'s Entry),
    CollectionItem(&'s Entry),
    TvSeries(&'s Entry),
    Thesis,
    Manuscript,
    ArtContainer(&'s Entry),
    StandaloneArt,
    StandaloneWeb,
    Web(&'s Entry),
    NewsItem(&'s Entry),
    ConferenceTalk(&'s Entry),
    GenericParent(&'s Entry),
    Generic,
}

impl<'s> SourceType<'s> {
    fn for_entry(entry: &'s Entry) -> Self {
        let periodical = sel!(Wc() => Bind("p", Id(Periodical)));
        let collection = sel!(alt
            sel!(Id(Anthos) => Bind("p", Id(Anthology))),
            sel!(Id(Entry) => Bind("p", Wc())),
            sel!(Wc() => Bind("p", Id(Reference))),
            sel!(Id(Article) => Bind("p", Id(Proceedings))),
        );
        let tv_series =
            sel!(attrs!(Id(Video), "issue", "volume") => Bind("p", Id(Video)));
        let thesis = Id(Thesis);
        let manuscript = Id(Manuscript);
        let art_container = sel!(Wc() => Bind("p", Id(Artwork)));
        let art = sel!(alt Id(Artwork), Id(Exhibition));
        let news_item = sel!(Wc() => Bind("p", Id(Newspaper)));
        let web_standalone = Id(Web);
        let web_contained = sel!(alt
            sel!(Id(Web) => Bind("p", Wc())),
            sel!(Wc() => Bind("p", sel!(alt attrs!(Id(Misc), "url"), Id(Blog), Id(Web)))),
        );
        let talk = sel!(Wc() => Bind("p", Id(Conference)));
        let generic_parent = sel!(Wc() => Bind("p", Wc()));

        if let Some(mut hm) = periodical.apply(entry) {
            Self::PeriodicalItem(hm.remove("p").unwrap())
        } else if let Some(mut hm) = collection.apply(entry) {
            Self::CollectionItem(hm.remove("p").unwrap())
        } else if let Some(mut hm) = tv_series.apply(entry) {
            Self::TvSeries(hm.remove("p").unwrap())
        } else if thesis.apply(entry).is_some() {
            Self::Thesis
        } else if manuscript.apply(entry).is_some() {
            Self::Manuscript
        } else if let Some(mut hm) = art_container.apply(entry) {
            Self::ArtContainer(hm.remove("p").unwrap())
        } else if art.apply(entry).is_some() {
            Self::StandaloneArt
        } else if let Some(mut hm) = news_item.apply(entry) {
            Self::NewsItem(hm.remove("p").unwrap())
        } else if web_standalone.apply(entry).is_some() {
            Self::StandaloneWeb
        } else if let Some(mut hm) = web_contained.apply(entry) {
            Self::Web(hm.remove("p").unwrap())
        } else if let Some(mut hm) = talk.apply(entry) {
            Self::ConferenceTalk(hm.remove("p").unwrap())
        } else if let Some(mut hm) = generic_parent.apply(entry) {
            Self::GenericParent(hm.remove("p").unwrap())
        } else {
            Self::Generic
        }
    }
}

fn ampersand_list(names: Vec<String>) -> String {
    let name_len = names.len() as i64;
    let mut res = String::new();

    for (index, name) in names.into_iter().enumerate() {
        if index > 19 && name_len > 20 && (index as i64) != name_len - 1 {
            // Element 20 or longer if longer than twenty and not last
            continue;
        }

        if index == 19 && name_len > 20 {
            res += "... ";
        } else {
            res += &name;
        }

        if (index as i64) <= name_len - 2 {
            res += ", ";
        }
        if (index as i64) == name_len - 2 {
            res += "& ";
        }
    }

    res
}

fn ed_vol_str(entry: &Entry, is_tv_show: bool) -> String {
    let vstr = if let Some(vols) = entry.get_volume() {
        if is_tv_show {
            Some(format_range("Episode", "Episodes", &vols))
        } else {
            Some(format_range("Vol.", "Vols.", &vols))
        }
    } else {
        None
    };

    let ed = if is_tv_show {
        entry.get_issue()
    } else {
        entry.get_edition()
    };

    let translator = entry.get_affiliated_filtered(PersonRole::Translator);

    let translator = if translator.is_empty() {
        None
    } else {
        Some(format!(
            "{}, Trans.",
            ampersand_list(name_list_straight(&translator))
        ))
    };

    let estr = if let Some(ed) = ed {
        if is_tv_show {
            Some(format!("Season {}", ed))
        } else {
            Some(format!("{} ed.", match ed {
                NumOrStr::Number(e) => get_ordinal(*e),
                NumOrStr::Str(s) => s.to_string(),
            }))
        }
    } else {
        None
    };

    match (translator, estr, vstr) {
        (Some(t), None, None) => format!(" ({})", t),
        (Some(t), Some(e), None) => format!(" ({}; {})", t, e),
        (Some(t), None, Some(v)) => format!(" ({}; {})", t, v),
        (Some(t), Some(e), Some(v)) => format!(" ({}; {}, {})", t, e, v),
        (None, None, None) => String::new(),
        (None, Some(e), None) => format!(" ({})", e),
        (None, None, Some(v)) => format!(" ({})", v),
        (None, Some(e), Some(v)) => format!(" ({}, {})", e, v),
    }
}

impl ApaBibliographyFormatter {
    /// Creates a new bibliography generator.
    pub fn new() -> Self {
        Self { formatter: SentenceCase::default() }
    }

    fn get_author(&self, entry: &Entry) -> String {
        #[derive(Clone, Debug)]
        enum AuthorRole {
            Normal,
            Director,
            ExecutiveProducer,
        }

        impl Default for AuthorRole {
            fn default() -> Self {
                Self::Normal
            }
        }

        let mut names = None;
        let mut role = AuthorRole::default();
        if entry.entry_type == Video {
            let tv_series = sel!(attrs!(Id(Video), "issue", "volume") => Id(Video));
            let dirs = entry.get_affiliated_filtered(PersonRole::Director);

            if tv_series.apply(entry).is_some() {
                // TV episode
                let mut dir_name_list = name_list(&dirs)
                    .into_iter()
                    .map(|s| format!("{} (Director)", s))
                    .collect::<Vec<String>>();

                let writers = entry.get_affiliated_filtered(PersonRole::Writer);
                let mut writers_name_list = name_list(&writers)
                    .into_iter()
                    .map(|s| format!("{} (Writer)", s))
                    .collect::<Vec<String>>();
                dir_name_list.append(&mut writers_name_list);

                if !dirs.is_empty() {
                    names = Some(dir_name_list);
                }
            } else {
                // Film
                if !dirs.is_empty() {
                    names = Some(name_list(&dirs));
                    role = AuthorRole::Director;
                } else {
                    // TV show
                    let prods =
                        entry.get_affiliated_filtered(PersonRole::ExecutiveProducer);

                    if !prods.is_empty() {
                        names = Some(name_list(&prods));
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let authors =
            names.or_else(|| entry.get_authors_fallible().map(|n| name_list(n)));
        let mut al = if let Some(mut authors) = authors {
            let count = authors.len();
            if entry.entry_type == Tweet {
                authors = authors
                    .into_iter()
                    .enumerate()
                    .map(|(i, n)| {
                        if let Some(handle) = entry.get_twitter_handle(i) {
                            format!("{} [{}]", n, handle)
                        } else {
                            n
                        }
                    })
                    .collect();
            }

            let amps = ampersand_list(authors);
            match role {
                AuthorRole::Normal => amps,
                AuthorRole::ExecutiveProducer if count == 1 => {
                    format!("{} (Executive Producer)", amps)
                }
                AuthorRole::ExecutiveProducer => {
                    format!("{} (Executive Producers)", amps)
                }
                AuthorRole::Director if count == 1 => format!("{} (Director)", amps),
                AuthorRole::Director => format!("{} (Directors)", amps),
            }
        } else if let Some(eds) = entry.get_editors() {
            if !eds.is_empty() {
                format!(
                    "{} ({})",
                    ampersand_list(name_list(&eds)),
                    if eds.len() == 1 { "Ed." } else { "Eds." }
                )
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        let mut details = vec![];
        let booklike = sel!(alt Id(Book), Id(Proceedings), Id(Anthology));
        if booklike.apply(entry).is_some() {
            let affs = entry
                .get_affiliated_persons()
                .unwrap_or_default()
                .into_iter()
                .filter(|(_, role)| {
                    [
                        PersonRole::Foreword,
                        PersonRole::Afterword,
                        PersonRole::Introduction,
                        PersonRole::Annotator,
                        PersonRole::Commentator,
                    ]
                    .contains(role)
                })
                .map(|(v, _)| v)
                .flatten()
                .cloned()
                .collect::<Vec<Person>>();

            if !affs.is_empty() {
                details.push(format!("with {}", ampersand_list(name_list(&affs))));
            }
        }

        if !details.is_empty() {
            if !al.is_empty() {
                al.push(' ');
            }

            al += &details[0];

            for e in &details[1 ..] {
                al += "; ";
                al += e;
            }
        }

        if !al.is_empty() {
            let lc = al.chars().last().unwrap_or('a');

            if lc != '?' && lc != '.' && lc != '!' {
                al.push('.');
            }
        }

        al
    }

    fn get_date(&self, entry: &Entry) -> String {
        if let Some(date) = entry.get_any_date() {
            match (date.month, date.day) {
                (None, _) => format!("({:04}).", date.year),
                (Some(month), None) => {
                    format!("({:04}, {}).", date.year, get_month_name(month).unwrap())
                }
                (Some(month), Some(day)) => format!(
                    "({:04}, {} {}).",
                    date.year,
                    get_month_name(month).unwrap(),
                    day + 1,
                ),
            }
        } else {
            "(n. d.).".to_string()
        }
    }

    fn get_retreival_date(&self, entry: &Entry, use_date: bool) -> Option<DisplayString> {
        let url = entry.get_any_url();

        if let Some(qurl) = url {
            let uv = qurl.value.as_str();
            let res = if use_date {
                if let Some(date) = &qurl.visit_date {
                    match (date.month, date.day) {
                        (None, _) => {
                            let mut res = DisplayString::from_string(format!(
                                "Retrieved {:04}, from ",
                                date.year
                            ));
                            res.start_format(Formatting::NoHyphenation);
                            res += uv;
                            res.commit_formats();
                            res
                        }
                        (Some(month), None) => {
                            let mut res = DisplayString::from_string(format!(
                                "Retrieved {} {:04}, from ",
                                get_month_name(month).unwrap(),
                                date.year,
                            ));
                            res.start_format(Formatting::NoHyphenation);
                            res += uv;
                            res.commit_formats();
                            res
                        }
                        (Some(month), Some(day)) => {
                            let mut res = DisplayString::from_string(format!(
                                "(Retrieved {} {}, {:04}, from ",
                                get_month_name(month).unwrap(),
                                day,
                                date.year,
                            ));
                            res.start_format(Formatting::NoHyphenation);
                            res += uv;
                            res.commit_formats();
                            res.push(')');
                            res
                        }
                    }
                } else {
                    let mut res = DisplayString::new();
                    res.start_format(Formatting::NoHyphenation);
                    res += uv;
                    res.commit_formats();
                    res
                }
            } else {
                let mut res = DisplayString::new();
                res.start_format(Formatting::NoHyphenation);
                res += uv;
                res.commit_formats();
                res
            };

            Some(res)
        } else {
            None
        }
    }

    fn get_title(&self, entry: &Entry, wrap: bool) -> DisplayString {
        let italicise = if entry
            .get_parents()
            .unwrap_or_default()
            .into_iter()
            .any(|p| p.get_title().is_some())
        {
            let talk = sel!(Wc() => Id(Conference));
            let preprint =
                sel!(sel!(alt Id(Article), Id(Book), Id(Anthos)) => Id(Repository));

            talk.apply(entry).is_some() || preprint.apply(entry).is_some()
        } else {
            true
        };

        let mut res = DisplayString::new();
        let vid_match = sel!(attrs!(Id(Video), "issue", "volume") => Id(Video));

        let book =
            sel!(alt Id(Book), Id(Report), Id(Reference), Id(Anthology), Id(Proceedings))
                .matches(entry);

        if let Some(title) =
            entry.get_title_fmt(None, Some(&self.formatter)).map(|t| t.value)
        {
            let multivol_spec = sel!(
                attrs!(sel!(alt Id(Book), Id(Proceedings), Id(Anthology)), "volume") =>
                Bind("p", sel!(alt Id(Book), Id(Proceedings), Id(Anthology)))
            );

            let multivolume_parent =
                multivol_spec.apply(entry).and_then(|mut hm| hm.remove("p"));

            if italicise {
                res.start_format(Formatting::Italic);
            }
            if entry.entry_type == Tweet {
                let words = &title.value.split_whitespace().collect::<Vec<_>>();
                res += &words[.. (if words.len() >= 20 { 20 } else { words.len() })]
                    .join(" ");
            } else {
                res += &title.sentence_case;
            }
            res.commit_formats();

            if let Some(mv_parent) = multivolume_parent {
                let vols = entry.get_volume().unwrap();
                let mut new = DisplayString::from_string(format!(
                    "{}: {} ",
                    mv_parent
                        .get_title_fmt(None, Some(&self.formatter))
                        .unwrap()
                        .value
                        .sentence_case,
                    format_range("Vol.", "Vols.", &vols),
                ));
                new += res;
                res = new;
            } else if (entry.get_volume().is_some() || entry.get_edition().is_some())
                && book
            {
                res += &ed_vol_str(entry, false);
            } else if vid_match.apply(entry).is_some() {
                res += &ed_vol_str(entry, true);
            }
        }

        let mut items: Vec<String> = vec![];
        if book {
            let illustrators = entry
                .get_affiliated_persons()
                .unwrap_or_default()
                .into_iter()
                .filter(|(_, role)| role == &PersonRole::Illustrator)
                .map(|(v, _)| v)
                .flatten()
                .cloned()
                .collect::<Vec<Person>>();

            if !illustrators.is_empty() {
                items.push(format!(
                    "{}, Illus.",
                    ampersand_list(name_list_straight(&illustrators))
                ));
            }

            if entry.get_note().and_then(|_| entry.get_editors()).is_some()
                && !entry.get_authors().is_empty()
            {
                let editors = entry.get_editors().unwrap();
                let amp_list = ampersand_list(name_list_straight(&editors));
                if editors.len() == 1 {
                    items.push(format!("{}, Ed.", amp_list));
                } else if editors.len() > 1 {
                    items.push(format!("{}, Eds.", amp_list));
                }
            }
        } else if entry.entry_type == Report {
            if let Some(serial) = entry.get_serial_number() {
                items.push(serial.to_string());
            }
        } else if entry.entry_type == Thesis {
            if let Some(serial) = entry.get_serial_number() {
                items.push(format!("Publication No. {}", serial));
            }
        }

        let items = items.join("; ");
        if !items.is_empty() {
            if !res.is_empty() {
                res += " ";
            }

            res += &format!("({})", items);
        }

        #[derive(Clone, Debug, PartialEq)]
        enum TitleSpec {
            Normal,
            LegalProceedings,
            PaperPresentation,
            ConferenceSession,
            Thesis,
            UnpublishedThesis,
            SoftwareRepository,
            SoftwareRepositoryItem,
            Exhibition,
            Audio,
            Video,
            TvShow,
            TvEpisode,
            Film,
            Tweet,
        }

        let conf_spec = sel!(Id(Article) => Id(Proceedings));
        let talk_spec = sel!(Wc() => Id(Conference));
        let repo_item =
            sel!(Neg(sel!(alt Id(Article), Id(Report), Id(Thesis))) => Id(Repository));
        let spec = if entry.entry_type == Case {
            TitleSpec::LegalProceedings
        } else if conf_spec.apply(entry).is_some() {
            TitleSpec::PaperPresentation
        } else if talk_spec.apply(entry).is_some() {
            TitleSpec::ConferenceSession
        } else if entry.entry_type == Thesis {
            if entry.get_archive().is_some() || entry.get_url().is_some() {
                TitleSpec::Thesis
            } else {
                TitleSpec::UnpublishedThesis
            }
        } else if entry.entry_type == Exhibition {
            TitleSpec::Exhibition
        } else if entry.entry_type == Repository {
            TitleSpec::SoftwareRepository
        } else if repo_item.apply(entry).is_some() {
            TitleSpec::SoftwareRepositoryItem
        } else if entry.entry_type == Audio {
            TitleSpec::Audio
        } else if entry.entry_type == Video {
            let dirs = entry
                .get_affiliated_persons()
                .unwrap_or_default()
                .into_iter()
                .filter(|(_, role)| role == &PersonRole::Director)
                .map(|(v, _)| v)
                .flatten()
                .collect::<Vec<&Person>>();
            if !dirs.is_empty()
                && entry.get_total_volumes().is_none()
                && entry.get_parents().is_none()
            {
                TitleSpec::Film
            } else {
                let is_online_vid = if let Some(url) = entry.get_url() {
                    match url.value.host_str().unwrap_or("").to_lowercase().as_ref() {
                        "youtube.com" => true,
                        "dailymotion.com" => true,
                        "vimeo.com" => true,
                        _ => false,
                    }
                } else {
                    false
                };

                if is_online_vid {
                    TitleSpec::Video
                } else {
                    let prods =
                        entry.get_affiliated_filtered(PersonRole::ExecutiveProducer);

                    if vid_match.apply(entry).is_some() {
                        TitleSpec::TvEpisode
                    } else if !prods.is_empty() || entry.get_total_volumes().is_some() {
                        TitleSpec::TvShow
                    } else {
                        TitleSpec::Video
                    }
                }
            }
        } else if entry.entry_type == Tweet {
            TitleSpec::Tweet
        } else {
            TitleSpec::Normal
        };

        let append = match spec {
            TitleSpec::Normal => "",
            TitleSpec::LegalProceedings => "Legal proceedings",
            TitleSpec::PaperPresentation => "Paper presentation",
            TitleSpec::ConferenceSession => "Conference session",
            TitleSpec::Thesis => "Thesis",
            TitleSpec::UnpublishedThesis => "Unpublished thesis",
            TitleSpec::SoftwareRepository => "Software repository",
            TitleSpec::SoftwareRepositoryItem => "Software repository item",
            TitleSpec::Exhibition => "Exhibiton",
            TitleSpec::Audio => "Audio",
            TitleSpec::Video => "Video",
            TitleSpec::TvShow => "TV series",
            TitleSpec::TvEpisode => "TV series episode",
            TitleSpec::Film => "Film",
            TitleSpec::Tweet => "Tweet",
        };

        if !append.is_empty() {
            if !res.is_empty() {
                res.push(' ');
            }

            let printed = if spec == TitleSpec::Thesis {
                if let Some(org) = entry.get_organization() {
                    res += &format!("[{}, {}]", append, org);
                    true
                } else {
                    false
                }
            } else {
                false
            };

            if !printed {
                res += &format!("[{}]", append);
            }
        }

        if wrap && !res.is_empty() {
            let mut new = DisplayString::from_str("[");
            new += res;
            new += "]";
            res = new;
        }

        if let Some(lc) = res.last() {
            if lc != '?' && lc != '.' && lc != '!' {
                res.push('.');
            }
        }

        res
    }

    fn get_source(&self, entry: &Entry) -> DisplayString {
        let st = SourceType::for_entry(entry);
        let mut res = DisplayString::new();

        match st {
            SourceType::PeriodicalItem(parent) => {
                let mut comma = if let Some(title) =
                    parent.get_title_fmt(None, Some(&self.formatter)).map(|t| t.value)
                {
                    res.start_format(Formatting::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if parent.get_volume().is_some() || parent.get_issue().is_some() {
                    if comma {
                        res += ", ";
                    }

                    if let Some(volume) = parent.get_volume() {
                        res += &format_range("", "", &volume);
                    }

                    if let Some(issue) = parent.get_issue() {
                        res += &format!("({})", issue);
                    }
                    comma = true;
                }

                if entry.get_serial_number().is_some() || entry.get_page_range().is_some()
                {
                    if comma {
                        res += ", ";
                    }

                    if let Some(sn) = entry.get_serial_number() {
                        res += "Article ";
                        res += sn;
                    } else if let Some(pages) = entry.get_page_range() {
                        res += &format_range("", "", &pages);
                    }
                }
            }
            SourceType::CollectionItem(parent) => {
                let mut comma = if let Some(eds) = parent.get_editors() {
                    let names = name_list(&eds);
                    match names.len() {
                        0 => false,
                        1 => {
                            res += &format!("{} (Ed.)", names[0]);
                            true
                        }
                        _ => {
                            res += &format!("{} (Eds.)", ampersand_list(names));
                            true
                        }
                    }
                } else {
                    false
                };

                if let Some(title) = parent.get_title_fmt(None, Some(&self.formatter)) {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(Formatting::Italic);
                    res += &title.value.sentence_case;
                    res.commit_formats();
                    comma = true;

                    if parent.get_volume().is_some() || parent.get_edition().is_some() {
                        res += &ed_vol_str(parent, false);
                        res.push('.');
                        comma = false;
                    }
                }

                if comma {
                    res += ".";
                }

                if !res.is_empty() {
                    let mut new = DisplayString::from_str("In ");
                    new += res;
                    res = new;
                }

                if parent.get_publisher().is_some() || parent.get_organization().is_some()
                {
                    res.push(' ');

                    if let Some(publisher) = parent.get_publisher() {
                        res += publisher;
                    } else if let Some(organization) = parent.get_organization() {
                        res += organization;
                    }
                }
            }
            SourceType::TvSeries(parent) => {
                let mut prods =
                    entry.get_affiliated_filtered(PersonRole::ExecutiveProducer);
                if prods.is_empty() {
                    prods = entry.get_authors().to_vec();
                }
                let mut comma = if !prods.is_empty() {
                    let names = name_list(&prods);
                    match names.len() {
                        0 => false,
                        1 => {
                            res += &format!("{} (Executive Producer)", names[0]);
                            true
                        }
                        _ => {
                            res += &format!(
                                "{} (Executive Producers)",
                                ampersand_list(names)
                            );
                            true
                        }
                    }
                } else {
                    false
                };

                if let Some(title) = parent.get_title_fmt(None, Some(&self.formatter)) {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(Formatting::Italic);
                    res += &title.value.sentence_case;
                    res.commit_formats();
                    comma = false;

                    if parent.get_volume().is_some() || parent.get_edition().is_some() {
                        res.push(' ');
                        res += &ed_vol_str(entry, true);
                        res.push('.');
                    } else {
                        let lc = res.last().unwrap_or('a');

                        if lc != '?' && lc != '.' && lc != '!' {
                            res.push('.');
                        }
                    }
                }

                if comma {
                    res += ".";
                }

                if !res.is_empty() {
                    let mut new = DisplayString::from_str("In ");
                    new += res;
                    res = new;
                }

                if parent.get_publisher().is_some() || parent.get_organization().is_some()
                {
                    res.push(' ');

                    if let Some(publisher) = parent.get_publisher() {
                        res += publisher;
                    } else if let Some(organization) = parent.get_organization() {
                        res += organization;
                    }
                }
            }
            SourceType::Thesis => {
                if let Some(archive) = entry.get_archive() {
                    res += archive;
                } else if let Some(org) = entry.get_organization() {
                    if entry.get_url().is_none() {
                        res += org;
                    }
                }
            }
            SourceType::Manuscript => {
                if let Some(archive) = entry.get_archive() {
                    res += archive;
                }
            }
            SourceType::ArtContainer(parent) => {
                let org = parent
                    .get_organization()
                    .or_else(|| parent.get_archive())
                    .or_else(|| parent.get_publisher())
                    .or_else(|| entry.get_organization())
                    .or_else(|| entry.get_archive())
                    .or_else(|| entry.get_publisher());

                if let Some(org) = org {
                    if let Some(loc) = parent
                        .get_location()
                        .or_else(|| parent.get_archive_location())
                        .or_else(|| entry.get_location())
                        .or_else(|| entry.get_archive_location())
                    {
                        res += &format!("{}, {}.", org, loc);
                    } else {
                        res += org;
                    }
                }
            }
            SourceType::StandaloneArt => {
                let org = entry
                    .get_organization()
                    .or_else(|| entry.get_archive())
                    .or_else(|| entry.get_publisher());

                if let Some(org) = org {
                    if let Some(loc) =
                        entry.get_location().or_else(|| entry.get_archive_location())
                    {
                        res += &format!("{}, {}.", org, loc);
                    } else {
                        res += org;
                    }
                }
            }
            SourceType::StandaloneWeb => {
                let publisher =
                    entry.get_publisher().or_else(|| entry.get_organization());

                if let Some(publisher) = publisher {
                    let authors = entry.get_authors();
                    if authors.len() != 1
                        || authors.get(0).map(|a| a.name.as_ref()) != Some(publisher)
                    {
                        res += publisher;
                    }
                }
            }
            SourceType::Web(parent) => {
                if let Some(title) =
                    parent.get_title_fmt(None, Some(&self.formatter)).map(|t| t.value)
                {
                    let authors = entry.get_authors();
                    if authors.len() != 1
                        || authors.get(0).map(|a| &a.name) != Some(&title.value)
                    {
                        res.start_format(Formatting::Italic);
                        res += &title.sentence_case;
                        res.commit_formats();
                    }
                }
            }
            SourceType::NewsItem(parent) => {
                let comma = if let Some(title) =
                    parent.get_title_fmt(None, Some(&self.formatter)).map(|t| t.value)
                {
                    res.start_format(Formatting::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if let Some(pps) = entry.get_page_range() {
                    if comma {
                        res += ", ";
                    }

                    res += &format_range("", "", &pps);
                }
            }
            SourceType::ConferenceTalk(parent) => {
                let comma = if let Some(title) =
                    parent.get_title_fmt(None, Some(&self.formatter)).map(|t| t.value)
                {
                    res += &title.sentence_case;
                    true
                } else {
                    false
                };

                if let Some(loc) = parent.get_location() {
                    if comma {
                        res += ", ";
                    }

                    res += loc;
                }
            }
            SourceType::GenericParent(parent) => {
                if let Some(title) = parent.get_title() {
                    let preprint = sel!(sel!(alt Id(Article), Id(Book), Id(Anthos)) => Id(Repository));

                    if preprint.apply(entry).is_none() {
                        res.start_format(Formatting::Italic);
                    }
                    res += title;
                    res.commit_formats();
                }
            }
            SourceType::Generic => {
                if entry.get_publisher().is_some() || entry.get_organization().is_some() {
                    if let Some(publisher) = entry.get_publisher() {
                        res += publisher;
                    } else if let Some(organization) = entry.get_organization() {
                        res += organization;
                    }
                }
            }
        }

        let lc = res.last().unwrap_or('a');

        if !res.is_empty() && lc != '?' && lc != '.' && lc != '!' {
            res.push('.');
        }

        if let Some(doi) = entry.get_doi() {
            if !res.is_empty() {
                res.push(' ');
            }

            res.start_format(Formatting::NoHyphenation);
            res += &format!("https://doi.org/{}", doi);
            res.commit_formats();
        } else {
            let reference_entry = sel!(Id(Reference) => Id(Entry));
            let url_str = self.get_retreival_date(
                entry,
                entry.get_date().is_none()
                    || reference_entry.apply(entry).is_some()
                    || (matches!(st, SourceType::StandaloneWeb)
                        && entry.get_parents().unwrap_or_default().is_empty()),
            );
            if let Some(url) = url_str {
                if !res.is_empty() {
                    res.push(' ');
                }
                res += url;
            }
        }

        res
    }
}

impl BibliographyFormatter for ApaBibliographyFormatter {
    fn get_reference(&self, mut entry: &Entry, _prev: Option<&Entry>) -> DisplayString {
        let mut parent = entry.get_parents().and_then(|v| v.first());
        while sel!(alt Id(Chapter), Id(Scene)).matches(entry) {
            if let Some(p) = parent {
                entry = &p;
                parent = entry.get_parents().and_then(|v| v.first());
            } else {
                break;
            }
        }

        let art_plaque = sel!(Wc() => Bind("p", Id(Artwork))).matches(entry);

        let authors = self.get_author(entry);
        let date = self.get_date(entry);
        let title = self.get_title(entry, art_plaque);
        let source = self.get_source(entry);

        let mut res = DisplayString::from_string(authors);

        if res.is_empty() {
            res += title;

            if !date.is_empty() {
                if !res.is_empty() {
                    res += &format!(" {}", date);
                } else {
                    res += &date;
                }
            }
        } else {
            if !date.is_empty() {
                res += &format!(" {}", date);
            }

            if !title.is_empty() {
                if !res.is_empty() {
                    res += " ";
                    res += title;
                } else {
                    res += title;
                }
            }
        }

        if !source.is_empty() {
            if !res.is_empty() {
                res += " ";
                res += source;
            } else {
                res += source;
            }
        }

        if let Some(note) = entry.get_note() {
            if !res.is_empty() {
                res.push(' ');
            }
            res += &format!("({})", note);
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::ApaBibliographyFormatter;
    use crate::types::EntryType;
    use crate::types::Person;
    use crate::Entry;

    #[test]
    fn name_list() {
        let p = vec![
            Person::from_strings(&vec!["van de Graf", "Judith"]),
            Person::from_strings(&vec!["Günther", "Hans-Joseph"]),
            Person::from_strings(&vec!["Mädje", "Laurenz Elias"]),
        ]
        .into_iter()
        .map(|e| e.unwrap())
        .collect();
        let mut entry = Entry::new("test", EntryType::Newspaper);
        entry.set_authors(p);

        let apa = ApaBibliographyFormatter::new();
        assert_eq!(
            "van de Graf, J., Günther, H.-J., & Mädje, L. E.",
            apa.get_author(&entry)
        );
    }
}
