use super::{
    format_range, name_list, name_list_straight, BibliographyGenerator, DisplayString,
    FormatVariantOptions,
};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::SentenceCase;
use crate::types::{
    EntryType, EntryTypeModality, EntryTypeSpec, NumOrStr, Person, PersonRole,
};
use crate::Entry;

#[derive(Clone, Debug)]
pub struct ApaBibliographyGenerator {
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
    StandaloneWebItem,
    WebItem(&'s Entry),
    NewsItem(&'s Entry),
    ConferenceTalk(&'s Entry),
    GenericParent(&'s Entry),
    Generic,
}

impl<'s> SourceType<'s> {
    fn for_entry(entry: &'s Entry) -> Self {
        let periodical = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::Periodical),
        );
        let collection_a = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::InAnthology),
            EntryTypeModality::Specific(EntryType::Anthology),
        );
        let collection_b = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::Entry),
            EntryTypeModality::Any,
        );
        let collection_c = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::Reference),
        );
        let collection_d = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::Article),
            EntryTypeModality::Specific(EntryType::Proceedings),
        );
        let tv_series = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::Video),
            EntryTypeModality::Specific(EntryType::Video),
        );
        let thesis = EntryTypeSpec::with_single(EntryType::Thesis);
        let manuscript = EntryTypeSpec::with_single(EntryType::Manuscript);
        let art_container = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::Artwork),
        );
        let art = EntryTypeSpec::new(
            EntryTypeModality::Alternate(vec![EntryType::Artwork, EntryType::Exhibition]),
            vec![],
        );
        let news_item = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::NewspaperIssue),
        );
        let web_standalone = EntryTypeSpec::with_single(EntryType::WebItem);
        let web_contained_a = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Alternate(vec![
                EntryType::Misc,
                EntryType::Blog,
                EntryType::WebItem,
            ]),
        );
        let web_contained_b = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::WebItem),
            EntryTypeModality::Any,
        );
        let talk = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::Conference),
        );

        let generic_parent =
            EntryTypeSpec::single_parent(EntryTypeModality::Any, EntryTypeModality::Any);

        if let Ok(i) = entry.check_with_spec(periodical) {
            return Self::PeriodicalItem(&entry.get_parents().unwrap()[i[0]]);
        }
        if let Ok(i) = entry
            .check_with_spec(collection_a)
            .or_else(|_| entry.check_with_spec(collection_b))
            .or_else(|_| entry.check_with_spec(collection_c))
            .or_else(|_| entry.check_with_spec(collection_d))
        {
            return Self::CollectionItem(&entry.get_parents().unwrap()[i[0]]);
        }
        if let Ok(i) = entry.check_with_spec(tv_series) {
            if entry.get_issue().is_ok() && entry.get_volume().is_ok() {
                return Self::TvSeries(&entry.get_parents().unwrap()[i[0]]);
            }
        }
        if entry.check_with_spec(thesis).is_ok() {
            return Self::Thesis;
        }
        if entry.check_with_spec(manuscript).is_ok() {
            return Self::Manuscript;
        }
        if let Ok(i) = entry.check_with_spec(art_container) {
            return Self::ArtContainer(&entry.get_parents().unwrap()[i[0]]);
        }
        if entry.check_with_spec(art).is_ok() {
            return Self::StandaloneArt;
        }
        if let Ok(i) = entry.check_with_spec(news_item) {
            return Self::NewsItem(&entry.get_parents().unwrap()[i[0]]);
        }
        if entry.check_with_spec(web_standalone).is_ok() {
            return Self::StandaloneWebItem;
        }
        if let Ok(i) = entry
            .check_with_spec(web_contained_a)
            .or_else(|_| entry.check_with_spec(web_contained_b))
        {
            return Self::WebItem(&entry.get_parents().unwrap()[i[0]]);
        }
        if let Ok(i) = entry.check_with_spec(talk) {
            return Self::ConferenceTalk(&entry.get_parents().unwrap()[i[0]]);
        }
        if let Ok(i) = entry.check_with_spec(generic_parent) {
            return Self::GenericParent(&entry.get_parents().unwrap()[i[0]]);
        }

        return Self::Generic;
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
    let vstr = if let Ok(vols) = entry.get_volume() {
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

    let translator = entry
        .get_affiliated_persons()
        .unwrap_or_default()
        .into_iter()
        .filter(|(_, role)| role == &PersonRole::Translator)
        .map(|(v, _)| v)
        .flatten()
        .cloned()
        .collect::<Vec<Person>>();

    let translator = if translator.is_empty() {
        None
    } else {
        Some(format!(
            "{}, Trans.",
            ampersand_list(name_list_straight(&translator))
        ))
    };

    let estr = if let Ok(ed) = ed {
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

impl ApaBibliographyGenerator {
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
        if entry.entry_type == EntryType::Video {
            let tv_series = EntryTypeSpec::single_parent(
                EntryTypeModality::Specific(EntryType::Video),
                EntryTypeModality::Specific(EntryType::Video),
            );

            if entry.get_issue().is_ok()
                && entry.get_volume().is_ok()
                && entry.check_with_spec(tv_series).is_ok()
            {
                // TV episode
                let dirs = entry
                    .get_affiliated_persons()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Director)
                    .map(|(v, _)| v)
                    .flatten()
                    .cloned()
                    .collect::<Vec<Person>>();
                let mut dir_name_list = name_list(&dirs)
                    .into_iter()
                    .map(|s| format!("{} (Director)", s))
                    .collect::<Vec<String>>();
                let writers = entry
                    .get_affiliated_persons()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Writer)
                    .map(|(v, _)| v)
                    .flatten()
                    .cloned()
                    .collect::<Vec<Person>>();
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
                let dirs = entry
                    .get_affiliated_persons()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Director)
                    .map(|(v, _)| v)
                    .flatten()
                    .cloned()
                    .collect::<Vec<Person>>();

                if !dirs.is_empty() {
                    names = Some(name_list(&dirs));
                    role = AuthorRole::Director;
                } else {
                    // TV show
                    let prods = entry
                        .get_affiliated_persons()
                        .unwrap_or_default()
                        .into_iter()
                        .filter(|(_, role)| role == &PersonRole::ExecutiveProducer)
                        .map(|(v, _)| v)
                        .flatten()
                        .cloned()
                        .collect::<Vec<Person>>();

                    if !prods.is_empty() {
                        names = Some(name_list(&prods));
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let mut authors = names.unwrap_or_else(|| name_list(&entry.get_authors()));
        let count = authors.len();
        let mut al = if !authors.is_empty() {
            if entry.entry_type == EntryType::Tweet {
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
        } else if let Ok(eds) = entry.get_editors() {
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
        let booklike = EntryTypeModality::Alternate(vec![
            EntryType::Book,
            EntryType::Proceedings,
            EntryType::Anthology,
        ]);
        if entry.entry_type.check(booklike) {
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

    fn get_retreival_date(&self, entry: &Entry, use_date: bool) -> Option<String> {
        let url = entry.get_any_url();

        if let Some(qurl) = url {
            let uv = qurl.value.as_str();
            let res = if use_date {
                if let Some(date) = &qurl.visit_date {
                    match (date.month, date.day) {
                        (None, _) => format!("Retrieved {:04}, from {}", date.year, uv),
                        (Some(month), None) => format!(
                            "Retrieved {} {:04}, from {}",
                            get_month_name(month).unwrap(),
                            date.year,
                            uv,
                        ),
                        (Some(month), Some(day)) => format!(
                            "(Retrieved {} {}, {:04}, from {})",
                            get_month_name(month).unwrap(),
                            day,
                            date.year,
                            uv,
                        ),
                    }
                } else {
                    uv.to_string()
                }
            } else {
                uv.to_string()
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
            .any(|p| p.get_title().is_ok())
        {
            let talk = EntryTypeSpec::single_parent(
                EntryTypeModality::Any,
                EntryTypeModality::Specific(EntryType::Conference),
            );
            let preprint = EntryTypeSpec::single_parent(
                EntryTypeModality::Alternate(vec![
                    EntryType::Article,
                    EntryType::Book,
                    EntryType::InAnthology,
                ]),
                EntryTypeModality::Specific(EntryType::Repository),
            );

            entry.check_with_spec(talk).is_ok() || entry.check_with_spec(preprint).is_ok()
        } else {
            true
        };

        let mut res = DisplayString::new();
        let vid_match = entry.check_with_spec(EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::Video),
            EntryTypeModality::Specific(EntryType::Video),
        ));

        let book = entry
            .check_with_spec(EntryTypeSpec::new(
                EntryTypeModality::Alternate(vec![
                    EntryType::Book,
                    EntryType::Report,
                    EntryType::Reference,
                    EntryType::Anthology,
                    EntryType::Proceedings,
                ]),
                vec![],
            ))
            .is_ok();

        if let Ok(title) = entry.get_title_fmt(None, Some(&self.formatter)) {
            let multivol_spec = EntryTypeSpec::with_parents(EntryType::Book, vec![
                EntryTypeSpec::with_single(EntryType::Book),
            ]);

            let mut multivolume_parent = None;
            if let Ok(parents) = entry.check_with_spec(multivol_spec) {
                if entry.get_volume().is_ok() {
                    multivolume_parent = Some(parents[0]);
                }
            }

            if italicise {
                res.start_format(FormatVariantOptions::Italic);
            }
            if entry.entry_type == EntryType::Tweet {
                let words = &title.value.split_whitespace().collect::<Vec<_>>();
                res += &words[.. (if words.len() >= 20 { 20 } else { words.len() })]
                    .join(" ");
            } else {
                res += &title.sentence_case;
            }
            res.commit_formats();

            if let Some(mv_parent) = multivolume_parent {
                let p: &Entry = &entry.get_parents().unwrap()[mv_parent];
                let vols = entry.get_volume().unwrap();
                let mut new = DisplayString::from_string(format!(
                    "{}: {} ",
                    p.get_title_fmt(None, Some(&self.formatter)).unwrap().sentence_case,
                    format_range("Vol.", "Vols.", &vols),
                ));
                new += res;
                res = new;
            } else if (entry.get_volume().is_ok() || entry.get_edition().is_ok()) && book
            {
                res += &ed_vol_str(entry, false);
            } else if (entry.get_volume().is_ok() || entry.get_issue().is_ok())
                && vid_match.is_ok()
            {
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

            if entry.get_note().and_then(|_| entry.get_editors()).is_ok()
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
        } else if entry.entry_type == EntryType::Report {
            if let Ok(serial) = entry.get_serial_number() {
                items.push(serial.to_string());
            }
        } else if entry.entry_type == EntryType::Thesis {
            if let Ok(serial) = entry.get_serial_number() {
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

        let conf_spec = EntryTypeSpec::single_parent(
            EntryTypeModality::Specific(EntryType::Article),
            EntryTypeModality::Specific(EntryType::Proceedings),
        );
        let talk_spec = EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Specific(EntryType::Conference),
        );
        let repo_item = EntryTypeSpec::single_parent(
            EntryTypeModality::Disallowed(vec![
                EntryType::Article,
                EntryType::Report,
                EntryType::Thesis,
            ]),
            EntryTypeModality::Specific(EntryType::Repository),
        );

        let spec = if entry.entry_type == EntryType::Case {
            TitleSpec::LegalProceedings
        } else if entry.check_with_spec(conf_spec).is_ok() {
            TitleSpec::PaperPresentation
        } else if entry.check_with_spec(talk_spec).is_ok() {
            TitleSpec::ConferenceSession
        } else if entry.entry_type == EntryType::Thesis {
            if entry.get_archive().is_ok() || entry.get_url().is_ok() {
                TitleSpec::Thesis
            } else {
                TitleSpec::UnpublishedThesis
            }
        } else if entry.entry_type == EntryType::Exhibition {
            TitleSpec::Exhibition
        } else if entry.entry_type == EntryType::Repository {
            TitleSpec::SoftwareRepository
        } else if entry.check_with_spec(repo_item).is_ok() {
            TitleSpec::SoftwareRepositoryItem
        } else if entry.entry_type == EntryType::Audio {
            TitleSpec::Audio
        } else if entry.entry_type == EntryType::Video {
            let dirs = entry
                .get_affiliated_persons()
                .unwrap_or_default()
                .into_iter()
                .filter(|(_, role)| role == &PersonRole::Director)
                .map(|(v, _)| v)
                .flatten()
                .collect::<Vec<&Person>>();
            if !dirs.is_empty()
                && entry.get_total_volumes().is_err()
                && entry.get_parents().is_err()
            {
                TitleSpec::Film
            } else {
                let is_online_vid = if let Ok(url) = entry.get_url() {
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
                    let prods = entry
                        .get_affiliated_persons()
                        .unwrap_or_default()
                        .into_iter()
                        .filter(|(_, role)| role == &PersonRole::ExecutiveProducer)
                        .map(|(v, _)| v)
                        .flatten()
                        .collect::<Vec<&Person>>();

                    if vid_match.is_ok()
                        && entry.get_issue().is_ok()
                        && entry.get_volume().is_ok()
                    {
                        TitleSpec::TvEpisode
                    } else if !prods.is_empty() || entry.get_total_volumes().is_ok() {
                        TitleSpec::TvShow
                    } else {
                        TitleSpec::Video
                    }
                }
            }
        } else if entry.entry_type == EntryType::Tweet {
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

            let  printed = if spec == TitleSpec::Thesis {
                if let Ok(org) = entry.get_organization() {
                    res += &format!("[{}, {}]", append, org);
                    true
                } else {
                    false
                }
            } else { false };

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
                let mut comma = if let Ok(title) =
                    parent.get_title_fmt(None, Some(&self.formatter))
                {
                    res.start_format(FormatVariantOptions::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if parent.get_volume().is_ok() || parent.get_issue().is_ok() {
                    if comma {
                        res += ", ";
                    }

                    if let Ok(volume) = parent.get_volume() {
                        res += &format_range("", "", &volume);
                    }

                    if let Ok(issue) = parent.get_issue() {
                        res += &format!("({})", issue);
                    }
                    comma = true;
                }

                if entry.get_serial_number().is_ok() || entry.get_page_range().is_ok() {
                    if comma {
                        res += ", ";
                    }

                    if let Ok(sn) = entry.get_serial_number() {
                        res += "Article ";
                        res += sn;
                    } else if let Ok(pages) = entry.get_page_range() {
                        res += &format_range("", "", &pages);
                    }
                }
            }
            SourceType::CollectionItem(parent) => {
                let mut comma = if let Ok(eds) = parent.get_editors() {
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

                if let Ok(title) = parent.get_title_fmt(None, Some(&self.formatter)) {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(FormatVariantOptions::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    comma = true;

                    if parent.get_volume().is_ok() || parent.get_edition().is_ok() {
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

                if parent.get_publisher().is_ok() || parent.get_organization().is_ok() {
                    res.push(' ');

                    if let Ok(publisher) = parent.get_publisher() {
                        res += publisher;
                    } else if let Ok(organization) = parent.get_organization() {
                        res += organization;
                    }
                }
            }
            SourceType::TvSeries(parent) => {
                let prods = parent
                    .get_affiliated_persons()
                    .map(|p| {
                        p.into_iter()
                            .filter(|x| x.1 == PersonRole::ExecutiveProducer)
                            .map(|x| &x.0)
                            .flatten()
                            .cloned()
                            .collect::<Vec<Person>>()
                    })
                    .unwrap_or_else(|_| entry.get_authors().to_vec());
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

                if let Ok(title) = parent.get_title_fmt(None, Some(&self.formatter)) {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(FormatVariantOptions::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    comma = false;

                    if parent.get_volume().is_ok() || parent.get_edition().is_ok() {
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

                if parent.get_publisher().is_ok() || parent.get_organization().is_ok() {
                    res.push(' ');

                    if let Ok(publisher) = parent.get_publisher() {
                        res += publisher;
                    } else if let Ok(organization) = parent.get_organization() {
                        res += organization;
                    }
                }
            }
            SourceType::Thesis => {
                if let Ok(archive) = entry.get_archive() {
                    res += archive;
                } else if let Ok(org) = entry.get_organization() {
                    if entry.get_url().is_err() {
                        res += org;
                    }
                }
            }
            SourceType::Manuscript => {
                if let Ok(archive) = entry.get_archive() {
                    res += archive;
                }
            }
            SourceType::ArtContainer(parent) => {
                let org = parent
                    .get_organization()
                    .or_else(|_| parent.get_archive())
                    .or_else(|_| parent.get_publisher())
                    .or_else(|_| entry.get_organization())
                    .or_else(|_| entry.get_archive())
                    .or_else(|_| entry.get_publisher());

                if let Ok(org) = org {
                    if let Ok(loc) = parent
                        .get_location()
                        .or_else(|_| parent.get_archive_location())
                        .or_else(|_| entry.get_location())
                        .or_else(|_| entry.get_archive_location())
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
                    .or_else(|_| entry.get_archive())
                    .or_else(|_| entry.get_publisher());

                if let Ok(org) = org {
                    if let Ok(loc) =
                        entry.get_location().or_else(|_| entry.get_archive_location())
                    {
                        res += &format!("{}, {}.", org, loc);
                    } else {
                        res += org;
                    }
                }
            }
            SourceType::StandaloneWebItem => {
                let publisher = entry
                    .get_publisher()
                    .or_else(|_| entry.get_organization());

                if let Ok(publisher) = publisher {
                    let authors = entry.get_authors();
                    if authors.len() != 1
                        || authors.get(0).map(|a| a.name.as_ref()) != Some(publisher)
                    {
                        res += publisher;
                    }
                }
            }
            SourceType::WebItem(parent) => {
                if let Ok(title) = parent.get_title_fmt(None, Some(&self.formatter)) {
                    let authors = entry.get_authors();
                    if authors.len() != 1
                        || authors.get(0).map(|a| &a.name) != Some(&title.value)
                    {
                        res.start_format(FormatVariantOptions::Italic);
                        res += &title.sentence_case;
                        res.commit_formats();
                    }
                }
            }
            SourceType::NewsItem(parent) => {
                let comma = if let Ok(title) =
                    parent.get_title_fmt(None, Some(&self.formatter))
                {
                    res.start_format(FormatVariantOptions::Italic);
                    res += &title.sentence_case;
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if let Ok(pps) = entry.get_page_range() {
                    if comma {
                        res += ", ";
                    }

                    res += &format_range("", "", &pps);
                }
            }
            SourceType::ConferenceTalk(parent) => {
                let comma = if let Ok(title) =
                    parent.get_title_fmt(None, Some(&self.formatter))
                {
                    res += &title.sentence_case;
                    true
                } else {
                    false
                };

                if let Ok(loc) = parent.get_location() {
                    if comma {
                        res += ", ";
                    }

                    res += loc;
                }
            }
            SourceType::GenericParent(parent) => {
                if let Ok(title) = parent.get_title() {
                    let preprint = EntryTypeSpec::single_parent(
                        EntryTypeModality::Alternate(vec![
                            EntryType::Article,
                            EntryType::Book,
                            EntryType::Entry,
                        ]),
                        EntryTypeModality::Specific(EntryType::Repository),
                    );

                    if !entry.check_with_spec(preprint).is_ok() {
                        res.start_format(FormatVariantOptions::Italic);
                    }
                    res += title;
                    res.commit_formats();
                }
            }
            SourceType::Generic => {
                if entry.get_publisher().is_ok() || entry.get_organization().is_ok() {
                    if let Ok(publisher) = entry.get_publisher() {
                        res += publisher;
                    } else if let Ok(organization) = entry.get_organization() {
                        res += organization;
                    }
                }
            }
        }

        let lc = res.last().unwrap_or('a');

        if !res.is_empty() && lc != '?' && lc != '.' && lc != '!' {
            res.push('.');
        }

        if let Ok(doi) = entry.get_doi() {
            if !res.is_empty() {
                res.push(' ');
            }

            res += &format!("https://doi.org/{}", doi);
        } else {
            let reference_entry = EntryTypeSpec::single_parent(
                EntryTypeModality::Specific(EntryType::Entry),
                EntryTypeModality::Specific(EntryType::Reference),
            );
            let url_str = self.get_retreival_date(
                entry,
                entry.get_date().is_err()
                    || entry.check_with_spec(reference_entry).is_ok()
                    || (matches!(st, SourceType::StandaloneWebItem)
                        && entry.get_parents().unwrap_or_default().is_empty()),
            );
            if let Some(url) = url_str {
                if !res.is_empty() {
                    res.push(' ');
                }
                res += &url;
            }
        }

        res
    }
}

impl BibliographyGenerator for ApaBibliographyGenerator {
    fn get_reference(&self, mut entry: &Entry) -> DisplayString {
        let mut parent = entry.get_parents().ok().and_then(|v| v.first());
        while entry.entry_type.check(EntryTypeModality::Alternate(vec![
            EntryType::Chapter,
            EntryType::Scene,
        ])) {
            if let Some(p) = parent {
                entry = &p;
                parent = entry.get_parents().ok().and_then(|v| v.first());
            } else {
                break;
            }
        }

        let art_plaque = entry
            .check_with_spec(EntryTypeSpec::single_parent(
                EntryTypeModality::Any,
                EntryTypeModality::Specific(EntryType::Artwork),
            ))
            .is_ok();

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

        if let Ok(note) = entry.get_note() {
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
    use super::ApaBibliographyGenerator;
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
        let mut entry = Entry::new("test", EntryType::NewspaperIssue);
        entry.set_authors(p);

        let apa = ApaBibliographyGenerator::new();
        assert_eq!(
            "van de Graf, J., Günther, H.-J., & Mädje, L. E.",
            apa.get_author(&entry)
        );
    }
}
