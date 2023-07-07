use super::{
    alph_designator, delegate_titled_entry, format_range, name_list, name_list_straight,
    sorted_bibliography, BibliographyOrdering, BibliographyStyle, Database,
    DisplayReference, DisplayString, Formatting, Record,
};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::SentenceCase;
use crate::types::{EntryType::*, FmtOptionExt, NumOrStr, Person, PersonRole};
use crate::Entry;

/// Bibliographies following APA guidance.
///
/// # Examples
/// - Henrich, J., Heine, S. J., & Norenzayan, A. (2010, June 15). The weirdest
///   people in the world? _Behavioral and brain sciences,_ 33(2-3), 61–83.
///   <https://doi.org/10.1017/S0140525X0999152X>
/// - Gluck, M. A., Mercado, E., & Myers, C. E. (2020). _Learning and memory_
///   (4th ed.).
/// - Davidson, A. (Director), & McKenna, C. (Writer). (2010, November 18).
///   Conspiracy theories and interior design (Season 9, Episode 2) [TV series
///   episode]. In _Community._ Universal Television; Sony Pictures Television;
///   Krasnoff Foster Productions; Harmonious Claptrap; Russo Brothers Film.
///
/// # Reference
/// See the [7th edition of the APA Publication Manual][apa] for details on how
/// the American Psychological Association advises you to format citations and
/// bibliographies.
///
/// [apa]: https://apastyle.apa.org/style-grammar-guidelines/references
#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Apa {
    /// The configuration for sentence case formatting.
    pub sentence_case: SentenceCase,
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
        let periodical = select!(* > ("p":Periodical));
        let collection = select!(
            (Anthos > ("p":Anthology)) |
            (Entry  > ("p":*)) |
            (* > ("p":Reference)) |
            (Article > ("p":Proceedings))
        );
        let tv_series = select!((Video["issue", "volume"]) > ("p":Video));
        let thesis = select!(Thesis);
        let manuscript = select!(Manuscript);
        let art_container = select!(* > ("p":Artwork));
        let art = select!(Artwork | Exhibition);
        let news_item = select!(* > ("p":Newspaper));
        let web_standalone = select!(Web);
        let web_contained = select!(
            (Web > ("p":*)) |
            (* > ("p":((Misc["url"]) | Blog | Web)))
        );
        let talk = select!(* > ("p":Conference));
        let generic_parent = select!(* > ("p":*));
        let series = select!(
            (Book > Book) | (Anthology > Anthology) | (Proceedings > Proceedings)
        );

        if let Some(binding) = periodical.bound(entry, "p") {
            Self::PeriodicalItem(binding)
        } else if let Some(binding) = collection.bound(entry, "p") {
            Self::CollectionItem(binding)
        } else if let Some(binding) = tv_series.bound(entry, "p") {
            Self::TvSeries(binding)
        } else if thesis.matches(entry) {
            Self::Thesis
        } else if manuscript.matches(entry) {
            Self::Manuscript
        } else if let Some(binding) = art_container.bound(entry, "p") {
            Self::ArtContainer(binding)
        } else if art.matches(entry) {
            Self::StandaloneArt
        } else if let Some(binding) = news_item.bound(entry, "p") {
            Self::NewsItem(binding)
        } else if web_standalone.matches(entry) {
            Self::StandaloneWeb
        } else if let Some(binding) = web_contained.bound(entry, "p") {
            Self::Web(binding)
        } else if let Some(binding) = talk.bound(entry, "p") {
            Self::ConferenceTalk(binding)
        } else if let Some(binding) = generic_parent.bound(entry, "p") {
            if series.matches(entry) {
                Self::Generic
            } else {
                Self::GenericParent(binding)
            }
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
    let vstr = if let Some(vols) = entry.volume() {
        if is_tv_show {
            Some(format_range("Episode", "Episodes", &vols))
        } else {
            Some(format_range("Vol.", "Vols.", &vols))
        }
    } else {
        None
    };

    let ed = if is_tv_show { entry.issue() } else { entry.edition() };

    let translator = entry.affiliated_with_role(PersonRole::Translator);

    let translator = if translator.is_empty() {
        None
    } else {
        Some(format!("{}, Trans.", ampersand_list(name_list_straight(&translator))))
    };

    let estr = if let Some(ed) = ed {
        if is_tv_show {
            Some(format!("Season {}", ed))
        } else {
            Some(format!(
                "{} ed.",
                match ed {
                    NumOrStr::Number(e) => get_ordinal(*e),
                    NumOrStr::Str(s) => s.to_string(),
                }
            ))
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

impl Apa {
    /// Creates a new bibliography generator.
    pub fn new() -> Self {
        Self::default()
    }

    fn get_author(&self, entry: &Entry) -> (String, Vec<Person>) {
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
        let mut pers_refs = vec![];
        if entry.entry_type == Video {
            let tv_series = select!((Video["issue", "volume"]) > Video);
            let dirs = entry.affiliated_with_role(PersonRole::Director);

            if tv_series.apply(entry).is_some() {
                // TV episode
                let mut dir_name_list = name_list(&dirs)
                    .into_iter()
                    .map(|s| format!("{} (Director)", s))
                    .collect::<Vec<String>>();

                let writers = entry.affiliated_with_role(PersonRole::Writer);
                let mut writers_name_list = name_list(&writers)
                    .into_iter()
                    .map(|s| format!("{} (Writer)", s))
                    .collect::<Vec<String>>();
                dir_name_list.append(&mut writers_name_list);

                if !dirs.is_empty() {
                    names = Some(dir_name_list);
                    pers_refs.extend(dirs);
                    pers_refs.extend(writers);
                }
            } else {
                // Film
                if !dirs.is_empty() {
                    names = Some(name_list(&dirs));
                    pers_refs.extend(dirs);
                    role = AuthorRole::Director;
                } else {
                    // TV show
                    let prods = entry.affiliated_with_role(PersonRole::ExecutiveProducer);

                    if !prods.is_empty() {
                        names = Some(name_list(&prods));
                        pers_refs.extend(prods);
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let authors = if let Some(names) = names {
            Some(names)
        } else if let Some(authors) = entry.authors() {
            let list = name_list(&authors);
            pers_refs.extend(authors.iter().cloned());
            Some(list)
        } else {
            None
        };

        let mut al = if let Some(mut authors) = authors {
            let count = authors.len();
            if entry.entry_type == Tweet {
                authors = authors
                    .into_iter()
                    .enumerate()
                    .map(|(i, n)| {
                        if let Some(handle) = entry.twitter_handle(i) {
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
        } else if let Some(eds) = entry.editors() {
            let res = if !eds.is_empty() {
                format!(
                    "{} ({})",
                    ampersand_list(name_list(&eds)),
                    if eds.len() == 1 { "Ed." } else { "Eds." }
                )
            } else {
                String::new()
            };
            pers_refs.extend(eds.iter().cloned());
            res
        } else {
            String::new()
        };

        let mut details = vec![];
        let booklike = select!(Book | Proceedings | Anthology);
        if booklike.apply(entry).is_some() {
            let affs = entry
                .affiliated_persons()
                .unwrap_or_default()
                .iter()
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
                pers_refs.extend(affs);
            }
        }

        if !details.is_empty() {
            if !al.is_empty() {
                al.push(' ');
            }

            al += &details[0];

            for e in &details[1..] {
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

        (al, pers_refs)
    }

    fn get_date(&self, entry: &Entry, disambiguation: Option<usize>) -> String {
        if let Some(date) = entry.date_any() {
            let suppress_exact = select!(Book | Anthology).matches(entry);
            let letter = if let Some(disamb) = disambiguation {
                alph_designator(disamb).into()
            } else {
                String::new()
            };

            match (date.month, date.day) {
                (Some(month), None) if !suppress_exact => format!(
                    "({}{}, {}).",
                    date.display_year(),
                    letter,
                    get_month_name(month).unwrap()
                ),
                (Some(month), Some(day)) if !suppress_exact => format!(
                    "({}{}, {} {}).",
                    date.display_year(),
                    letter,
                    get_month_name(month).unwrap(),
                    day + 1,
                ),
                _ => format!("({}{}).", date.display_year(), letter),
            }
        } else {
            "(n. d.).".to_string()
        }
    }

    fn get_retrieval_date(&self, entry: &Entry, use_date: bool) -> Option<DisplayString> {
        let url = entry.url_any();

        if let Some(qurl) = url {
            let uv = qurl.value.as_str();
            let res = if use_date {
                if let Some(date) = &qurl.visit_date {
                    match (date.month, date.day) {
                        (None, _) => {
                            let mut res = DisplayString::from_string(format!(
                                "Retrieved {}, from ",
                                date.display_year()
                            ));
                            res.start_format(Formatting::Link(uv.into()));
                            res += uv;
                            res.commit_formats();
                            res
                        }
                        (Some(month), None) => {
                            let mut res = DisplayString::from_string(format!(
                                "Retrieved {} {}, from ",
                                get_month_name(month).unwrap(),
                                date.display_year(),
                            ));
                            res.start_format(Formatting::Link(uv.into()));
                            res += uv;
                            res.commit_formats();
                            res
                        }
                        (Some(month), Some(day)) => {
                            let mut res = DisplayString::from_string(format!(
                                "(Retrieved {} {}, {}, from ",
                                get_month_name(month).unwrap(),
                                day + 1,
                                date.display_year(),
                            ));
                            res.start_format(Formatting::Link(uv.into()));
                            res += uv;
                            res.commit_formats();
                            res.push(')');
                            res
                        }
                    }
                } else {
                    let mut res = DisplayString::new();
                    res.start_format(Formatting::Link(uv.into()));
                    res += uv;
                    res.commit_formats();
                    res
                }
            } else {
                let mut res = DisplayString::new();
                res.start_format(Formatting::Link(uv.into()));
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
        let italicise =
            if let Some(titled) = select!(* > ("p":(*["title"]))).bound(entry, "p") {
                let talk = select!(* > Conference);
                let preprint = select!((Article | Book | Anthos) > Repository);
                let booklike = select!(Book | Proceedings | Anthology);

                talk.matches(entry)
                    || preprint.matches(entry)
                    || (titled.kind() == entry.kind() && booklike.matches(entry))
            } else {
                true
            };

        let mut res = DisplayString::new();
        let vid_match = select!((Video["issue", "volume"]) > Video);

        let book =
            select!(Book | Report | Reference | Anthology | Proceedings).matches(entry);

        if let Some(title) = entry.title() {
            let sent_cased = title.canonical.format_sentence_case(&self.sentence_case);
            let multivol_spec = select!(
                ((Book | Proceedings | Anthology)["volume"])
                > ("p":(Book | Proceedings | Anthology))
            );

            let multivolume_parent = multivol_spec.bound(entry, "p");

            if italicise {
                res.start_format(Formatting::Italic);
            }
            if entry.entry_type == Tweet {
                let words = &title.canonical.value.split_whitespace().collect::<Vec<_>>();
                res += &words[..(if words.len() >= 20 { 20 } else { words.len() })]
                    .join(" ");
            } else {
                res += &sent_cased;
            }
            res.commit_formats();

            if let Some(mv_parent) = multivolume_parent {
                let vols = entry.volume().unwrap();
                let mut new = DisplayString::from_string(format!(
                    "{}: {} ",
                    mv_parent
                        .title()
                        .unwrap()
                        .canonical
                        .format_sentence_case(&self.sentence_case),
                    format_range("Vol.", "Vols.", &vols),
                ));
                new += res;
                res = new;
            } else if (entry.volume().is_some() || entry.edition().is_some()) && book {
                res += &ed_vol_str(entry, false);
            } else if vid_match.apply(entry).is_some() {
                res += &ed_vol_str(entry, true);
            }
        }

        let mut items: Vec<String> = vec![];
        if book {
            let illustrators = entry.affiliated_with_role(PersonRole::Illustrator);
            if !illustrators.is_empty() {
                items.push(format!(
                    "{}, Illus.",
                    ampersand_list(name_list_straight(&illustrators))
                ));
            }

            if entry.note().and_then(|_| entry.editors()).is_some()
                && !entry.authors().unwrap_or_default().is_empty()
            {
                let editors = entry.editors().unwrap();
                let amp_list = ampersand_list(name_list_straight(&editors));
                if editors.len() == 1 {
                    items.push(format!("{}, Ed.", amp_list));
                } else if editors.len() > 1 {
                    items.push(format!("{}, Eds.", amp_list));
                }
            }
        } else if entry.entry_type == Report {
            if let Some(serial) = entry.serial_number() {
                items.push(serial.to_string());
            }
        } else if entry.entry_type == Thesis {
            if let Some(serial) = entry.serial_number() {
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

        let conf_spec = select!(Article > Proceedings);
        let talk_spec = select!(* > Conference);
        let repo_item = select!((!(Article | Report | Thesis)) > Repository);
        let spec = if entry.entry_type == Case {
            TitleSpec::LegalProceedings
        } else if conf_spec.apply(entry).is_some() {
            TitleSpec::PaperPresentation
        } else if talk_spec.apply(entry).is_some() {
            TitleSpec::ConferenceSession
        } else if entry.entry_type == Thesis {
            if entry.archive().is_some() || entry.url().is_some() {
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
            if entry
                .affiliated_persons()
                .unwrap_or_default()
                .iter()
                .filter(|(_, role)| role == &PersonRole::Director)
                .map(|(v, _)| v)
                .flatten()
                .next()
                .is_none()
                && entry.volume_total().is_none()
                && entry.parents().is_none()
            {
                TitleSpec::Film
            } else {
                let is_online_vid = if let Some(url) = entry.url() {
                    matches!(
                        url.value.host_str().unwrap_or("").to_lowercase().as_ref(),
                        "youtube.com" | "dailymotion.com" | "vimeo.com"
                    )
                } else {
                    false
                };

                if is_online_vid {
                    TitleSpec::Video
                } else {
                    let prods = entry.affiliated_with_role(PersonRole::ExecutiveProducer);

                    if vid_match.apply(entry).is_some() {
                        TitleSpec::TvEpisode
                    } else if !prods.is_empty() || entry.volume_total().is_some() {
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
                if let Some(org) = entry.organization() {
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
            let mut new = DisplayString::from_string("[");
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
                let mut comma = if let Some(title) = parent.title() {
                    res.start_format(Formatting::Italic);
                    res += &title.canonical.format_sentence_case(&self.sentence_case);
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if parent.volume().is_some() || parent.issue().is_some() {
                    if comma {
                        res += ", ";
                    }

                    if let Some(volume) = parent.volume() {
                        res += &format_range("", "", &volume);
                    }

                    if let Some(issue) = parent.issue() {
                        res += &format!("({})", issue);
                    }
                    comma = true;
                }

                if entry.serial_number().is_some() || entry.page_range().is_some() {
                    if comma {
                        res += ", ";
                    }

                    if let Some(sn) = entry.serial_number() {
                        res += "Article ";
                        res += sn;
                    } else if let Some(pages) = entry.page_range() {
                        res += &format_range("", "", &pages);
                    }
                }
            }
            SourceType::CollectionItem(parent) => {
                let mut comma = if let Some(eds) = parent.editors() {
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

                if let Some(title) = parent.title() {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(Formatting::Italic);
                    res += &title.canonical.format_sentence_case(&self.sentence_case);
                    res.commit_formats();
                    comma = true;

                    if parent.volume().is_some() || parent.edition().is_some() {
                        res += &ed_vol_str(parent, false);
                        res.push('.');
                        comma = false;
                    }
                }

                if comma {
                    res += ".";
                }

                if !res.is_empty() {
                    let mut new = DisplayString::from_string("In ");
                    new += res;
                    res = new;
                }

                if parent.publisher().is_some() || parent.organization().is_some() {
                    res.push(' ');

                    if let Some(publisher) = parent.publisher() {
                        res += &publisher.value;
                    } else if let Some(organization) = parent.organization() {
                        res += organization;
                    }
                }
            }
            SourceType::TvSeries(parent) => {
                let mut prods = entry.affiliated_with_role(PersonRole::ExecutiveProducer);
                if prods.is_empty() {
                    prods = entry.authors().unwrap_or_default().to_vec();
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

                if let Some(title) = parent.title() {
                    if comma {
                        res += ", ";
                    }

                    res.start_format(Formatting::Italic);
                    res += &title.canonical.format_sentence_case(&self.sentence_case);
                    res.commit_formats();
                    comma = false;

                    if parent.volume().is_some() || parent.edition().is_some() {
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
                    let mut new = DisplayString::from_string("In ");
                    new += res;
                    res = new;
                }

                if parent.publisher().is_some() || parent.organization().is_some() {
                    res.push(' ');

                    if let Some(publisher) = parent.publisher() {
                        res += &publisher.value;
                    } else if let Some(organization) = parent.organization() {
                        res += organization;
                    }
                }
            }
            SourceType::Thesis => {
                if let Some(archive) = entry.archive() {
                    res += &archive.value;
                } else if let Some(org) = entry.organization() {
                    if entry.url().is_none() {
                        res += org;
                    }
                }
            }
            SourceType::Manuscript => {
                if let Some(archive) = entry.archive() {
                    res += &archive.value;
                }
            }
            SourceType::ArtContainer(parent) => {
                let org = parent
                    .organization()
                    .or_else(|| parent.archive().value())
                    .or_else(|| parent.publisher().value())
                    .or_else(|| entry.organization())
                    .or_else(|| entry.archive().value())
                    .or_else(|| entry.publisher().value());

                if let Some(org) = org {
                    if let Some(loc) = parent
                        .location()
                        .or_else(|| parent.archive_location())
                        .or_else(|| entry.location())
                        .or_else(|| entry.archive_location())
                    {
                        res += &format!("{}, {}.", org, loc.value);
                    } else {
                        res += org;
                    }
                }
            }
            SourceType::StandaloneArt => {
                let org = entry
                    .organization()
                    .or_else(|| entry.archive().value())
                    .or_else(|| entry.publisher().value());

                if let Some(org) = org {
                    if let Some(loc) =
                        entry.location().or_else(|| entry.archive_location())
                    {
                        res += &format!("{}, {}.", org, loc.value);
                    } else {
                        res += org;
                    }
                }
            }
            SourceType::StandaloneWeb => {
                let publisher =
                    entry.publisher().value().or_else(|| entry.organization());

                if let Some(publisher) = publisher {
                    let authors = entry.authors().unwrap_or_default();
                    if authors.len() != 1
                        || authors.get(0).map(|a| a.name.as_ref()) != Some(publisher)
                    {
                        res += publisher;
                    }
                }
            }
            SourceType::Web(parent) => {
                if let Some(title) = parent.title().map(|t| &t.canonical) {
                    let authors = entry.authors().unwrap_or_default();
                    if authors.len() != 1
                        || authors.get(0).map(|a| &a.name) != Some(&title.value)
                    {
                        res.start_format(Formatting::Italic);
                        res += &title.format_sentence_case(&self.sentence_case);
                        res.commit_formats();
                    }
                }
            }
            SourceType::NewsItem(parent) => {
                let comma = if let Some(title) = parent.title().map(|t| &t.canonical) {
                    res.start_format(Formatting::Italic);
                    res += &title.format_sentence_case(&self.sentence_case);
                    res.commit_formats();
                    true
                } else {
                    false
                };

                if let Some(pps) = entry.page_range() {
                    if comma {
                        res += ", ";
                    }

                    res += &format_range("", "", &pps);
                }
            }
            SourceType::ConferenceTalk(parent) => {
                let comma = if let Some(title) = parent.title().map(|t| &t.canonical) {
                    res += &title.format_sentence_case(&self.sentence_case);
                    true
                } else {
                    false
                };

                if let Some(loc) = parent.location() {
                    if comma {
                        res += ", ";
                    }

                    res += &loc.value;
                }
            }
            SourceType::GenericParent(parent) => {
                if let Some(title) = parent.title() {
                    let preprint = select!((Article | Book | Anthos) > Repository);
                    if preprint.apply(entry).is_none() {
                        res.start_format(Formatting::Italic);
                    }
                    res += &title.canonical.value;
                    res.commit_formats();
                }
            }
            SourceType::Generic => {
                if entry.publisher().is_some() || entry.organization().is_some() {
                    if let Some(publisher) = entry.publisher() {
                        res += &publisher.value;
                    } else if let Some(organization) = entry.organization() {
                        res += organization;
                    }
                }
            }
        }

        let lc = res.last().unwrap_or('a');

        if !res.is_empty() && lc != '?' && lc != '.' && lc != '!' {
            res.push('.');
        }

        if let Some(doi) = entry.doi() {
            if !res.is_empty() {
                res.push(' ');
            }

            let link = format!("https://doi.org/{}", doi);
            res.start_format(Formatting::Link(link.clone()));
            res += &link;
            res.commit_formats();
        } else {
            let reference_entry = select!(Reference > Entry);
            let url_str = self.get_retrieval_date(
                entry,
                entry.date().is_none()
                    || reference_entry.apply(entry).is_some()
                    || (matches!(st, SourceType::StandaloneWeb)
                        && entry.parents().unwrap_or_default().is_empty()),
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

    fn get_single_record<'a>(
        &self,
        record: &Record<'a>,
    ) -> (DisplayReference<'a>, Vec<Person>) {
        let entry = delegate_titled_entry(record.entry);
        let art_plaque = select!(* > ("p":Artwork)).matches(entry);

        let (authors, al) = self.get_author(entry);
        let date = self.get_date(entry, record.disambiguation);
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

        if let Some(note) = entry.note() {
            if !res.is_empty() {
                res.push(' ');
            }
            res += &format!("({})", note);
        }

        (
            DisplayReference::new(
                record.entry,
                record.prefix.clone().map(Into::into),
                res,
            ),
            al,
        )
    }
}

impl<'a> BibliographyStyle<'a> for Apa {
    fn bibliography(
        &self,
        db: &Database<'a>,
        ordering: BibliographyOrdering,
    ) -> Vec<DisplayReference<'a>> {
        let mut items = vec![];

        for record in db.records() {
            items.push(self.get_single_record(record));
        }

        sorted_bibliography(items, ordering)
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        self.get_single_record(record).0
    }

    fn ordering(&self) -> BibliographyOrdering {
        BibliographyOrdering::ByAuthor
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use url::Url;

    use super::Apa;
    use crate::types::Date;
    use crate::types::EntryType;
    use crate::types::Person;
    use crate::types::QualifiedUrl;
    use crate::Entry;

    #[test]
    fn name_list() {
        let p = vec![
            Person::from_strings(&["van de Graf", "Judith"]),
            Person::from_strings(&["Günther", "Hans-Joseph"]),
            Person::from_strings(&["Mädje", "Laurenz Elias"]),
        ]
        .into_iter()
        .map(|e| e.unwrap())
        .collect();
        let mut entry = Entry::new("test", EntryType::Newspaper);
        entry.set_authors(p);

        let apa = Apa::new();
        assert_eq!(
            "van de Graf, J., Günther, H.-J., & Mädje, L. E.",
            apa.get_author(&entry).0
        );
    }

    #[test]
    fn retrieved_date() {
        let mut entry = Entry::new("test", EntryType::Article);
        entry.set_url(QualifiedUrl {
            value: Url::parse("https://example.net").unwrap(),
            visit_date: Some(Date::from_str("2023-03-1").unwrap()),
        });

        let apa = Apa::new();
        assert_eq!(
            "(Retrieved March 1, 2023, from https://example.net/)",
            apa.get_retrieval_date(&entry, true).unwrap().value
        );
    }
}
