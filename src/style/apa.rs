use super::{
    alph_designator, delegate_titled_entry, format_range, name_list, name_list_straight,
    sorted_bibliography, AuthorUniqueness, BibliographyOrdering, BibliographyStyle,
    Brackets, Citation, CitationStyle, Database, DisplayCitation, DisplayReference,
    DisplayString, Formatting, Record,
};
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::SentenceCase;
use crate::types::{EntryType::*, FmtOptionExt, NumOrStr, Person, PersonRole, Title};
use crate::Entry;

/// The form of an APA citation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ApaCitationForm {
    /// Enclosed in parentheses.
    Parenthetical,
    /// Only the year is enclosed in parentheses.
    Narrative,
}

/// Citations and bibliographies following APA guidance.
///
/// # Examples
/// Citations:
/// - Angell, 1997
/// - Davidson & McKenna, 2010
///
/// Bibliography:
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
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Apa {
    /// The configuration for sentence case formatting.
    pub sentence_case: SentenceCase,
    /// The form of citation to use.
    pub citation_form: ApaCitationForm,
}

impl Default for Apa {
    fn default() -> Self {
        Self {
            sentence_case: SentenceCase::default(),
            citation_form: ApaCitationForm::Parenthetical,
        }
    }
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
            Some(format_range("Episode", "Episodes", vols))
        } else {
            Some(format_range("Vol.", "Vols.", vols))
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct AuthorList(Vec<Person>);

impl From<Vec<Person>> for AuthorList {
    fn from(authors: Vec<Person>) -> Self {
        Self(authors)
    }
}

impl IntoIterator for AuthorList {
    type Item = Person;
    type IntoIter = std::vec::IntoIter<Person>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Apa {
    /// Creates a new bibliography generator.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new citation generator with a specific citation form.
    pub fn new_citation(citation_form: ApaCitationForm) -> Self {
        Self {
            sentence_case: SentenceCase::default(),
            citation_form,
        }
    }

    fn get_author(&self, entry: &Entry) -> (String, AuthorList) {
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
            let list = name_list(authors);
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
                    ampersand_list(name_list(eds)),
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
                .flat_map(|(v, _)| v)
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

        (al, pers_refs.into())
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

    fn italicise_title(entry: &Entry) -> bool {
        if let Some(titled) = select!(* > ("p":(*["title"]))).bound(entry, "p") {
            let talk = select!(* > Conference);
            let preprint = select!((Article | Book | Anthos) > Repository);
            let booklike = select!(Book | Proceedings | Anthology);

            talk.matches(entry)
                || preprint.matches(entry)
                || (titled.kind() == entry.kind() && booklike.matches(entry))
        } else {
            true
        }
    }

    fn get_title(&self, entry: &Entry, wrap: bool) -> DisplayString {
        let italicise = Apa::italicise_title(entry);

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
                    format_range("Vol.", "Vols.", vols),
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
                let amp_list = ampersand_list(name_list_straight(editors));
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
                .flat_map(|(v, _)| v)
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
                        res += &format_range("", "", volume);
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
                        res += &format_range("", "", pages);
                    }
                }
            }
            SourceType::CollectionItem(parent) => {
                let mut comma = if let Some(eds) = parent.editors() {
                    let names = name_list(eds);
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

                    res += &format_range("", "", pps);
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
            al.0,
        )
    }

    fn citation_web_creator(&self, entry: &Entry) -> Option<String> {
        let web_thing = select!(Web | ((Misc | Web) > ("p": Web))).apply(entry);
        web_thing.map(|wt| {
            if let Some(org) = entry.organization() {
                org.into()
            } else if wt.get("p").and_then(|e| e.authors()).is_some() {
                let authors = self
                    .get_author(wt.get("p").unwrap())
                    .1
                    .into_iter()
                    .map(|p| p.given_first(false));

                let count = if authors.len() >= 3 { 1 } else { authors.len() };

                self.citation_and_list(authors, count)
            } else if let Some(org) = wt.get("p").and_then(|e| e.organization()) {
                org.into()
            } else {
                "".into()
            }
        })
    }

    fn citation_and_list(
        &self,
        names: impl IntoIterator<Item = String>,
        count: usize,
    ) -> String {
        let names = names.into_iter().collect::<Vec<_>>();
        let name_len = names.len();
        let mut res = String::new();

        let and = match self.citation_form {
            ApaCitationForm::Parenthetical => " & ",
            ApaCitationForm::Narrative => " and ",
        };

        for (index, name) in names.iter().enumerate() {
            if index >= count {
                break;
            }

            res += name;

            if index + 1 < name_len && index + 1 < count {
                if name_len > 2 {
                    res += ", ";
                } else {
                    res += and;
                }
            }
        }

        if name_len > count {
            if count > 1 {
                res += ",";
            }
            if count + 1 == name_len {
                res += and;
                res += &names[count];
            } else {
                res += " et al."
            }
        }

        res
    }

    fn citation_title(&self, entry: &Entry, title: &Title) -> DisplayString {
        let italicise = Apa::italicise_title(entry);
        let mut res = DisplayString::new();

        if italicise {
            res.start_format(Formatting::Italic);
        } else {
            res.push('“');
        }

        res += &super::chicago::shorthand(title).value;

        if self.citation_form == ApaCitationForm::Parenthetical {
            res.push(',');
        }

        if !italicise {
            res.push('”');
        }

        res
    }
}

impl<'a> CitationStyle<'a> for Apa {
    fn citation(
        &mut self,
        db: &mut Database<'a>,
        parts: &[Citation<'a>],
    ) -> DisplayCitation {
        let mut author_lists = Vec::<(_, Vec<_>)>::new();
        for atomic in parts {
            let entry = delegate_titled_entry(atomic.entry);
            let authors = self.get_author(entry).1;

            if !authors.0.is_empty() {
                if let Some((_, entries)) =
                    author_lists.iter_mut().find(|(a, _)| a == &authors)
                {
                    entries.push((atomic, entry));
                    continue;
                }
            }

            author_lists.push((authors, vec![(atomic, entry)]));
        }

        let mut items: Vec<DisplayString> = vec![];
        for (AuthorList(authors), entries) in author_lists {
            let mut last_full = false;
            let mut separator_included = false;
            let mut s = if !authors.is_empty() {
                let names = authors
                    .iter()
                    .map(|author| {
                        let uniqueness = db.uniqueness(author);
                        last_full = uniqueness == AuthorUniqueness::None;
                        match uniqueness {
                            AuthorUniqueness::Full => {
                                let mut res = String::new();
                                if let Some(prefix) = &author.prefix {
                                    res += prefix;
                                    res.push(' ');
                                }
                                res += &author.name;
                                res
                            }
                            AuthorUniqueness::Initials => author.given_first(true),
                            AuthorUniqueness::None => author.name_first(false, true),
                        }
                    })
                    .collect::<Vec<_>>();

                let count = if names.len() >= 3 {
                    // 0: First author is different
                    let mut distinction_authors = 0;
                    for entry in db.records().map(|r| r.entry) {
                        if entries.iter().any(|(_, e)| e == &entry) {
                            continue;
                        }
                        let other_authors = self.get_author(entry).1;
                        let len = other_authors.0.len();
                        let mut mismatch = len;

                        if let Some(i) = authors
                            .iter()
                            .zip(other_authors.into_iter())
                            .position(|(a, b)| a != &b)
                        {
                            mismatch = i;
                        }

                        if mismatch == len {
                            continue;
                        }

                        if mismatch > distinction_authors {
                            distinction_authors = mismatch;
                        }
                    }

                    distinction_authors + 1
                } else {
                    names.len()
                };

                self.citation_and_list(names, count).into()
            } else {
                let entry = entries[0].1;

                if let Some(title) = entry.title().map(|t| self.citation_title(entry, t))
                {
                    separator_included = true;
                    title
                } else if let Some(creator) = self.citation_web_creator(entry) {
                    creator.into()
                } else if matches!(
                    entry.entry_type,
                    Report | Patent | Legislation | Conference | Exhibition
                ) {
                    entry.organization().unwrap_or_default().into()
                } else if let Some(np) = select!(* > ("p":Newspaper)).bound(entry, "p") {
                    separator_included = true;
                    np.title().map(|t| self.citation_title(np, t)).unwrap_or_default()
                } else {
                    DisplayString::new()
                }
            };

            match self.citation_form {
                ApaCitationForm::Parenthetical => {
                    if !s.is_empty() {
                        if last_full {
                            if s.last() != Some('.') {
                                s.push('.');
                            }
                        } else if !separator_included && s.last() != Some(',') {
                            s.push(',')
                        }
                        s.push(' ');
                    }
                }
                ApaCitationForm::Narrative => {
                    if !s.is_empty() {
                        s.push(' ');
                    }
                    s.push('(');
                }
            }

            for (i, (atomic, entry)) in entries.iter().enumerate() {
                let date = entry.date_any();
                let similars = db
                    .records()
                    .filter(|&r| {
                        r.entry.date_any().map(|d| d.year) == date.map(|d| d.year)
                            && self.get_author(r.entry).1 .0 == authors
                            && !authors.is_empty()
                    })
                    .collect::<Vec<_>>();

                let colon = if let Some(date) = date {
                    s += &date.display_year();
                    false
                } else {
                    s += "n.d.";
                    true
                };

                if similars.len() > 1 {
                    let pos = similars.iter().position(|&x| x.entry == *entry).unwrap();
                    let num = if let Some(disambiguation) = similars[pos].disambiguation {
                        disambiguation
                    } else {
                        db.records
                            .iter_mut()
                            .find(|(_, r)| r.entry == *entry)
                            .unwrap()
                            .1
                            .disambiguation = Some(pos);
                        pos
                    };

                    let designator = alph_designator(num);

                    if colon {
                        s.push('-');
                    }

                    s.push(designator);
                }

                if let Some(supplement) = atomic.supplement {
                    if !supplement.ends_with(';') {
                        s += ", ";
                    }

                    s += supplement;
                }

                if i + 1 < entries.len() {
                    s += ", ";
                }
            }

            if self.citation_form == ApaCitationForm::Narrative {
                s.push(')');
            }

            items.push(s);
        }

        let joiner = match self.citation_form {
            ApaCitationForm::Parenthetical => "; ",
            ApaCitationForm::Narrative => ", ",
        };

        DisplayCitation::new(DisplayString::join(&items, joiner), false)
    }

    fn brackets(&self) -> Brackets {
        Brackets::Round
    }

    fn wrapped(&self) -> bool {
        match self.citation_form {
            ApaCitationForm::Parenthetical => true,
            ApaCitationForm::Narrative => false,
        }
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

    use super::{Apa, ApaCitationForm};
    use crate::style::{Citation, Database};
    use crate::types::{Date, EntryType, Person, QualifiedUrl, Title};
    use crate::Entry;

    fn date_author_entry(key: &str, authors: Vec<Person>, year: i32) -> Entry {
        let mut e = Entry::new(key, EntryType::Article);
        e.set_authors(authors);
        e.set_date(Date::from_year(year));
        e
    }

    #[allow(non_snake_case)]
    fn A(given: &str, family: &str) -> Person {
        Person::from_strings(&[family, given]).unwrap()
    }

    #[allow(non_snake_case)]
    fn C(entry: &Entry) -> Citation {
        Citation::new(entry, None)
    }

    #[allow(non_snake_case)]
    fn Cs(entries: &[Entry]) -> (Vec<Citation>, Database) {
        let cv = entries.iter().map(C).collect();

        let mut db = Database::new();

        for entry in entries {
            db.push(entry);
        }

        (cv, db)
    }

    #[test]
    fn simple() {
        let es = vec![date_author_entry("key", vec![A("Martin", "Haug")], 2018)];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(database.citation(&mut apa, &citations).display.value, "Haug, 2018");
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Haug (2018)"
        );
    }

    #[test]
    fn same_author_year() {
        let es = vec![
            date_author_entry("klaus1", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("klaus2", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("unklaus", vec![A("Haus", "Hinsky")], 2018),
            date_author_entry("klaus3", vec![A("Klaus", "Kinsky")], 2018),
            date_author_entry("klaus4", vec![A("Klaus", "Kinsky")], 2019),
            date_author_entry("klaus5", vec![A("Klaus", "Kinsky")], 2019),
        ];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "Kinsky, 2018a, 2018b, 2018c, 2019a, 2019b; Hinsky, 2018"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Kinsky (2018a, 2018b, 2018c, 2019a, 2019b), Hinsky (2018)"
        );
    }

    #[test]
    fn author_initials() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Rich", "Doe")], 2011),
        ];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "J. Doe, 1967; R. Doe, 2011"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "J. Doe (1967), R. Doe (2011)"
        );
    }

    #[test]
    fn author_gn() {
        let es = vec![
            date_author_entry("1", vec![A("John", "Doe")], 1967),
            date_author_entry("2", vec![A("Janet", "Doe")], 2011),
        ];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "Doe, John. 1967; Doe, Janet. 2011"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Doe, John (1967), Doe, Janet (2011)"
        );
    }

    #[test]
    fn multi_author() {
        let es = vec![
            date_author_entry(
                "key",
                vec![A("Laurenz", "Mädje"), A("Martin", "Haug")],
                2020,
            ),
            date_author_entry(
                "key2",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Madeleine", "Béjart"),
                    A("Charles", "du Fresne"),
                ],
                1648,
            ),
        ];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "Mädje & Haug, 2020; Poquelin et al., 1648"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Mädje and Haug (2020), Poquelin et al. (1648)"
        );
    }

    #[test]
    fn differentiate_et_al() {
        let es = vec![
            date_author_entry(
                "key",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Madeleine", "Béjart"),
                    A("Charles", "du Fresne"),
                    A("Bernard", "de Nogaret"),
                ],
                1648,
            ),
            date_author_entry(
                "2",
                vec![
                    A("Jean-Baptiste", "Poquelin"),
                    A("Armande", "Béjart"),
                    A("Charles", "du Fresne"),
                ],
                1662,
            ),
        ];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "Poquelin, M. Béjart, et al., 1648; Poquelin, A. Béjart, & du Fresne, 1662"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Poquelin, M. Béjart, et al. (1648), Poquelin, A. Béjart, and du Fresne (1662)"
        );
    }

    #[test]
    fn no_author_report() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_date(Date::from_year(1999));
        e.set_title(Title::new("Third International Report on Reporting"));
        let es = vec![e];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "Third International Report on Reporting, 1999"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Third International Report on Reporting (1999)"
        );
    }

    #[test]
    fn no_author_article() {
        let mut e = Entry::new("article", EntryType::Article);
        e.set_date(Date::from_year(1999));
        e.set_title(Title::new("Article on Articles"));
        let mut p = Entry::new("article", EntryType::Periodical);
        p.set_title(Title::new("International Magazine on Magazines"));
        e.add_parent(p);
        let es = vec![e];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(
            database.citation(&mut apa, &citations).display.value,
            "“Article on Articles,” 1999"
        );
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "“Article on Articles” (1999)"
        );
    }

    #[test]
    fn no_date() {
        let mut e = Entry::new("report", EntryType::Report);
        e.set_authors(vec![A("John", "Doe")]);
        let es = vec![e];
        let mut apa = Apa::new();
        let mut apa_narrative = Apa::new_citation(ApaCitationForm::Narrative);
        let (citations, mut database) = Cs(&es);
        assert_eq!(database.citation(&mut apa, &citations).display.value, "Doe, n.d.");
        assert_eq!(
            database.citation(&mut apa_narrative, &citations).display.value,
            "Doe (n.d.)"
        );
    }

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
