use en::get_ordinal;
use unicode_segmentation::UnicodeSegmentation;

use super::{
    abbreviate_publisher, alph_designator, format_range, offset_format_range,
    sorted_bibliography, BibliographyOrdering, BibliographyStyle, Database,
    DisplayReference, DisplayString, Formatting, Record,
};
use crate::lang::{en, TitleCase};
use crate::types::{Date, EntryType::*, FmtOptionExt, NumOrStr, Person, PersonRole};
use crate::Entry;

/// Bibliographies following MLA guidance.
///
/// # Examples
/// - Habermas, Jürgen. _Reason and the Rationalization of Society._ Translated
///   by Thomas McCarthy, Reprint ed., vol. 1, Beacon P, 1985. _The Theory of
///   Communicative Action._
/// - Moore, Edward F. “Gedanken-Experiments on Sequential Machines.” _Automata
///   Studies,_ edited by Shannon, C. E., and McCarthy, J., vol. 34, NBS, Apr. 1956. Annals of Mathematics Studies.
/// - “Authoritative.” _Cambridge Dictionary,_
///   <https://dictionary.cambridge.org/dictionary/english/authoritative>.
///   Accessed 29 Nov. 2020.
///
/// # Reference
/// See the 8th edition of the MLA Handbook for details on how the Modern
/// Language Association advises you to format citations and bibliographies
/// (_Works Cited_ lists).
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Mla {
    /// Forces location element to appear whenever given.
    /// Otherwise, location will only appear for physical items.
    pub always_use_location: bool,
    /// Forces all dates to be printed if true. Otherwise,
    /// only the most top-level date field will be printed.
    pub always_print_date: bool,
    /// Title case configuration.
    pub title_case: TitleCase,
}

impl Default for Mla {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ContainerInfo {
    title: DisplayString,
    contributors: String,
    version: String,
    number: String,
    publisher: String,
    date: String,
    location: DisplayString,
    optionals: String,
}

impl ContainerInfo {
    fn new() -> Self {
        Self {
            title: DisplayString::new(),
            contributors: String::new(),
            version: String::new(),
            number: String::new(),
            publisher: String::new(),
            date: String::new(),
            location: DisplayString::new(),
            optionals: String::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.title.is_empty()
            && self.contributors.is_empty()
            && self.version.is_empty()
            && self.number.is_empty()
            && self.publisher.is_empty()
            && self.date.is_empty()
            && self.location.is_empty()
            && self.optionals.is_empty()
    }

    fn into_display_string(self) -> DisplayString {
        let mut res = self.title;
        if !res.is_empty() && !self.contributors.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += &self.contributors;
        if !res.is_empty() && !self.version.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += &self.version;
        if !res.is_empty() && !self.number.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += &self.number;
        if !res.is_empty() && !self.publisher.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += &self.publisher;
        if !res.is_empty() && !self.date.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += &self.date;
        if !res.is_empty() && !self.location.is_empty() {
            if res.last() != Some(',') {
                res.push(',');
            }
            res.push(' ');
        }
        res += self.location;
        if !res.is_empty() {
            if res.last() != Some('.') {
                res.push('.');
            }
            if !self.optionals.is_empty() {
                res.push(' ');
            }
        }
        res += &self.optionals;
        if !res.is_empty() && res.last() != Some('.') {
            res.push('.');
        }

        if res.len() < 4
            || !res.value.is_char_boundary(4)
            || !(&res.value[..4] == "http" || &res.value[..4] == "doi:")
        {
            if let Some(gc) = res.value.graphemes(true).next() {
                let len = gc.len();
                let new = gc.to_uppercase();
                let diff = new.len() - len;
                res.value = new + &res.value[len..];
                res.formatting = res
                    .formatting
                    .into_iter()
                    .map(|f| offset_format_range(f, diff))
                    .collect();
            }
        }

        res
    }
}

fn format_date(d: &Date) -> String {
    let mut res = String::new();
    if let Some(month) = d.month {
        res.push_str(&if let Some(day) = d.day {
            format!("{} {} ", day + 1, en::get_month_abbr(month, true).unwrap())
        } else {
            format!("{} ", en::get_month_abbr(month, true).unwrap())
        });
    }

    res += &d.display_year();
    res
}

fn is_religious(s: &str) -> bool {
    let reference = [
        "Bible",
        "Genesis",
        "Gospels",
        "Koran",
        "New Testament",
        "Old Testament",
        "Qur'an",
        "Quran",
        "Talmud",
        "The Bible",
        "Upanishads",
    ];
    reference.binary_search(&s).is_ok()
}

impl Mla {
    /// Create a new MLA Bibliography Generator with default values.
    pub fn new() -> Self {
        let mut title_case = TitleCase::new();
        title_case.always_capitalize_last_word = false;

        Self {
            title_case,
            always_use_location: false,
            always_print_date: false,
        }
    }

    /// Indicates whether a kind of work is its own container.
    fn own_container(entry: &Entry) -> bool {
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
            || select!((*["editor"]) | (*["publisher"]) | (* > (!*))).matches(entry)
    }

    fn and_list(&self, names: Vec<String>, et_al: bool) -> String {
        let name_len = names.len();
        let mut res = String::new();
        let threshold = 3;

        for (index, name) in names.into_iter().enumerate() {
            res += &name;

            if (index as i32) <= name_len as i32 - 2 {
                res += ", ";
            }
            if name_len >= threshold && et_al {
                break;
            }
            if (index as i32) == name_len as i32 - 2 {
                res += "and ";
            }
        }

        if name_len >= threshold && et_al {
            res += "et al."
        }

        res
    }

    fn get_main_contributors(&self, entry: &Entry) -> Option<Vec<Person>> {
        entry
            .authors()
            .map(|a| a.to_vec())
            .or_else(|| {
                entry.affiliated_persons().and_then(|a| {
                    if a.len() == 1 {
                        Some(a[0].0.clone())
                    } else {
                        None
                    }
                })
            })
            .or_else(|| entry.editors().map(|a| a.to_vec()))
    }

    fn name_list(&self, persons: &[Person], tweet_entry: Option<&Entry>) -> Vec<String> {
        let mut names = vec![];

        for (i, author) in persons.iter().enumerate() {
            let alias = tweet_entry
                .and_then(|entry| entry.twitter_handle(i))
                .or_else(|| author.alias.clone());

            names.push(if let Some(alias) = alias {
                format!("{} ({})", alias, author.given_first(false))
            } else {
                author.name_first(false, true)
            });
        }

        names
    }

    /// Prints the names of the main creators of the item.
    fn get_author(
        &self,
        mut entry: &Entry,
        prev_entry: Option<&Entry>,
    ) -> (String, Vec<Person>) {
        while entry.authors().is_none()
            && entry.affiliated_persons().is_none()
            && entry.editors().is_none()
            && select!(Chapter | Scene).matches(entry)
        {
            if let Some(p) = entry.parents().and_then(|ps| ps.first()) {
                entry = p;
            } else {
                break;
            }
        }

        let main_contribs = self.get_main_contributors(entry);
        let mut contribs = vec![];
        let (mut res, previous) = if main_contribs.is_some()
            && Some(main_contribs) == prev_entry.map(|s| self.get_main_contributors(s))
        {
            ("---".to_string(), true)
        } else {
            (String::new(), false)
        };
        res += &if let Some(authors) = entry.authors() {
            contribs.extend(authors.iter().cloned());
            if !previous && entry.entry_type == Tweet {
                self.and_list(self.name_list(authors, Some(entry)), true)
            } else if !previous {
                self.and_list(self.name_list(authors, None), true)
            } else {
                String::new()
            }
        } else if let Some(affs) = entry.affiliated_persons() {
            let mut res = String::new();
            for (persons, role) in affs.iter() {
                let plural = persons.len() > 1;
                let desc = match role {
                    PersonRole::Translator if plural => "translators",
                    PersonRole::Translator => "translator",
                    PersonRole::Annotator if plural => "annotators",
                    PersonRole::Annotator => "annotator",
                    PersonRole::Commentator if plural => "commentators",
                    PersonRole::Commentator => "commentator",
                    PersonRole::Holder if plural => "holders",
                    PersonRole::Holder => "holder",
                    PersonRole::Compiler if plural => "compilers",
                    PersonRole::Compiler => "compiler",
                    PersonRole::Founder if plural => "founders",
                    PersonRole::Founder => "founder",
                    PersonRole::Collaborator if plural => "collaborators",
                    PersonRole::Collaborator => "collaborator",
                    PersonRole::Organizer if plural => "organizers",
                    PersonRole::Organizer => "organizer",
                    PersonRole::CastMember if plural => "performers",
                    PersonRole::CastMember => "performer",
                    PersonRole::Composer if plural => "composers",
                    PersonRole::Composer => "composer",
                    PersonRole::Producer if plural => "poducers",
                    PersonRole::Producer => "poducer",
                    PersonRole::ExecutiveProducer if plural => "executive producers",
                    PersonRole::ExecutiveProducer => "executive producer",
                    PersonRole::Writer if plural => "writers",
                    PersonRole::Writer => "writer",
                    PersonRole::Cinematography if plural => "cinematographers",
                    PersonRole::Cinematography => "cinematographer",
                    PersonRole::Director if plural => "directors",
                    PersonRole::Director => "director",
                    PersonRole::Illustrator if plural => "illustrators",
                    PersonRole::Illustrator => "illustrator",
                    _ => "",
                };

                if desc.is_empty() || persons.is_empty() {
                    continue;
                }
                contribs.extend(persons.iter().cloned());

                if !res.is_empty() {
                    res += ", ";
                }

                if !previous {
                    res += &self.and_list(self.name_list(persons, None), true);
                }
                res += ", ";
                res += desc;
            }
            res
        } else if let Some(eds) = entry.editors() {
            let plural = eds.len() > 1;
            let mut res = if !previous {
                self.and_list(self.name_list(eds, None), true)
            } else {
                String::new()
            };
            contribs.extend(eds.iter().cloned());

            res += ", editor";
            if plural {
                res.push('s');
            }

            res
        } else {
            String::new()
        };

        if !res.is_empty() && !res.ends_with('.') {
            res.push('.');
        }
        (res, contribs)
    }

    fn get_title(&self, mut entry: &Entry, use_quotes: bool) -> DisplayString {
        let sc = !use_quotes || Mla::own_container(entry);
        let mut res = DisplayString::new();

        if use_quotes {
            if let Some(mut bindings) = select!(Chapter > ("a":*)).apply(entry) {
                let temp = bindings.remove("a").unwrap();

                if ["preface", "introduction", "foreword", "afterword"].iter().any(|&x| {
                    Some(x.into())
                        == entry.title().map(|x| x.canonical.value.to_lowercase())
                }) {
                    res += &entry
                        .title()
                        .unwrap()
                        .canonical
                        .format_title_case(&self.title_case);

                    res += ". ";
                    entry = temp;
                }
            }
        }

        if let Some(title) = entry.title() {
            let fmt = title.canonical.format_title_case(&self.title_case);
            if sc
                && !select!(Legislation | Conference).matches(entry)
                && !is_religious(&fmt)
            {
                res.start_format(Formatting::Italic)
            } else if !sc {
                res += "“"
            }
            res += &fmt;
            if !res.is_empty() && res.last() != Some('.') && use_quotes {
                res.push('.');
            }
            res.commit_formats();
            if !sc {
                res += "”";
            }
        } else if let Some(note) = entry.note() {
            res += note;
            if !res.is_empty() && res.last() != Some('.') && use_quotes {
                res.push('.');
            }
        }

        res
    }

    fn get_parent_container_infos(
        &self,
        entry: &Entry,
        root: &Entry,
        disambiguation: Option<usize>,
        mut has_date: bool,
        mut has_url: bool,
    ) -> (Vec<ContainerInfo>, bool) {
        let mut containers: Vec<ContainerInfo> = vec![];
        let series: Option<&Entry> =
            select!(Anthology > ("p":(Anthology["title"]))).bound(entry, "p");

        if entry != root || Mla::own_container(entry) {
            let mut container = ContainerInfo::new();

            // Title
            if entry != root {
                container.title = self.get_title(entry, false);
            }

            // Other contributors.
            let mut contributors = vec![];
            if let Some(affiliated) = entry.affiliated_persons() {
                if entry != root || entry.authors().is_some() {
                    for (ps, r) in affiliated.iter() {
                        if ps.is_empty() {
                            continue;
                        }

                        let prefix = match r {
                            PersonRole::Translator => "translated by",
                            PersonRole::Afterword => "afterword by",
                            PersonRole::Foreword => "foreword by",
                            PersonRole::Introduction => "introduction by",
                            PersonRole::Annotator => "annotated by",
                            PersonRole::Commentator => "commented by",
                            PersonRole::Holder => "held by",
                            PersonRole::Compiler => "compiled by",
                            PersonRole::Founder => "founded by",
                            PersonRole::Collaborator => "supported by",
                            PersonRole::Organizer => "organized by",
                            PersonRole::CastMember => "performance by",
                            PersonRole::Composer => "composed by",
                            PersonRole::Producer | PersonRole::ExecutiveProducer => {
                                "produced by"
                            }
                            PersonRole::Writer => "written by",
                            PersonRole::Cinematography => "shot by",
                            PersonRole::Director => "directed by",
                            PersonRole::Illustrator => "illustrated by",
                            PersonRole::Narrator => "narrated by",
                            PersonRole::Unknown(_) => "",
                        };

                        let mut res = prefix.to_string();
                        if let PersonRole::Unknown(r) = r {
                            res = format!("{},", r);
                        }

                        res.push(' ');

                        let mut names = vec![];

                        for author in ps.iter() {
                            names.push(author.given_first(false));
                        }

                        res += &self.and_list(names, false);
                        contributors.push(res);
                    }
                }
            }

            if let Some(eds) = entry.editors() {
                if !eds.is_empty()
                    && (entry != root
                        || entry.authors().is_some()
                        || entry.affiliated_persons().is_some())
                {
                    let mut res = "edited by ".to_string();
                    res += &self.and_list(self.name_list(eds, None), true);
                    contributors.push(res);
                }
            }

            if !contributors.is_empty() {
                container.contributors = contributors.join(", ");
            }

            // Version
            if let Some(edition) = entry.edition() {
                match edition {
                    NumOrStr::Str(i) => {
                        container.version = i
                            .replace("revised", "rev.")
                            .replace("edition", "ed.")
                            .replace("Edition", "ed.")
                    }
                    NumOrStr::Number(i) => {
                        container.version = format!("{} ed.", get_ordinal(*i))
                    }
                }
            } else if let Some(serial_number) = entry.serial_number() {
                container.version = serial_number.to_string();
            }

            // Number
            let mut number = String::new();
            let tv = select!(Video > *).matches(entry);
            if let Some(vols) = entry.volume() {
                number += &if tv {
                    format_range("season", "seasons", vols)
                } else {
                    format_range("vol.", "vols.", vols)
                }
            }

            if let Some(issue) = entry.issue() {
                let res = match issue {
                    NumOrStr::Str(i) => i.clone(),
                    NumOrStr::Number(i) if tv => format!("episode {}", i),
                    NumOrStr::Number(i) => format!("no. {}", i),
                };

                if !number.is_empty() && !res.is_empty() {
                    number += ", ";
                }
                number += &res;
            }
            container.number = number;

            // Publisher
            if !select!((Manuscript > (!*)) | Periodical).matches(entry) {
                if let Some(publisher) =
                    entry.publisher().value().or_else(|| entry.organization())
                {
                    container.publisher = abbreviate_publisher(publisher, true);
                }
            }

            // Date
            if let Some(date) = entry.date() {
                if !has_date || self.always_print_date {
                    has_date = true;
                    container.date = format_date(date);
                    if let Some(disambiguation) = disambiguation {
                        container.date.push(alph_designator(disambiguation))
                    }
                }
            }

            // Location
            let mut location: Vec<DisplayString> = vec![];
            let physical =
                select!(Scene | Artwork | Case | Conference | Exhibition).matches(entry);
            if physical || self.always_use_location || entry.publisher().is_none() {
                if let Some(loc) = entry.location() {
                    location.push(DisplayString::from_string(loc.value.clone()));
                }
            }
            if let Some(page_range) = entry.page_range() {
                location.push(format_range("p.", "pp.", page_range).into());
            }

            if entry.publisher().is_some() && entry.organization().is_some() {
                location.push(entry.organization().unwrap().into());
            }

            if entry.edition().is_some() && entry.serial_number().is_some() {
                location.push(entry.serial_number().unwrap().into());
            }

            if let Some(archive) = entry.archive() {
                if let Some(aloc) = entry.archive_location() {
                    location.push(aloc.value.clone().into());
                }

                location.push(archive.value.clone().into());
            }

            let mut supplemental = vec![];

            // Location: May also produce a supplemental item.
            if let Some(doi) = entry.doi() {
                let mut dstr = DisplayString::new();
                dstr.start_format(Formatting::Link(format!("https://doi.org/{}", doi)));
                dstr += &format!("doi:{}", doi);
                dstr.commit_formats();
                location.push(dstr);
                has_url = true;
            } else if let Some(qurl) = entry.url() {
                let vdate = qurl.visit_date.is_some()
                    && select!(Blog | Web | Misc | (!(*["date"])) | (* > (Blog | Web | Misc)))
                    .matches(entry);

                if vdate {
                    supplemental.push(format!(
                        "Accessed {}",
                        format_date(qurl.visit_date.as_ref().unwrap())
                    ));
                }
                let mut dstr = DisplayString::new();
                dstr.start_format(Formatting::Link(qurl.value.to_string()));
                dstr += qurl.value.as_str();
                dstr.commit_formats();

                location.push(dstr);
                has_url = true;
            }

            if !location.is_empty() {
                container.location = DisplayString::join(&location, ", ");
            }

            // Supplemental
            if let Some(&tvol) = entry.volume_total() {
                if tvol > 1 {
                    supplemental.push(format!("{} vols", tvol));
                }
            }

            if let Some(series) = series {
                supplemental.push(
                    series.title().unwrap().canonical.format_title_case(&self.title_case),
                );
            }

            if !supplemental.is_empty() {
                container.optionals = supplemental.join(". ");
            }

            if !container.is_empty() {
                containers.push(container);
            }
        }

        for p in &entry.parents().map(|r| r.to_vec()).unwrap_or_default() {
            if Some(p) == series {
                continue;
            }

            let parents =
                self.get_parent_container_infos(p, root, None, has_date, has_url);
            has_url = has_url || parents.1;
            containers.extend(parents.0.into_iter());
        }

        if entry == root && !has_url {
            if let Some(lc) = containers.last_mut() {
                if let Some(doi) = entry.doi() {
                    if !lc.location.is_empty() {
                        lc.location += ", ";
                    }
                    lc.location += &format!("doi:{}", doi);
                } else if let Some(qurl) = entry.url_any() {
                    let vdate = qurl.visit_date.is_some()
                        && select!(
                            Blog | Web | Misc |
                            (!(*["date"])) |
                            (* > (Blog | Web | Misc))
                        )
                        .matches(entry);
                    if vdate {
                        if !lc.optionals.is_empty() {
                            lc.optionals += ". ";
                        }

                        lc.optionals += &format!(
                            "Accessed {}",
                            format_date(qurl.visit_date.as_ref().unwrap())
                        );
                    }
                    if !lc.location.is_empty() {
                        lc.location += ", ";
                    }
                    lc.location.start_format(Formatting::Link(qurl.value.to_string()));
                    lc.location += qurl.value.as_str();
                    lc.location.commit_formats();
                    has_url = true;
                }
            } else if let Some(doi) = entry.doi() {
                let mut nc = ContainerInfo::new();
                nc.location
                    .start_format(Formatting::Link(format!("https://doi.org/{}", doi)));
                nc.location += &format!("doi:{}", doi);
                nc.location.commit_formats();
                containers.push(nc);
            } else if let Some(qurl) = entry.url_any() {
                let mut nc = ContainerInfo::new();
                nc.location.start_format(Formatting::Link(qurl.value.to_string()));
                nc.location += qurl.value.as_str();
                nc.location.commit_formats();
                let vdate = qurl.visit_date.is_some() && select!(Blog | Web | Misc | (!(*["date"])) | (* > (Blog | Web | Misc))).matches(entry);
                if vdate {
                    nc.optionals = format!(
                        "Accessed {}",
                        format_date(qurl.visit_date.as_ref().unwrap())
                    );
                }
                containers.push(nc);
            }
        }

        (containers, has_url)
    }

    fn get_container_info(
        &self,
        entry: &Entry,
        disambiguation: Option<usize>,
    ) -> DisplayString {
        let ds = self
            .get_parent_container_infos(entry, entry, disambiguation, false, false)
            .0
            .into_iter()
            .map(|p| p.into_display_string());
        let mut res = DisplayString::new();
        for (i, d) in ds.enumerate() {
            if i != 0 {
                res.push(' ');
            }

            res += d;
        }
        res
    }

    fn get_single_record<'a>(
        &self,
        record: &Record<'a>,
        last_record: Option<&Record<'a>>,
    ) -> (DisplayReference<'a>, Vec<Person>) {
        let entry = record.entry;
        let (authors, auth_list) = self.get_author(entry, last_record.map(|r| r.entry));

        let mut res = DisplayString::from_string(authors);
        let title = self.get_title(entry, true);

        if !res.is_empty() && !title.is_empty() {
            res.push(' ');
        }
        res += title;
        let container_info = self.get_container_info(entry, record.disambiguation);

        if !res.is_empty() && !container_info.is_empty() {
            res.push(' ');
        }
        res += container_info;

        (
            DisplayReference::new(
                record.entry,
                record.prefix.clone().map(Into::into),
                res,
            ),
            auth_list,
        )
    }
}

impl<'a> BibliographyStyle<'a> for Mla {
    fn bibliography(
        &self,
        db: &Database<'a>,
        ordering: BibliographyOrdering,
    ) -> Vec<DisplayReference<'a>> {
        let mut items = vec![];

        for i in 0..db.records.len() {
            let record = db.records().nth(i).unwrap();
            let last_record = if let Some(prev) = i.checked_sub(1) {
                db.records().nth(prev)
            } else {
                None
            };
            items.push(self.get_single_record(record, last_record))
        }

        sorted_bibliography(items, ordering)
    }

    fn reference(&self, record: &Record<'a>) -> DisplayReference<'a> {
        self.get_single_record(record, None).0
    }

    fn ordering(&self) -> BibliographyOrdering {
        BibliographyOrdering::ByAuthor
    }
}
