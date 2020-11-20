//! Style for entries in the "Works Cited" listing as of the 8th edition
//! of the MLA Handbook.

use super::{format_range, BibliographyGenerator, DisplayString, FormatVariantOptions};
use crate::lang::{en, TitleCase};
use crate::selectors::{Bind, Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::types::{Date, NumOrStr, Person, PersonRole};
use crate::{attrs, sel, Entry};
use unicode_segmentation::UnicodeSegmentation;

/// Generates the "Works Cited" entries
pub struct MlaBibliographyGenerator<'s> {
    tc_formatter: TitleCase,
    prev_entry: Option<&'s Entry>,
    /// Forces location element to appear whenever given.
    /// Otherwise, location will only appear for physical items.
    pub always_use_location: bool,
    /// Forces all dates to be printed if true. Otherwise,
    /// only the most top-level date field will be printed.
    pub always_print_date: bool,
}

struct ContainerInfo {
    title: DisplayString,
    contributors: String,
    version: String,
    number: String,
    publisher: String,
    date: String,
    location: String,
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
            location: String::new(),
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
        res += &self.location;
        if !res.is_empty() {
            if res.last() != Some('.') {
                res.push('.');
            }
            if !self.optionals.is_empty() {
                res.push(' ');
            }
        }
        res += &self.optionals;
        if !res.is_empty() {
            if res.last() != Some('.') {
                res.push('.');
            }
        }

        if res.len() < 4
            || !res.value.is_char_boundary(4)
            || !(&res.value[.. 4] == "http" || &res.value[.. 4] == "doi:")
        {
            if let Some(gc) = res.value.graphemes(true).next() {
                let len = gc.len();
                let new = gc.to_uppercase();
                let diff = new.len() - len;
                res.value = new + &res.value[len ..];
                res.formatting =
                    res.formatting.into_iter().map(|f| f.offset(diff as i32)).collect();
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

    if d.year > 0 {
        res += &d.year.to_string();
    } else {
        res += &format!("{} B.C.E.", -(d.year - 1));
    }
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

fn abbreviate_publisher(s: &str) -> String {
    let s1 = s
        .replace("University Press", "UP")
        .replace("University", "U")
        .replace("Universität", "U")
        .replace("Université", "U")
        .replace("Press", "P")
        .replace("Presse", "P");
    let business_words = [
        "Co",
        "Co.",
        "Corp",
        "Corp.",
        "Corporated",
        "Corporation",
        "Inc",
        "Inc.",
        "Incorporated",
        "Limited",
        "Ltd",
        "Ltd.",
    ];
    s1.split(' ')
        .filter(|w| !w.is_empty() && business_words.binary_search(w).is_err())
        .collect::<Vec<_>>()
        .join(" ")
}

impl<'s> MlaBibliographyGenerator<'s> {
    /// Create a new MLA Bibliography Generator with default values.
    pub fn new() -> Self {
        let mut tc_formatter = TitleCase::new();
        tc_formatter.always_capitalize_last_word = false;

        Self {
            tc_formatter,
            prev_entry: None,
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
            || sel!(alt
                attrs!(Wc(), "editor"),
                attrs!(Wc(), "publisher"),
                sel!(Wc() => Neg(Wc())),
            )
            .apply(entry)
            .is_some()
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
            .get_authors_fallible()
            .map(|a| a.to_vec())
            .or_else(|| {
                entry
                    .get_affiliated_persons()
                    .ok()
                    .and_then(|a| if a.len() == 1 { Some(a[0].0.clone()) } else { None })
            })
            .or_else(|| entry.get_editors().ok().map(|a| a.to_vec()))
    }

    fn name_list(&self, persons: &[Person], tweet_entry: Option<&Entry>) -> Vec<String> {
        let mut names = vec![];

        for (i, author) in persons.iter().enumerate() {
            let alias = tweet_entry
                .and_then(|entry| entry.get_twitter_handle(i))
                .or_else(|| author.alias.clone());

            names.push(if let Some(alias) = alias {
                format!(
                    "{} ({})",
                    alias,
                    author.get_given_name_initials_first(false)
                )
            } else {
                author.get_name_first(false, true)
            });
        }

        names
    }

    /// Prints the names of the main creators of the item.
    fn get_author(&self, mut entry: &Entry) -> String {
        while entry.get_authors_fallible().is_none()
            && entry.get_affiliated_persons().is_err()
            && entry.get_editors().is_err()
            && sel!(alt Id(Chapter), Id(Scene)).apply(entry).is_some()
        {
            if let Some(p) = entry.get_parents().ok().and_then(|ps| ps.get(0)) {
                entry = &p;
            } else {
                break;
            }
        }

        let main_contribs = self.get_main_contributors(entry);
        let (mut res, previous) = if main_contribs.is_some()
            && Some(main_contribs)
                == self.prev_entry.map(|s| self.get_main_contributors(s))
        {
            ("---".to_string(), true)
        } else {
            (String::new(), false)
        };
        res += &if let Some(authors) = entry.get_authors_fallible() {
            if !previous && entry.entry_type == Tweet {
                self.and_list(self.name_list(authors, Some(entry)), true)
            } else if !previous {
                self.and_list(self.name_list(authors, None), true)
            } else {
                String::new()
            }
        } else if let Ok(affs) = entry.get_affiliated_persons() {
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
        } else if let Ok(eds) = entry.get_editors() {
            let plural = eds.len() > 1;
            let mut res = if !previous {
                self.and_list(self.name_list(eds, None), true)
            } else {
                String::new()
            };

            res += ", editor";
            if plural {
                res.push('s');
            }

            res
        } else {
            String::new()
        };

        if !res.is_empty() && res.chars().last().unwrap_or('a') != '.' {
            res.push('.');
        }
        res
    }

    fn get_title(&self, mut entry: &Entry, use_quotes: bool) -> DisplayString {
        let sc = !use_quotes || MlaBibliographyGenerator::own_container(entry);
        let mut res = DisplayString::new();

        if use_quotes {
            if let Some(mut hm) = sel!(Id(Chapter) => Bind("a", Wc())).apply(entry) {
                let temp = hm.remove("a").unwrap();

                if ["preface", "introduction", "foreword", "afterword"]
                    .iter()
                    .position(|&x| {
                        Ok(x.into()) == entry.get_title().map(|x| x.to_lowercase())
                    })
                    .is_some()
                {
                    res += &entry
                        .get_title_fmt(Some(&self.tc_formatter), None)
                        .unwrap()
                        .title_case;
                    res += ". ";
                    entry = temp;
                }
            }
        }

        if let Ok(title) = entry.get_title_fmt(Some(&self.tc_formatter), None) {
            if sc
                && sel!(alt Id(Legislation), Id(Conference)).apply(entry).is_none()
                && !is_religious(&title.title_case)
            {
                res.start_format(FormatVariantOptions::Italic)
            } else if !sc {
                res += "“"
            }
            res += &title.title_case;
            if !res.is_empty() && res.last() != Some('.') && use_quotes {
                res.push('.');
            }
            res.commit_formats();
            if !sc {
                res += "”";
            }
        } else if let Ok(note) = entry.get_note() {
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
        mut has_date: bool,
        mut has_url: bool,
    ) -> (Vec<ContainerInfo>, bool) {
        let mut containers: Vec<ContainerInfo> = vec![];
        let series: Option<&Entry> =
            sel!(Id(Anthology) => Bind("p", attrs!(Id(Anthology), "title")))
                .apply(entry)
                .map(|mut hm| hm.remove("p").unwrap());

        if entry != root || MlaBibliographyGenerator::own_container(entry) {
            let mut container = ContainerInfo::new();

            // Title
            if entry != root {
                container.title = self.get_title(entry, false);
            }

            // Other contributors.
            let mut contributors = vec![];
            if let Ok(affiliated) = entry.get_affiliated_persons() {
                if entry != root || entry.get_authors_fallible().is_some() {
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
                            names.push(author.get_given_name_initials_first(false));
                        }

                        res += &self.and_list(names, false);
                        contributors.push(res);
                    }
                }
            }

            if let Ok(eds) = entry.get_editors() {
                if !eds.is_empty()
                    && (entry != root
                        || entry.get_authors_fallible().is_some()
                        || entry.get_affiliated_persons().is_ok())
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
            if let Ok(edition) = entry.get_edition() {
                match edition {
                    NumOrStr::Str(i) => container.version = i.replace("revised", "rev."),
                    NumOrStr::Number(i) => container.version = format!("{} ed.", i),
                }
            } else if let Ok(serial_number) = entry.get_serial_number() {
                container.version = serial_number.to_string();
            }

            // Number
            let mut number = String::new();
            let tv = sel!(Id(Video) => Wc()).apply(entry).is_some();
            if let Ok(vols) = entry.get_volume() {
                number += &if tv {
                    format_range("season", "seasons", &vols)
                } else {
                    format_range("vol.", "vols.", &vols)
                }
            }

            if let Ok(issue) = entry.get_issue() {
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
            if sel!(alt sel!(Id(Manuscript) => Neg(Wc())), Id(Periodical))
                .apply(entry)
                .is_none()
            {
                if let Ok(publisher) =
                    entry.get_publisher().or_else(|_| entry.get_organization())
                {
                    container.publisher = abbreviate_publisher(publisher);
                }
            }

            // Date
            if let Ok(date) = entry.get_date() {
                if !has_date || self.always_print_date {
                    has_date = true;
                    container.date = format_date(&date);
                }
            }

            // Location
            let mut location = vec![];
            let physical = sel!(alt Id(Scene), Id(Artwork), Id(Case), Id(Conference), Id(Exhibition)).apply(entry).is_some();
            if physical || self.always_use_location || entry.get_publisher().is_err() {
                if let Ok(loc) = entry.get_location() {
                    location.push(loc.to_string());
                }
            }
            if let Ok(page_range) = entry.get_page_range() {
                location.push(format_range("p.", "pp.", page_range));
            }

            if entry.get_publisher().is_ok() && entry.get_organization().is_ok() {
                location.push(entry.get_organization().unwrap().to_string());
            }

            if entry.get_edition().is_ok() && entry.get_serial_number().is_ok() {
                location.push(entry.get_serial_number().unwrap().to_string());
            }

            if let Ok(archive) = entry.get_archive() {
                if let Ok(aloc) = entry.get_archive_location() {
                    location.push(aloc.into());
                }

                location.push(archive.into());
            }

            let mut supplemental = vec![];

            // Location: May also produce a supplemental item.
            if let Ok(doi) = entry.get_doi() {
                location.push(format!("doi:{}", doi));
                has_url = true;
            } else if let Ok(qurl) = entry.get_url() {
                let vdate = qurl.visit_date.is_some() && sel!(alt Id(Blog), Id(Web), Id(Misc), Neg(attrs!(Wc(), "date")), sel!(Wc() => sel!(alt Id(Blog), Id(Web), Id(Misc)))).apply(entry).is_some();
                if vdate {
                    supplemental.push(format!(
                        "Accessed {}",
                        format_date(qurl.visit_date.as_ref().unwrap())
                    ));
                }
                location.push(qurl.value.to_string());
                has_url = true;
            }

            if !location.is_empty() {
                container.location = location.join(", ");
            }

            // Supplemental
            if let Ok(&tvol) = entry.get_total_volumes() {
                if tvol > 1 {
                    supplemental.push(format!("{} vols", tvol));
                }
            }

            if let Some(series) = series {
                supplemental.push(
                    series
                        .get_title_fmt(Some(&self.tc_formatter), None)
                        .unwrap()
                        .title_case,
                );
            }

            if !supplemental.is_empty() {
                container.optionals = supplemental.join(". ");
            }

            if !container.is_empty() {
                containers.push(container);
            }
        }

        for p in &entry.get_parents().map(|r| r.to_vec()).unwrap_or_default() {
            if Some(p) == series {
                continue;
            }

            let parents = self.get_parent_container_infos(p, root, has_date, has_url);
            has_url = has_url || parents.1;
            containers.extend(parents.0.into_iter());
        }

        if entry == root && !has_url {
            if let Some(lc) = containers.last_mut() {
                if let Ok(doi) = entry.get_doi() {
                    if !lc.location.is_empty() {
                        lc.location += ", ";
                    }
                    lc.location += &format!("doi:{}", doi);
                } else if let Some(qurl) = entry.get_any_url() {
                    let vdate = qurl.visit_date.is_some() && sel!(alt Id(Blog), Id(Web), Id(Misc), Neg(attrs!(Wc(), "date")), sel!(Wc() => sel!(alt Id(Blog), Id(Web), Id(Misc)))).apply(entry).is_some();
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
                    lc.location += qurl.value.as_str();
                    has_url = true;
                }
            } else if let Ok(doi) = entry.get_doi() {
                let mut nc = ContainerInfo::new();
                nc.location += &format!("doi:{}", doi);
                containers.push(nc);
            } else if let Some(qurl) = entry.get_any_url() {
                let mut nc = ContainerInfo::new();
                nc.location = qurl.value.to_string();
                let vdate = qurl.visit_date.is_some() && sel!(alt Id(Blog), Id(Web), Id(Misc), Neg(attrs!(Wc(), "date")), sel!(Wc() => sel!(alt Id(Blog), Id(Web), Id(Misc)))).apply(entry).is_some();
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

    fn get_container_info(&self, entry: &Entry) -> DisplayString {
        let ds = self
            .get_parent_container_infos(entry, entry, false, false)
            .0
            .into_iter()
            .map(|p| p.into_display_string())
            .collect::<Vec<_>>();
        let mut res = DisplayString::new();
        for (i, d) in ds.into_iter().enumerate() {
            if i != 0 {
                res.push(' ');
            }

            res += d;
        }
        res
    }
}

impl<'s> BibliographyGenerator<'s> for MlaBibliographyGenerator<'s> {
    fn get_reference(&mut self, entry: &'s Entry) -> DisplayString {
        let mut res = DisplayString::from_string(self.get_author(entry));
        let title = self.get_title(entry, true);

        if !res.is_empty() && !title.is_empty() {
            res.push(' ');
        }
        res += title;
        let container_info = self.get_container_info(entry);

        if !res.is_empty() && !container_info.is_empty() {
            res.push(' ');
        }
        res += container_info;

        self.prev_entry = Some(entry);
        res
    }
}
