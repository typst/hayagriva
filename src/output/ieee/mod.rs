mod abbreviations;
use super::{
    format_range, name_list_straight, BibliographyGenerator, DisplayString,
    FormatVariantOptions,
};
use crate::lang::{en, SentenceCase, TitleCase};
use crate::types::EntryType::*;
use crate::types::{
    Date, EntryTypeModality, EntryTypeSpec, NumOrStr, Person, PersonRole,
};
use crate::Entry;
use isolang::Language;

#[derive(Clone, Debug)]
pub struct IeeeBibliographyGenerator {
    sc_formatter: SentenceCase,
    tc_formatter: TitleCase,
    et_al_threshold: Option<u32>,
}

fn get_canonical_parent(entry: &Entry) -> Option<&Entry> {
    let section_spec = EntryTypeSpec::single_parent(
        EntryTypeModality::Alternate(vec![Chapter, Scene, WebItem]),
        EntryTypeModality::Any,
    );
    let anthology_spec = EntryTypeSpec::single_parent(
        EntryTypeModality::Specific(InAnthology),
        EntryTypeModality::Specific(Anthology),
    );
    let entry_spec = EntryTypeSpec::single_parent(
        EntryTypeModality::Specific(Entry),
        EntryTypeModality::Alternate(vec![Reference, Repository]),
    );
    let proc_spec = EntryTypeSpec::single_parent(
        EntryTypeModality::Any,
        EntryTypeModality::Alternate(vec![Conference, Proceedings]),
    );
    let periodical_spec = EntryTypeSpec::single_parent(
        EntryTypeModality::Specific(Article),
        EntryTypeModality::Specific(Periodical),
    );

    entry
        .check_with_spec(anthology_spec)
        .or_else(|_| entry.check_with_spec(periodical_spec))
        .or_else(|_| entry.check_with_spec(entry_spec))
        .or_else(|_| entry.check_with_spec(proc_spec))
        .or_else(|_| entry.check_with_spec(section_spec))
        .ok()
        .map(|r| entry.get_parents().unwrap().get(r[0]).unwrap())
}

impl IeeeBibliographyGenerator {
    pub fn new() -> Self {
        let mut tc_formatter = TitleCase::default();
        tc_formatter.always_capitalize_min_len = Some(4);
        Self {
            sc_formatter: SentenceCase::default(),
            tc_formatter,
            et_al_threshold: Some(6),
        }
    }

    fn and_list(&self, names: Vec<String>) -> String {
        let name_len = names.len() as u32;
        let mut res = String::new();
        let threshold = self.et_al_threshold.unwrap_or(0);

        for (index, name) in names.into_iter().enumerate() {
            if threshold > 0 && index > 1 && name_len >= threshold {
                break;
            }

            res += &name;

            if (index as i32) <= name_len as i32 - 2 {
                res += ", ";
            }
            if (index as i32) == name_len as i32 - 2 {
                res += "and ";
            }
        }

        if threshold > 0 && name_len >= threshold {
            res += "et al."
        }

        res
    }

    fn show_url(&self, entry: &Entry) -> bool {
        entry.get_any_url().is_some()
    }

    fn get_author(&self, entry: &Entry, canonical: &Entry) -> String {
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
            let tv_series = EntryTypeSpec::single_parent(
                EntryTypeModality::Specific(Video),
                EntryTypeModality::Specific(Video),
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
                let mut dir_name_list_straight = name_list_straight(&dirs)
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
                let mut writers_name_list_straight = name_list_straight(&writers)
                    .into_iter()
                    .map(|s| format!("{} (Writer)", s))
                    .collect::<Vec<String>>();
                dir_name_list_straight.append(&mut writers_name_list_straight);

                if !dirs.is_empty() {
                    names = Some(dir_name_list_straight);
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
                    names = Some(name_list_straight(&dirs));
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
                        names = Some(name_list_straight(&prods));
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let mut authors = entry.get_authors();
        if authors.is_empty() {
            authors = canonical.get_authors();
        }
        let authors = names.unwrap_or_else(|| name_list_straight(&authors));
        let count = authors.len();
        let al = if !authors.is_empty() {
            let amps = self.and_list(authors);
            match role {
                AuthorRole::Normal => amps,
                AuthorRole::ExecutiveProducer if count == 1 => {
                    format!("{}, Executive Prod", amps)
                }
                AuthorRole::ExecutiveProducer => format!("{}, Executive Prods", amps),
                AuthorRole::Director if count == 1 => format!("{}, Director", amps),
                AuthorRole::Director => format!("{}, Directors", amps),
            }
        } else if let Ok(eds) = entry.get_editors() {
            if !eds.is_empty() {
                format!(
                    "{}, {}",
                    self.and_list(name_list_straight(&eds)),
                    if eds.len() == 1 { "Ed." } else { "Eds." }
                )
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        al
    }

    fn get_title_element(&self, entry: &Entry, canonical: &Entry) -> DisplayString {
        // Article > Periodical: "<SC>," _<abbr(TC)>_
        // Any > Conference:     <SC>. Presented at <abbr(TC)>
        // Any > Anthology:      "<SC>," in _<TC>_ (TC, no. <issue>)
        // entry != canonical:   "<SC>," in _<TC>_
        // Legislation:          _<serial number>, <TC>_
        // Repository, Video, Reference, Book, Proceedings, Anthology, : _<TC>_
        // Fallback:             "<SC>,"

        let mut res = DisplayString::new();

        if entry != canonical {
            let entry_title = entry.get_title_fmt(None, Some(&self.sc_formatter));
            let canon_title = canonical.get_title_fmt(Some(&self.tc_formatter), None);

            if let Ok(et) = entry_title {
                if canonical.entry_type == Conference {
                    res += &et.sentence_case;
                    res.push('.');
                } else {
                    res += "“";
                    res += &et.sentence_case;
                    res += ",”";
                }

                if canon_title.is_ok() {
                    res.push(' ');
                }
            }

            if let Ok(ct) = canon_title {
                let ct = abbreviations::abbreviate_journal(&ct.title_case);

                if canonical.entry_type == Conference {
                    res += "Presented at ";
                    res += &ct;
                } else {
                    if let Ok(lang) =
                        entry.get_language().or_else(|_| canonical.get_language())
                    {
                        res += "(in ";
                        res += Language::from_639_1(lang.language.as_str())
                            .unwrap()
                            .to_name();
                        res += ") ";
                    }

                    if entry.entry_type != Article || canonical.entry_type != Periodical {
                        res += "in ";
                    }
                    res.start_format(FormatVariantOptions::Italic);
                    res += &ct;
                    res.commit_formats();

                    // Do the series parentheses thing here
                    let spec = EntryTypeSpec::single_parent(
                        EntryTypeModality::Specific(Anthology),
                        EntryTypeModality::Specific(Anthology),
                    );
                    if let Ok(par_anth) = canonical.check_with_spec(spec) {
                        let par_anth =
                            canonical.get_parents().unwrap().get(par_anth[0]).unwrap();
                        if let Ok(par_t) =
                            par_anth.get_title_fmt(Some(&self.tc_formatter), None)
                        {
                            res += " (";
                            res += &par_t.title_case;

                            res.add_if_ok(
                                par_anth.get_issue().map(|v| v.to_string()),
                                Some(", no. "),
                                None,
                            );
                            res += ")";
                        }
                    }

                    // And the conference series thing as well
                    let spec = EntryTypeSpec::single_parent(
                        EntryTypeModality::Specific(Proceedings),
                        EntryTypeModality::Alternate(vec![Proceedings, Anthology, Misc]),
                    );
                    if let Ok(par_conf) = canonical.check_with_spec(spec) {
                        let par_conf =
                            canonical.get_parents().unwrap().get(par_conf[0]).unwrap();
                        if let Ok(par_t) =
                            par_conf.get_title_fmt(Some(&self.tc_formatter), None)
                        {
                            res += " in ";
                            res += &par_t.title_case;
                        }
                    }
                }
            }
        // No canonical parent
        } else if [
            Legislation,
            Repository,
            Video,
            Reference,
            Book,
            Proceedings,
            Anthology,
        ]
        .contains(&entry.entry_type)
        {
            res.start_format(FormatVariantOptions::Italic);

            if entry.entry_type == Legislation {
                res.add_if_ok(entry.get_serial_number(), None, None);
            }

            if let Ok(title) = entry.get_title_fmt(Some(&self.tc_formatter), None) {
                if !res.is_empty() {
                    res += ", ";
                }

                res += &title.title_case;
            }
            res.commit_formats();
        } else {
            if let Ok(title) = entry.get_title_fmt(None, Some(&self.sc_formatter)) {
                res += "“";
                res += &title.sentence_case;
                res += ",”";
            }
        }

        res
    }

    fn get_addons(
        &self,
        entry: &Entry,
        canonical: &Entry,
        chapter: Option<u32>,
        section: Option<u32>,
    ) -> Vec<String> {
        let mut res = vec![];
        let preprint = entry.check_with_spec(EntryTypeSpec::single_parent(
            EntryTypeModality::Alternate(vec![Article, Book, InAnthology]),
            EntryTypeModality::Specific(Repository),
        ));

        let web_parented = entry.check_with_spec(EntryTypeSpec::single_parent(
            EntryTypeModality::Any,
            EntryTypeModality::Alternate(vec![Blog, WebItem]),
        ));

        match (entry.entry_type, canonical.entry_type) {
            (_, Conference) | (_, Proceedings) => {
                if canonical.entry_type == Proceedings {
                    if let Ok(eds) = canonical.get_editors() {
                        let mut al = self.and_list(name_list_straight(&eds));
                        if eds.len() > 1 {
                            al += ", Eds."
                        } else {
                            al += ", Ed."
                        }
                        res.push(al);
                    }

                    if let Ok(vols) =
                        entry.get_volume().or_else(|_| canonical.get_volume())
                    {
                        res.push(format_range("vol.", "vols.", &vols));
                    }

                    if let Ok(ed) = canonical.get_edition() {
                        match ed {
                            NumOrStr::Number(i) => {
                                if *i > 1 {
                                    res.push(format!("{} ed.", en::get_ordinal(*i)));
                                }
                            }
                            NumOrStr::Str(s) => res.push(s.to_string()),
                        }
                    }
                }

                if let Ok(location) = canonical.get_location() {
                    res.push(location.value.clone());
                }

                if canonical.entry_type != Conference || !self.show_url(entry) {
                    if let Some(date) = entry.get_any_date() {
                        if let Some(month) = date.month {
                            res.push(if let Some(day) = date.day {
                                format!(
                                    "{} {}",
                                    en::get_month_abbr(month, true).unwrap(),
                                    day + 1
                                )
                            } else {
                                en::get_month_abbr(month, true).unwrap()
                            });
                        }

                        res.push(date.year.to_string());
                    }
                }

                if canonical.entry_type == Conference {
                    if let Ok(sn) = entry.get_serial_number() {
                        res.push(format!("Paper {}", sn));
                    }
                } else {
                    if let Ok(pages) = entry.get_page_range() {
                        res.push(format_range("p.", "pp.", &pages));
                    }

                    if let Ok(doi) = entry.get_doi() {
                        res.push(format!("doi: {}", doi));
                    }
                }
            }
            (_, Reference) => {
                let has_url = self.show_url(entry);
                let date = entry.get_any_date().map(|date| {
                    let mut res = if let Some(month) = date.month {
                        if let Some(day) = date.day {
                            format!(
                                "{} {}, ",
                                en::get_month_abbr(month, true).unwrap(),
                                day + 1
                            )
                        } else {
                            format!("{} ", en::get_month_abbr(month, true).unwrap())
                        }
                    } else {
                        String::new()
                    };

                    res += &date.year.to_string();
                    res
                });

                if let Ok(ed) = canonical.get_edition() {
                    match ed {
                        NumOrStr::Number(i) => {
                            if *i > 1 {
                                res.push(format!("{} ed.", en::get_ordinal(*i)));
                            }
                        }
                        NumOrStr::Str(s) => res.push(s.clone()),
                    }
                }

                if !has_url {
                    if let Ok(publisher) = canonical
                        .get_organization()
                        .or_else(|_| canonical.get_publisher().map(|e| e.value.as_ref()))
                    {
                        res.push(publisher.to_string());

                        if let Ok(location) = canonical.get_location() {
                            res.push(location.value.clone());
                        }
                    }

                    if let Some(date) = date {
                        res.push(date);
                    }

                    if let Ok(pages) = entry.get_page_range() {
                        res.push(format_range("p.", "pp.", &pages));
                    }
                } else {
                    if let Some(date) = date {
                        res.push(format!("({})", date));
                    }
                }
            }
            (_, Repository) => {
                if let Ok(sn) = canonical.get_serial_number() {
                    res.push(format!("(version {})", sn));
                } else if let Some(date) =
                    canonical.get_date().ok().or_else(|| entry.get_any_date())
                {
                    res.push(format!("({})", date.year));
                }

                if let Ok(publisher) = canonical
                    .get_publisher()
                    .map(|e| e.value.as_ref())
                    .or_else(|_| canonical.get_organization())
                {
                    let mut publ = String::new();
                    if let Ok(location) = canonical.get_location() {
                        publ += &location.value;
                        publ += ": ";
                    }

                    publ += publisher;

                    if let Ok(lang) =
                        entry.get_language().or_else(|_| canonical.get_language())
                    {
                        publ += " (in ";
                        publ += Language::from_639_1(lang.language.as_str())
                            .unwrap()
                            .to_name();
                        publ.push(')');
                    }

                    res.push(publ);
                }
            }
            (_, Video) => {
                if let Some(date) =
                    canonical.get_date().ok().or_else(|| entry.get_any_date())
                {
                    res.push(format!("({})", date.year));
                }
            }
            (_, Patent) => {
                let mut start = String::new();
                if let Ok(location) = canonical.get_location() {
                    start += &location.value;
                    start.push(' ');
                }

                start += "Patent";

                if let Ok(sn) = canonical.get_serial_number() {
                    start += &format!(" {}", sn);
                }

                if self.show_url(entry) {
                    let mut fin = String::new();
                    if let Some(date) = entry.get_any_date() {
                        fin += "(";
                        fin += &date.year.to_string();
                        if let Some(month) = date.month {
                            fin += ", ";
                            fin += &(if let Some(day) = date.day {
                                format!(
                                    "{} {}",
                                    en::get_month_abbr(month, true).unwrap(),
                                    day + 1
                                )
                            } else {
                                en::get_month_abbr(month, true).unwrap()
                            });
                        }
                        fin += "). ";
                    }

                    fin += &start;

                    res.push(fin);
                } else {
                    res.push(start);

                    if let Some(date) = entry.get_any_date() {
                        if let Some(month) = date.month {
                            res.push(if let Some(day) = date.day {
                                format!(
                                    "{} {}",
                                    en::get_month_abbr(month, true).unwrap(),
                                    day + 1
                                )
                            } else {
                                en::get_month_abbr(month, true).unwrap()
                            });
                        }

                        res.push(date.year.to_string());
                    }
                }
            }
            (_, Periodical) => {
                if let Ok(vols) = canonical.get_volume() {
                    res.push(format_range("vol.", "vols.", &vols));
                }

                if let Ok(iss) = canonical.get_issue() {
                    res.push(format!("no. {}", iss));
                }

                let pages = if let Ok(pages) = entry.get_page_range() {
                    res.push(format_range("p.", "pp.", &pages));
                    true
                } else {
                    false
                };

                if let Some(date) = entry.get_any_date() {
                    if let Some(month) = date.month {
                        res.push(if let Some(day) = date.day {
                            format!(
                                "{} {}",
                                en::get_month_abbr(month, true).unwrap(),
                                day + 1
                            )
                        } else {
                            en::get_month_abbr(month, true).unwrap()
                        });
                    }

                    res.push(date.year.to_string());
                }

                if !pages {
                    if let Ok(sn) = entry.get_serial_number() {
                        res.push(format!("Art. no. {}", sn));
                    }
                }

                if let Ok(doi) = entry.get_doi() {
                    res.push(format!("doi: {}", doi));
                }
            }
            (_, Report) => {
                if let Ok(publisher) = canonical
                    .get_organization()
                    .or_else(|_| canonical.get_publisher().map(|e| e.value.as_ref()))
                {
                    res.push(publisher.to_string());

                    if let Ok(location) = canonical.get_location() {
                        res.push(location.value.clone());
                    }
                }

                if let Ok(sn) = canonical.get_serial_number() {
                    res.push(format!("Rep. {}", sn));
                }

                let date = entry.get_any_date().map(|date| {
                    let mut res = if let Some(month) = date.month {
                        if let Some(day) = date.day {
                            format!(
                                "{} {}, ",
                                en::get_month_abbr(month, true).unwrap(),
                                day + 1
                            )
                        } else {
                            format!("{} ", en::get_month_abbr(month, true).unwrap())
                        }
                    } else {
                        String::new()
                    };

                    res += &date.year.to_string();
                    res
                });

                if !self.show_url(entry) {
                    if let Some(date) = date.clone() {
                        res.push(date);
                    }
                }

                if let Ok(vols) = canonical.get_volume().or_else(|_| entry.get_volume()) {
                    res.push(format_range("vol.", "vols.", &vols));
                }


                if let Ok(iss) = canonical.get_issue() {
                    res.push(format!("no. {}", iss));
                }


                if self.show_url(entry) {
                    if let Some(date) = date {
                        res.push(date);
                    }
                }
            }
            (_, Thesis) => {
                res.push("Thesis".to_string());
                if let Ok(org) = canonical.get_organization() {
                    res.push(abbreviations::abbreviate_journal(&org));

                    if let Ok(location) = canonical.get_location() {
                        res.push(location.value.clone());
                    }
                }

                if let Ok(sn) = entry.get_serial_number() {
                    res.push(sn.to_string());
                }

                if let Some(date) = entry.get_any_date() {
                    res.push(date.year.to_string());
                }
            }
            (_, Legislation) => {}
            (_, Manuscript) => {
                res.push("unpublished".to_string());
            }
            _ if preprint.is_ok() => {
                let parent =
                    entry.get_parents().unwrap().get(preprint.unwrap()[0]).unwrap();
                if let Ok(sn) = entry.get_serial_number() {
                    let mut sn = if let Some(url) = entry.get_any_url() {
                        if !sn.to_lowercase().contains("arxiv")
                            && (url.value.host_str().unwrap_or("").to_lowercase()
                                == "arxiv.org"
                                || parent
                                    .get_title()
                                    .map(|e| e.value.to_lowercase())
                                    .unwrap_or("".to_string())
                                    == "arxiv")
                        {
                            format!("arXiv: {}", sn)
                        } else {
                            sn.to_string()
                        }
                    } else {
                        sn.to_string()
                    };

                    if let Ok(al) = entry.get_archive().or_else(|_| parent.get_archive())
                    {
                        sn += " [";
                        sn += &al.value;
                        sn += "]";
                    }

                    res.push(sn);
                }

                if let Some(date) = entry.get_any_date() {
                    if let Some(month) = date.month {
                        res.push(if let Some(day) = date.day {
                            format!(
                                "{} {}",
                                en::get_month_abbr(month, true).unwrap(),
                                day + 1
                            )
                        } else {
                            en::get_month_abbr(month, true).unwrap()
                        });
                    }

                    res.push(date.year.to_string());
                }
            }
            (WebItem, _) | (Blog, _) => {
                if let Ok(publisher) = entry
                    .get_publisher()
                    .map(|e| e.value.as_ref())
                    .or_else(|_| entry.get_organization())
                {
                    res.push(publisher.to_string());
                }
            }
            _ if web_parented.is_ok() => {
                let parent =
                    entry.get_parents().unwrap().get(preprint.unwrap()[0]).unwrap();
                if let Ok(publisher) = parent
                    .get_title()
                    .or_else(|_| parent.get_publisher())
                    .or_else(|_| entry.get_publisher())
                    .map(|e| e.value.clone())
                    .or_else(|_| parent.get_organization().map(|o| o.to_string()))
                    .or_else(|_| entry.get_organization().map(|o| o.to_string()))
                {
                    res.push(publisher);
                }
            }
            _ => {
                if let (Some(_), Ok(eds)) = (
                    entry.get_authors().get(0),
                    entry.get_editors().or_else(|_| canonical.get_editors()),
                ) {
                    let mut al = self.and_list(name_list_straight(&eds));
                    if eds.len() > 1 {
                        al += ", Eds."
                    } else {
                        al += ", Ed."
                    }
                    res.push(al);
                }

                if let Ok(vols) = entry.get_volume().or_else(|_| canonical.get_volume()) {
                    res.push(format_range("vol.", "vols.", &vols));
                }

                if let Ok(ed) = canonical.get_edition() {
                    match ed {
                        NumOrStr::Number(i) => {
                            if *i > 1 {
                                res.push(format!("{} ed.", en::get_ordinal(*i)));
                            }
                        }
                        NumOrStr::Str(s) => res.push(s.clone()),
                    }
                }

                if let Ok(publisher) = canonical
                    .get_publisher()
                    .map(|e| e.value.as_ref())
                    .or_else(|_| canonical.get_organization())
                {
                    let mut publ = String::new();
                    if let Ok(location) = canonical.get_location() {
                        publ += &location.value;
                        publ += ": ";
                    }

                    publ += &publisher;

                    if let Ok(lang) =
                        entry.get_language().or_else(|_| canonical.get_language())
                    {
                        publ += " (in ";
                        publ += Language::from_639_1(lang.language.as_str())
                            .unwrap()
                            .to_name();
                        publ.push(')');
                    }

                    res.push(publ);
                }

                if let Some(date) = canonical.get_any_date() {
                    res.push(date.year.to_string());
                }

                if let Some(chapter) = chapter {
                    res.push(format!("ch. {}", chapter));
                }

                if let Some(section) = section {
                    res.push(format!("sec. {}", section));
                }

                if let Ok(pages) = entry.get_page_range() {
                    res.push(format_range("p.", "pp.", &pages));
                }
            }
        }

        res
    }

    fn formt_date(&self, date: &Date) -> String {
        let mut res = String::new();
        if let Some(month) = date.month {
            res += &(if let Some(day) = date.day {
                format!("{} {},", en::get_month_abbr(month, true).unwrap(), day + 1)
            } else {
                en::get_month_abbr(month, true).unwrap()
            });
            res += " ";
        }

        res += &date.year.to_string();
        res
    }
}

impl BibliographyGenerator for IeeeBibliographyGenerator {
    fn get_reference(&self, mut entry: &Entry) -> DisplayString {
        let mut parent = entry.get_parents().ok().and_then(|v| v.first());
        let mut sn_stack = vec![];
        while entry.get_title().is_err()
            && entry
                .entry_type
                .check(EntryTypeModality::Alternate(vec![Chapter, Scene]))
        {
            if let Ok(sn) = entry.get_serial_number() {
                sn_stack.push(sn);
            }
            if let Some(p) = parent {
                entry = &p;
                parent = entry.get_parents().ok().and_then(|v| v.first());
            } else {
                break;
            }
        }

        if entry.entry_type == Chapter {
            if let Ok(sn) = entry.get_serial_number() {
                sn_stack.push(sn);
            }
        }

        let secs = sn_stack
            .into_iter()
            .map(|s| str::parse::<u32>(&s))
            .filter(|s| s.is_ok())
            .map(|s| s.unwrap())
            .collect::<Vec<_>>();

        let chapter = secs.get(0).map(|c| c.clone());
        let section = if secs.len() > 1 {
            secs.last().map(|c| c.clone())
        } else {
            None
        };

        let url = self.show_url(entry);

        let parent = get_canonical_parent(entry);
        let canonical = parent.unwrap_or(entry);

        let authors = self.get_author(entry, canonical);
        let title = self.get_title_element(entry, canonical);
        let addons = self.get_addons(entry, canonical, chapter, section);

        let mut res = DisplayString::from_string(authors);

        if canonical.entry_type == Legislation {
            if let Ok(NumOrStr::Str(session)) = entry.get_edition() {
                if !res.is_empty() {
                    res += ". ";
                }
                res += session;
            }
        }

        if canonical.entry_type == Video {
            if let Ok(location) = canonical.get_location() {
                if !res.is_empty() {
                    res += ", ";
                }
                res += &location.value;
            }
        } else if canonical.entry_type == Legislation
            || ((canonical.entry_type == Conference || canonical.entry_type == Patent)
                && url)
        {
            if let Some(date) = entry.get_any_date() {
                if !res.is_empty() {
                    res += ". ";
                }
                res.push('(');
                res += &self.formt_date(&date);
                res.push(')');
            }
        }

        if !res.is_empty() && !title.is_empty() {
            if canonical.entry_type == Legislation
                || canonical.entry_type == Video
                || ((canonical.entry_type == Conference
                    || canonical.entry_type == Patent)
                    && url)
            {
                res += ". ";
            } else {
                res += ", ";
            }
        }
        res += title;

        let cur_len = res.len();
        if cur_len > 4
            && res.value.is_char_boundary(cur_len - 4)
            && &res.value[cur_len - 4 ..] == ",”"
        {
            if addons.is_empty() {
                res.value = (&res.value[.. cur_len - 4]).to_string();
                res.value += "”";
            } else {
                res.push(' ');
            }
        } else if !res.is_empty() && !addons.is_empty() {
            res += ", ";
        }

        let addon_count = addons.len();
        for (index, addon) in addons.into_iter().enumerate() {
            res += &addon;
            if index + 1 < addon_count {
                res += ", "
            }
        }

        let cur_len = res.len();
        if cur_len > 3
            && res.value.is_char_boundary(cur_len - 3)
            && &res.value[cur_len - 3 ..] == "”"
        {
            res.value = (&res.value[.. cur_len - 3]).to_string();
            res.value += ".”";
        } else if !res.is_empty() {
            res += ".";
        }

        if url {
            if let Some(url) = entry.get_any_url() {
                if !res.is_empty() {
                    res += " ";
                }

                if canonical.entry_type != WebItem && canonical.entry_type != Blog {
                    if let Some(date) = &url.visit_date {
                        res += &format!("Accessed: {}. ", self.formt_date(&date));
                    }

                    if canonical.entry_type == Video {
                        res += "[Online Video]";
                    } else {
                        res += "[Online]";
                    }

                    res += ". Available: ";
                    res += url.value.as_str();
                } else {
                    res += url.value.as_str();

                    if let Some(date) = &url.visit_date {
                        res += &format!(" (accessed: {}).", self.formt_date(&date));
                    }
                }
            }
        }

        if let Ok(note) = entry.get_note() {
            if !res.is_empty() {
                res += " ";
            }

            res += &format!("({})", note);
        }

        res
    }
}
