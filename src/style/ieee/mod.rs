mod abbreviations;

use std::vec;

use isolang::Language;

use super::{
    alph_designator, format_range, name_list_straight, push_comma_quote_aware,
    sorted_bibliography, BibliographyOrdering, BibliographyStyle, Database,
    DisplayReference, DisplayString, Formatting, Record,
};
use crate::lang::{en, SentenceCase, TitleCase};
use crate::types::{Date, EntryType::*, FmtOptionExt, NumOrStr, Person, PersonRole};
use crate::Entry;

/// Bibliographies following IEEE guidance.
///
/// The IEEE style is best paired with [`Numerical`](super::Numerical) citations.
///
/// # Examples
/// - S. Mohan, C. Thirumalai, and G. Srivastava, “Effective heart disease
///   prediction using hybrid machine learning techniques,” _IEEE Access,_ vol.
///   7, Jun. 19, 2019, doi: 10.1109/ACCESS.2019.
/// - R. Roundy, “Report on practices related to demand forecasting for
///   semiconductor products,” Cornell University Operations Research and
///   Industrial Engineering, Jul. 2001. Accessed: Jan. 7, 2021. \[Online\].
///   Available: <https://hdl.handle.net/1813/9174>
///
/// # Reference
/// See the following documents for details on how the Institute of Electrical
/// and Electronics Engineers advises you to format citations and
/// bibliographies:
/// - [2018 IEEE Reference Guide][ref-guide]
/// - ["How to Cite References: The IEEE Citation Style"][how-to]
///
/// [ref-guide]: https://ieeeauthorcenter.ieee.org/wp-content/uploads/IEEE-Reference-Guide.pdf
/// [how-to]: https://ieee-dataport.org/sites/default/files/analysis/27/IEEE%20Citation%20Guidelines.pdf
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Ieee {
    /// The sentence case configuration. Used for paper titles etc.
    pub sentence_case: SentenceCase,
    /// The title case configuration. Used for journal titles etc.
    pub title_case: TitleCase,
    /// How many authors have to be there for their list to be abbreviated with
    /// "et al."
    pub et_al_threshold: Option<u32>,
    /// Whether to use abbreviations for journal titles
    pub abbreviate_journals: bool,
}

fn get_canonical_parent(entry: &Entry) -> Option<&Entry> {
    let section = select!((Chapter | Scene | Web) > ("p":*));
    let anthology = select!(Anthos > ("p": Anthology));
    let entry_spec = select!(Entry > ("p":(Reference | Repository)));
    let proceedings = select!(* > ("p":(Conference | Proceedings | Periodical)));

    section
        .apply(entry)
        .or_else(|| proceedings.apply(entry))
        .or_else(|| anthology.apply(entry))
        .or_else(|| entry_spec.apply(entry))
        .and_then(|mut bindings| bindings.remove("p"))
}

impl Default for Ieee {
    fn default() -> Self {
        let title_case = TitleCase {
            always_capitalize_min_len: Some(4),
            ..TitleCase::default()
        };
        Self {
            sentence_case: SentenceCase::default(),
            title_case,
            et_al_threshold: Some(6),
            abbreviate_journals: true,
        }
    }
}

impl Ieee {
    /// Creates a new IEEE bibliography generator.
    pub fn new() -> Self {
        Self::default()
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
        entry.url_any().is_some()
    }

    fn get_author(&self, entry: &Entry, canonical: &Entry) -> (String, Vec<Person>) {
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

            if tv_series.matches(entry) {
                // TV episode
                let mut dir_name_list_straight = name_list_straight(&dirs)
                    .into_iter()
                    .map(|s| format!("{} (Director)", s))
                    .collect::<Vec<String>>();

                let writers = entry.affiliated_with_role(PersonRole::Writer);
                let mut writers_name_list_straight = name_list_straight(&writers)
                    .into_iter()
                    .map(|s| format!("{} (Writer)", s))
                    .collect::<Vec<String>>();
                dir_name_list_straight.append(&mut writers_name_list_straight);

                if !dirs.is_empty() {
                    names = Some(dir_name_list_straight);
                    pers_refs.extend(dirs);
                    pers_refs.extend(writers);
                }
            } else {
                // Film
                if !dirs.is_empty() {
                    names = Some(name_list_straight(&dirs));
                    pers_refs.extend(dirs);
                    role = AuthorRole::Director;
                } else {
                    // TV show
                    let prods = entry.affiliated_with_role(PersonRole::ExecutiveProducer);

                    if !prods.is_empty() {
                        names = Some(name_list_straight(&prods));
                        pers_refs.extend(prods);
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let authors = if let Some(names) = names {
            Some(names)
        } else if let Some(authors) = entry.authors().or_else(|| canonical.authors()) {
            let list = name_list_straight(&authors);
            pers_refs.extend(authors.iter().cloned());
            Some(list)
        } else {
            None
        };

        let al = if let Some(authors) = authors {
            let count = authors.len();
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
        } else if let Some(eds) = entry.editors() {
            let res = if !eds.is_empty() {
                format!(
                    "{}, {}",
                    self.and_list(name_list_straight(&eds)),
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

        (al, pers_refs)
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
            let canon_title = canonical.title();

            if let Some(title) = entry.title() {
                let sentence = title.canonical.format_sentence_case(&self.sentence_case);
                if canonical.entry_type == Conference {
                    res += &sentence;
                    res.push('.');
                } else {
                    res += "“";
                    res += &sentence;
                    res += ",”";
                }

                if canon_title.is_some() {
                    res.push(' ');
                }
            }

            if let Some(title) = canon_title {
                let title_case = title.canonical.format_title_case(&self.title_case);
                let ct = if self.abbreviate_journals {
                    abbreviations::abbreviate_journal(&title_case)
                } else {
                    title_case.to_string()
                };

                if canonical.entry_type == Conference {
                    res += "Presented at ";
                    res += &ct;
                } else {
                    if let Some(lang) = entry.language().or_else(|| canonical.language())
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
                    res.start_format(Formatting::Italic);
                    res += &ct;
                    res.commit_formats();

                    // Do the series parentheses thing here
                    let spec = select!(Anthology > ("p":(Anthology["title"])));
                    if let Some(mut bindings) = spec.apply(canonical) {
                        let parenth_anth = bindings.remove("p").unwrap();

                        res += " (";
                        res += &parenth_anth
                            .title()
                            .unwrap()
                            .canonical
                            .format_title_case(&self.title_case);

                        res.add_if_some(
                            parenth_anth.issue().map(|i| i.to_string()),
                            Some(", no. "),
                            None,
                        );
                        res += ")";
                    }

                    // And the conference series thing as well
                    let spec =
                        select!(Proceedings > ("p":(Proceedings | Anthology | Misc)));
                    if let Some(mut bindings) = spec.apply(canonical) {
                        let par_conf = bindings.remove("p").unwrap();
                        if let Some(parenth_title) = par_conf.title() {
                            res += " in ";
                            res += &parenth_title
                                .canonical
                                .format_title_case(&self.title_case);
                        }
                    }
                }
            }
        }
        // No canonical parent
        else if matches!(
            entry.entry_type,
            Legislation | Repository | Video | Reference | Book | Proceedings | Anthology
        ) {
            res.start_format(Formatting::Italic);

            if entry.entry_type == Legislation {
                res.add_if_some(entry.serial_number(), None, None);
            }

            if let Some(title) = entry.title() {
                if !res.is_empty() {
                    res += ", ";
                }

                res += &title.canonical.format_title_case(&self.title_case);
            }

            res.commit_formats();
        } else if let Some(title) = entry.title() {
            res += "“";
            res += &title.canonical.format_sentence_case(&self.sentence_case);
            res += ",”";
        }

        res
    }

    fn get_addons(
        &self,
        entry: &Entry,
        canonical: &Entry,
        chapter: Option<u32>,
        section: Option<u32>,
        disamb: Option<usize>,
    ) -> Vec<String> {
        let mut res = vec![];
        let preprint =
            select!((Article | Book | Anthos) > ("p": Repository)).apply(entry);
        let web_parented = select!(* > ("p":(Blog | Web))).apply(entry);

        match (entry.entry_type, canonical.entry_type) {
            (_, Conference) | (_, Proceedings) => {
                if canonical.entry_type == Proceedings {
                    if let Some(eds) = canonical.editors() {
                        let mut al = self.and_list(name_list_straight(&eds));
                        if eds.len() > 1 {
                            al += ", Eds."
                        } else {
                            al += ", Ed."
                        }
                        res.push(al);
                    }

                    if let Some(vols) = entry.volume().or_else(|| canonical.volume()) {
                        res.push(format_range("vol.", "vols.", &vols));
                    }

                    if let Some(ed) = canonical.edition() {
                        match ed {
                            NumOrStr::Number(i) => {
                                if *i > 1 {
                                    res.push(format!("{} ed.", en::get_ordinal(*i)));
                                }
                            }
                            NumOrStr::Str(s) => res.push(s.clone()),
                        }
                    }
                }

                if let Some(loc) = canonical.location() {
                    res.push(loc.value.clone());
                }

                if canonical.entry_type != Conference || !self.show_url(entry) {
                    if let Some(date) = entry.date_any() {
                        res.push(format_date(date, disamb))
                    }
                }

                if canonical.entry_type == Conference {
                    if let Some(sn) = entry.serial_number() {
                        res.push(format!("Paper {}", sn));
                    }
                } else {
                    if let Some(pages) = entry.page_range() {
                        res.push(format_range("p.", "pp.", &pages));
                    }

                    if let Some(doi) = entry.doi() {
                        res.push(format!("doi: {}", doi));
                    }
                }
            }
            (_, Reference) => {
                let has_url = self.show_url(entry);
                let date = entry.date_any().map(|date| format_date(date, disamb));

                if let Some(ed) = canonical.edition() {
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
                    if let Some(publisher) =
                        canonical.organization().or_else(|| canonical.publisher().value())
                    {
                        res.push(publisher.into());

                        if let Some(loc) = canonical.location() {
                            res.push(loc.value.clone());
                        }
                    }

                    if let Some(date) = date {
                        res.push(date);
                    }

                    if let Some(pages) = entry.page_range() {
                        res.push(format_range("p.", "pp.", &pages));
                    }
                } else if let Some(date) = date {
                    res.push(format!("({})", date));
                }
            }
            (_, Repository) => {
                if let Some(sn) = canonical.serial_number() {
                    res.push(format!("(version {})", sn));
                } else if let Some(date) = canonical.date().or_else(|| entry.date_any()) {
                    res.push(format!("({})", date.year));
                }

                if let Some(publisher) =
                    canonical.publisher().value().or_else(|| canonical.organization())
                {
                    let mut publ = String::new();
                    if let Some(location) = canonical.location() {
                        publ += &location.value;
                        publ += ": ";
                    }

                    publ += publisher;

                    if let Some(lang) = entry.language().or_else(|| canonical.language())
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
                if let Some(date) = canonical.date().or_else(|| entry.date_any()) {
                    res.push(format!("({})", date.year));
                }
            }
            (_, Patent) => {
                let mut start = String::new();
                if let Some(location) = canonical.location() {
                    start += &location.value;
                    start.push(' ');
                }

                start += "Patent";

                if let Some(sn) = canonical.serial_number() {
                    start += &format!(" {}", sn);
                }

                if self.show_url(entry) {
                    let mut fin = String::new();
                    if let Some(date) = entry.date_any() {
                        fin += "(";
                        fin += &date.display_year();
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

                    if let Some(date) = entry.date_any() {
                        res.push(format_date(date, disamb));
                    }
                }
            }
            (_, Periodical) => {
                if let Some(vols) = canonical.volume() {
                    res.push(format_range("vol.", "vols.", &vols));
                }

                if let Some(iss) = canonical.issue() {
                    res.push(format!("no. {}", iss));
                }

                let pages = if let Some(pages) = entry.page_range() {
                    res.push(format_range("p.", "pp.", &pages));
                    true
                } else {
                    false
                };

                if let Some(date) = entry.date_any() {
                    res.push(format_date(date, disamb));
                }

                if !pages {
                    if let Some(sn) = entry.serial_number() {
                        res.push(format!("Art. no. {}", sn));
                    }
                }

                if let Some(doi) = entry.doi() {
                    res.push(format!("doi: {}", doi));
                }
            }
            (_, Report) => {
                if let Some(publisher) =
                    canonical.organization().or_else(|| canonical.publisher().value())
                {
                    res.push(publisher.into());

                    if let Some(location) = canonical.location() {
                        res.push(location.value.clone());
                    }
                }

                if let Some(sn) = canonical.serial_number() {
                    res.push(format!("Rep. {}", sn));
                }

                let date = entry.date_any().map(|date| format_date(date, disamb));

                if !self.show_url(entry) {
                    if let Some(date) = date.clone() {
                        res.push(date);
                    }
                }

                if let Some(vols) = canonical.volume().or_else(|| entry.volume()) {
                    res.push(format_range("vol.", "vols.", &vols));
                }

                if let Some(iss) = canonical.issue() {
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
                if let Some(org) = canonical.organization() {
                    res.push(if self.abbreviate_journals {
                        abbreviations::abbreviate_journal(&org)
                    } else {
                        org.to_string()
                    });

                    if let Some(location) = canonical.location() {
                        res.push(location.value.clone());
                    }
                }

                if let Some(sn) = entry.serial_number() {
                    res.push(sn.into());
                }

                if let Some(date) = entry.date_any() {
                    res.push(date.display_year());
                }
            }
            (_, Legislation) => {}
            (_, Manuscript) => {
                res.push("unpublished".to_string());
            }
            _ if preprint.is_some() => {
                let parent = preprint.unwrap().remove("p").unwrap();
                if let Some(serial) = entry.serial_number() {
                    let mut sn = if let Some(url) = entry.url_any() {
                        let has_arxiv_serial = serial.to_lowercase().contains("arxiv");

                        let has_url = url
                            .value
                            .host_str()
                            .map(|h| h.to_lowercase())
                            .map_or(false, |h| h.as_str() == "arxiv.org");

                        let has_parent = parent
                            .title()
                            .map(|e| e.canonical.value.to_lowercase())
                            .map_or(false, |v| v.as_str() == "arxiv");

                        if !has_arxiv_serial && (has_url || has_parent) {
                            format!("arXiv: {}", serial)
                        } else {
                            serial.to_string()
                        }
                    } else {
                        serial.to_string()
                    };

                    if let Some(al) = entry.archive().or_else(|| parent.archive()) {
                        sn += " [";
                        sn += &al.value;
                        sn += "]";
                    }

                    res.push(sn);
                }

                if let Some(date) = entry.date_any() {
                    res.push(format_date(date, disamb));
                }
            }
            (Web, _) | (Blog, _) => {
                if let Some(publisher) = entry
                    .publisher()
                    .map(|publ| publ.value.as_str())
                    .or_else(|| entry.organization())
                {
                    res.push(publisher.into());
                }
            }
            _ if web_parented.is_some() => {
                let parent = web_parented.unwrap().remove("p").unwrap();
                if let Some(publisher) = parent
                    .title()
                    .map(|t| &t.canonical)
                    .or_else(|| parent.publisher())
                    .or_else(|| entry.publisher())
                    .value()
                    .or_else(|| parent.organization())
                    .or_else(|| entry.organization())
                {
                    res.push(publisher.into());
                }
            }
            _ => {
                if let (Some(_), Some(eds)) = (
                    entry.authors().unwrap_or_default().get(0),
                    entry.editors().or_else(|| canonical.editors()),
                ) {
                    let mut al = self.and_list(name_list_straight(&eds));
                    if eds.len() > 1 {
                        al += ", Eds."
                    } else {
                        al += ", Ed."
                    }
                    res.push(al);
                }

                if let Some(vols) = entry.volume().or_else(|| canonical.volume()) {
                    res.push(format_range("vol.", "vols.", &vols));
                }

                if let Some(ed) = canonical.edition() {
                    match ed {
                        NumOrStr::Number(i) => {
                            if *i > 1 {
                                res.push(format!("{} ed.", en::get_ordinal(*i)));
                            }
                        }
                        NumOrStr::Str(s) => res.push(s.clone()),
                    }
                }

                if let Some(publisher) =
                    canonical.publisher().value().or_else(|| canonical.organization())
                {
                    let mut publ = String::new();
                    if let Some(location) = canonical.location() {
                        publ += &location.value;
                        publ += ": ";
                    }

                    publ += &publisher;

                    if let Some(lang) = entry.language().or_else(|| canonical.language())
                    {
                        publ += " (in ";
                        publ += Language::from_639_1(lang.language.as_str())
                            .unwrap()
                            .to_name();
                        publ.push(')');
                    }

                    res.push(publ);
                }

                if let Some(date) = canonical.date_any() {
                    res.push(date.display_year());
                }

                if let Some(chapter) = chapter {
                    res.push(format!("ch. {}", chapter));
                }

                if let Some(section) = section {
                    res.push(format!("sec. {}", section));
                }

                if let Some(pages) = entry.page_range() {
                    res.push(format_range("p.", "pp.", &pages));
                }
            }
        }

        res
    }

    fn get_single_record<'a>(
        &self,
        record: &Record<'a>,
    ) -> (DisplayReference<'a>, Vec<Person>) {
        let mut entry = record.entry;
        let mut parent = entry.parents().and_then(|v| v.first());
        let mut sn_stack = vec![];
        while entry.title().is_none() && select!(Chapter | Scene).matches(entry) {
            if let Some(sn) = entry.serial_number() {
                sn_stack.push(sn);
            }
            if let Some(p) = parent {
                entry = &p;
                parent = entry.parents().and_then(|v| v.first());
            } else {
                break;
            }
        }

        if entry.entry_type == Chapter {
            if let Some(sn) = entry.serial_number() {
                sn_stack.push(sn);
            }
        }

        let secs = sn_stack
            .into_iter()
            .map(|s| str::parse::<u32>(&s))
            .filter(|s| s.is_ok())
            .map(|s| s.unwrap())
            .collect::<Vec<_>>();

        let chapter = secs.get(0).copied();
        let section = if secs.len() > 1 { secs.last().copied() } else { None };

        let url = self.show_url(entry);

        let parent = get_canonical_parent(entry);
        let canonical = parent.unwrap_or(entry);

        let (authors, al) = self.get_author(entry, canonical);
        let title = self.get_title_element(entry, canonical);
        let addons =
            self.get_addons(entry, canonical, chapter, section, record.disambiguation);

        let mut res = DisplayString::from_string(authors);

        if canonical.entry_type == Legislation {
            if let Some(NumOrStr::Str(session)) = entry.edition() {
                if !res.is_empty() {
                    res += ". ";
                }
                res += session;
            }
        }

        if canonical.entry_type == Video {
            if let Some(location) = canonical.location() {
                if !res.is_empty() {
                    res += ", ";
                }
                res += &location.value;
            }
        } else if canonical.entry_type == Legislation
            || ((canonical.entry_type == Conference || canonical.entry_type == Patent)
                && url)
        {
            if let Some(date) = entry.date_any() {
                if !res.is_empty() {
                    res += ". ";
                }
                res.push('(');
                res += &format_date(&date, record.disambiguation);
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
            && &res.value[cur_len - 4..] == ",”"
        {
            if addons.is_empty() {
                res.value = (&res.value[..cur_len - 4]).into();
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

        push_comma_quote_aware(&mut res.value, '.', false);

        if url {
            if let Some(url) = entry.url_any() {
                if !res.is_empty() {
                    res += " ";
                }

                if canonical.entry_type != Web && canonical.entry_type != Blog {
                    if let Some(date) = &url.visit_date {
                        res += &format!("Accessed: {}. ", format_date(&date, None));
                    }

                    if canonical.entry_type == Video {
                        res += "[Online Video]";
                    } else {
                        res += "[Online]";
                    }

                    res += ". Available: ";
                    res.start_format(Formatting::Link(url.value.to_string()));
                    res += url.value.as_str();
                    res.commit_formats();
                } else {
                    res.start_format(Formatting::Link(url.value.to_string()));
                    res += url.value.as_str();
                    res.commit_formats();

                    if let Some(date) = &url.visit_date {
                        res += &format!(" (accessed: {}).", format_date(&date, None));
                    }
                }
            }
        }

        if let Some(note) = entry.note() {
            if !res.is_empty() {
                res += " ";
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

fn format_date(date: &Date, disamb: Option<usize>) -> String {
    let mut res = String::new();
    if let Some(month) = date.month {
        res += &(if let Some(day) = date.day {
            format!("{} {},", en::get_month_abbr(month, true).unwrap(), day + 1)
        } else {
            en::get_month_abbr(month, true).unwrap()
        });
        res += " ";
    }

    res += &date.display_year();
    if let Some(disamb) = disamb {
        res.push(alph_designator(disamb).to_ascii_uppercase());
    }
    res
}

impl<'a> BibliographyStyle<'a> for Ieee {
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
        BibliographyOrdering::ByNumericPrefix
    }
}
