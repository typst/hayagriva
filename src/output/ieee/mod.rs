mod abbreviations;
use crate::lang::{SentenceCase, TitleCase};
use super::{name_list_straight};
use crate::types::{
    DisplayString, EntryTypeModality, EntryTypeSpec, FormattableString, FormattedString, FormatVariantOptions, Person, PersonRole
};
use crate::types::EntryType::*;
use crate::Entry;

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
        EntryTypeModality::Specific(Proceedings),
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
        .map(|r| entry.get_parents_ref().unwrap().get(r[0]).unwrap())
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

    pub fn get_author(&self, entry: &Entry) -> String {
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
                    .unwrap_or_else(|_| vec![])
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Director)
                    .map(|(v, _)| v)
                    .flatten()
                    .collect::<Vec<Person>>();
                let mut dir_name_list_straight = name_list_straight(&dirs)
                    .into_iter()
                    .map(|s| format!("{} (Director)", s))
                    .collect::<Vec<String>>();
                let writers = entry
                    .get_affiliated_persons()
                    .unwrap_or_else(|_| vec![])
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Writer)
                    .map(|(v, _)| v)
                    .flatten()
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
                    .unwrap_or_else(|_| vec![])
                    .into_iter()
                    .filter(|(_, role)| role == &PersonRole::Director)
                    .map(|(v, _)| v)
                    .flatten()
                    .collect::<Vec<Person>>();

                if !dirs.is_empty() {
                    names = Some(name_list_straight(&dirs));
                    role = AuthorRole::Director;
                } else {
                    // TV show
                    let prods = entry
                        .get_affiliated_persons()
                        .unwrap_or_else(|_| vec![])
                        .into_iter()
                        .filter(|(_, role)| role == &PersonRole::ExecutiveProducer)
                        .map(|(v, _)| v)
                        .flatten()
                        .collect::<Vec<Person>>();

                    if !prods.is_empty() {
                        names = Some(name_list_straight(&prods));
                        role = AuthorRole::ExecutiveProducer;
                    }
                }
            }
        }

        let authors = names.unwrap_or_else(|| name_list_straight(&entry.get_authors()));
        let count = authors.len();
        let mut al = if !authors.is_empty() {
            let amps = self.and_list(authors);
            match role {
                AuthorRole::Normal => amps,
                AuthorRole::ExecutiveProducer if count == 1 => {
                    format!("{}, Executive Prod.", amps)
                }
                AuthorRole::ExecutiveProducer => {
                    format!("{}, Executive Prods.", amps)
                }
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

    pub fn get_title_element(&self, entry: &Entry) -> DisplayString {
        let parent = get_canonical_parent(entry);
        let canonical = parent.unwrap_or(entry);

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
                    if entry.entry_type != Article || canonical.entry_type != Periodical {
                        res += "in ";
                    }
                    res.start_format(FormatVariantOptions::Italic);
                    res += &ct;
                    res.commit_formats();

                    // Do the series parentheses thing here
                    let spec = EntryTypeSpec::single_parent(EntryTypeModality::Specific(Anthology), EntryTypeModality::Specific(Anthology));
                    if let Ok(par_anth) = canonical.check_with_spec(spec) {
                        let par_anth = canonical.get_parents_ref().unwrap().get(par_anth[0]).unwrap();
                        if let Ok(par_t) = par_anth.get_title_fmt(Some(&self.tc_formatter), None) {
                            res += " (";
                            res += &par_t.title_case;

                            res.add_if_ok(par_anth.get_issue().map(|v| v.to_string()), Some(", no. "), None);
                            res += ")";
                        }
                    }

                    // And the conference series thing as well
                    let spec = EntryTypeSpec::single_parent(EntryTypeModality::Specific(Proceedings), EntryTypeModality::Alternate(vec![Proceedings, Anthology, Misc]));
                    if let Ok(par_conf) = canonical.check_with_spec(spec) {
                        let par_conf = canonical.get_parents_ref().unwrap().get(par_conf[0]).unwrap();
                        if let Ok(par_t) = par_conf.get_title_fmt(Some(&self.tc_formatter), None) {
                            res += " in ";
                            res += &par_t.title_case;
                        }
                    }
                }
            }
        // No canonical parent
        } else if [Legislation, Repository, Video, Reference, Book, Proceedings, Anthology].contains(&entry.entry_type) {
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
}
