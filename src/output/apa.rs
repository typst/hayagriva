use super::Entry;
use crate::lang::en::{get_month_name, get_ordinal};
use crate::lang::{CaseTransformer, SentenceCaseTransformer};
use crate::types::{EntryType, EntryTypeModality, EntryTypeSpec, NumOrStr};

#[derive(Clone, Debug)]
pub struct ApaBibliographyGenerator<'s> {
    entries: &'s [Entry],
}

impl<'s> ApaBibliographyGenerator<'s> {
    pub fn new(entries: &'s [Entry]) -> Self {
        Self { entries }
    }

    fn get_author(&self, index: usize) -> String {
        let entry = &self.entries[index];

        let mut names = vec![];

        for author in entry.get_authors().iter() {
            let mut single = if let Some(prefix) = &author.prefix {
                format!("{} {}", prefix, author.name)
            } else {
                author.name.clone()
            };

            if let Some(initials) = author.get_initials(Some(".")) {
                single += ", ";
                single += &initials;
            }

            if let Some(suffix) = &author.suffix {
                single += ", ";
                single += suffix;
            }

            names.push(single);
        }

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

    fn get_date(&self, index: usize) -> String {
        let date = &self.entries[index].get_date();

        if let Ok(date) = date {
            match (date.month, date.day) {
                (None, _) => format!("({:04})", date.year),
                (Some(month), None) => {
                    format!("({:04}, {})", date.year, get_month_name(month).unwrap())
                }
                (Some(month), Some(day)) => format!(
                    "({:04}, {} {})",
                    date.year,
                    get_month_name(month).unwrap(),
                    day,
                ),
            }
        } else {
            "(n. d.)".to_string()
        }
    }

    fn get_retreival_date(&self, index: usize) -> Option<String> {
        let url = &self.entries[index].get_url();

        if let Ok(qurl) = url {
            let uv = qurl.value.as_str();
            let res = if let Some(date) = &qurl.visit_date {
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
            };

            Some(res)
        } else {
            None
        }
    }

    fn get_title(&self, index: usize) -> Option<String> {
        let entry = &self.entries[index];
        let scase_transformer = SentenceCaseTransformer::default();
        if let Ok(title) = entry.get_title_fmt(None, Some(&scase_transformer)) {
            let multivol_spec = EntryTypeSpec::with_specific(EntryType::Book, vec![
                EntryTypeSpec::with_single(EntryType::Book),
            ]);

            let mut multivolume_parent = None;
            if let Ok(parents) = entry.check_with_spec(multivol_spec) {
                if entry.get_volume().is_ok() {
                    multivolume_parent = Some(parents[0]);
                }
            }

            let book_spec = EntryTypeSpec::new(
                EntryTypeModality::Alternate(vec![
                    EntryType::Book,
                    EntryType::Report,
                    EntryType::Reference,
                    EntryType::Anthology,
                    EntryType::Proceedings,
                ]),
                vec![],
            );

            let mut res = title.sentence_case;

            if let Some(mv_parent) = multivolume_parent {
                let p: &Entry = &entry.get_parents().unwrap()[mv_parent];
                let vols = entry.get_volume().unwrap();
                if vols.start == vols.end {
                    res = format!(
                        "{}: Vol. {}. {}.",
                        p.get_title_fmt(None, Some(&scase_transformer))
                            .unwrap()
                            .sentence_case,
                        vols.start,
                        res
                    );
                } else {
                    res = format!(
                        "{}: Vols. {}-{}. {}.",
                        p.get_title_fmt(None, Some(&scase_transformer))
                            .unwrap()
                            .sentence_case,
                        vols.start,
                        vols.end,
                        res
                    );
                }
            } else if (entry.get_volume().is_ok() || entry.get_edition().is_ok())
                && entry.check_with_spec(book_spec).is_ok()
            {
                let vstr = if let Ok(vols) = entry.get_volume() {
                    Some(if vols.start == vols.end {
                        format!("Vol. {}", vols.start)
                    } else {
                        format!("Vols. {}-{}", vols.start, vols.end)
                    })
                } else {
                    None
                };

                let estr = if let Ok(ed) = entry.get_edition() {
                    Some(match ed {
                        NumOrStr::Number(e) => get_ordinal(e),
                        NumOrStr::Str(s) => s,
                    })
                } else {
                    None
                };

                match (estr, vstr) {
                    (None, None) => unreachable!(),
                    (Some(e), None) => res += &format!(" ({} ed.).", e),
                    (None, Some(v)) => res += &format!(" ({}).", v),
                    (Some(e), Some(v)) => res += &format!(" ({} ed., {}).", e, v),
                }
            } else {
                let lc = res.chars().last().unwrap_or('a');

                if lc != '?' && lc != '.' && lc != '!' {
                    res.push('.');
                }
            }

            Some(res)
        } else {
            None
        }
    }

    fn get_source(&self, index: usize) -> String {
        let entry = &self.entries[index];

        todo!()
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
        let ets = vec![entry];

        let apa = ApaBibliographyGenerator::new(&ets);
        assert_eq!(
            "van de Graf, J., Günther, H.-J., & Mädje, L. E.",
            apa.get_author(0)
        );
    }
}
