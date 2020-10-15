use super::Entry;

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
        let entry = &self.entries[index];

        todo!()
    }

    fn get_title(&self, index: usize) -> String {
        let entry = &self.entries[index];

        todo!()
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
