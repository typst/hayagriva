use std::cmp::Ordering;

use citationberg::taxonomy::Variable;
use citationberg::{
    DemoteNonDroppingParticle, InheritableNameOptions, LocaleCode, LongShortForm, Sort,
    SortDirection, SortKey,
};

use crate::csl::rendering::RenderCsl;
use crate::csl::BufWriteFormat;

use super::taxonomy::EntryLike;
use super::{CitationItem, InstanceContext, StyleContext};

impl<'a> StyleContext<'a> {
    /// Retrieve the ordering of two entries according to the given sort key.
    fn cmp_entries<T: EntryLike>(
        &self,
        a: &CitationItem<T>,
        a_idx: usize,
        b: &CitationItem<T>,
        b_idx: usize,
        key: &SortKey,
        term_locale: Option<&LocaleCode>,
    ) -> Ordering {
        let ordering = match key {
            SortKey::Variable { variable: Variable::Standard(s), .. } => {
                let a = InstanceContext::sort_instance(a, a_idx)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string());
                let b = InstanceContext::sort_instance(b, b_idx)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string());

                a.cmp(&b)
            }
            SortKey::Variable { variable: Variable::Date(d), .. } => {
                let a = a.entry.resolve_date_variable(*d);
                let b = b.entry.resolve_date_variable(*d);

                match (a, b) {
                    (Some(a), Some(b)) => a.csl_cmp(&b),
                    (Some(_), None) => Ordering::Greater,
                    (None, Some(_)) => Ordering::Less,
                    (None, None) => Ordering::Equal,
                }
            }
            SortKey::Variable { variable: Variable::Name(n), .. } => {
                let a = a.entry.resolve_name_variable(*n);
                let b = b.entry.resolve_name_variable(*n);

                for (a_pers, b_pers) in a.iter().zip(b.iter()) {
                    let ord = a_pers.csl_cmp(
                        b_pers,
                        LongShortForm::Long,
                        self.csl.settings.demote_non_dropping_particle
                            != DemoteNonDroppingParticle::Never,
                    );
                    if ord != Ordering::Equal {
                        return ord;
                    }
                }

                if a.len() < b.len() {
                    Ordering::Less
                } else if a.len() > b.len() {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            }
            SortKey::Variable { variable: Variable::Number(n), .. } => {
                let a =
                    InstanceContext::sort_instance(a, a_idx).resolve_number_variable(*n);
                let b =
                    InstanceContext::sort_instance(b, b_idx).resolve_number_variable(*n);

                match (a, b) {
                    (Some(a), Some(b)) => a.csl_cmp(&b),
                    (Some(_), None) => Ordering::Greater,
                    (None, Some(_)) => Ordering::Less,
                    (None, None) => Ordering::Equal,
                }
            }
            SortKey::MacroName {
                name,
                names_min,
                names_use_first,
                names_use_last,
                ..
            } => {
                let render = |entry: &CitationItem<T>, idx: usize| {
                    let mut ctx =
                        self.sorting_ctx(entry, idx, entry.locale.as_ref(), term_locale);
                    ctx.writing.name_options.push(InheritableNameOptions {
                        et_al_min: *names_min,
                        et_al_subsequent_min: *names_min,
                        et_al_use_first: *names_use_first,
                        et_al_subsequent_use_first: *names_use_first,
                        et_al_use_last: *names_use_last,
                        ..Default::default()
                    });

                    self.get_macro(name).map(|m| {
                        for child in &m.children {
                            child.render(&mut ctx)
                        }
                        ctx.flush().0.into_iter().fold(String::new(), |mut s, f| {
                            f.write_buf(&mut s, BufWriteFormat::Plain).unwrap();
                            s.to_lowercase()
                        })
                    })
                };

                let a_rendered = render(a, a_idx);
                let b_rendered = render(b, b_idx);

                a_rendered.cmp(&b_rendered)
            }
        };

        if key.sort_direction() == SortDirection::Descending {
            ordering.reverse()
        } else {
            ordering
        }
    }

    /// Sorts the given citation items by the style's sort keys.
    pub fn sort<T: EntryLike>(
        &self,
        cites: &mut [CitationItem<T>],
        sort: Option<&Sort>,
        term_locale: Option<&LocaleCode>,
    ) {
        if let Some(sort) = sort {
            cites.sort_by(|a, b| {
                let mut ordering = Ordering::Equal;
                for key in &sort.keys {
                    ordering = self.cmp_entries(a, 0, b, 0, key, term_locale);
                    if ordering != Ordering::Equal {
                        break;
                    }
                }
                ordering
            });
        }
    }
}
