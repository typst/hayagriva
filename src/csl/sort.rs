use std::cmp::Ordering;

use citationberg::taxonomy::Variable;
use citationberg::{
    DemoteNonDroppingParticle, InheritableNameOptions, LocaleCode, LongShortForm, Sort,
    SortDirection, SortKey,
};

use crate::csl::BufWriteFormat;
use crate::csl::rendering::RenderCsl;

use super::taxonomy::EntryLike;
use super::{CitationItem, InstanceContext, StyleContext};

impl StyleContext<'_> {
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
        let (ordering, empty_value) = match key {
            SortKey::Variable { variable: Variable::Standard(s), .. } => {
                let a = InstanceContext::variable_sort_instance(a, a_idx)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string().to_lowercase());
                let b = InstanceContext::variable_sort_instance(b, b_idx)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string().to_lowercase());

                (a.cmp(&b), a.is_none() || b.is_none())
            }
            SortKey::Variable { variable: Variable::Date(d), .. } => {
                let a = a.entry.resolve_date_variable(*d);
                let b = b.entry.resolve_date_variable(*d);

                match (a, b) {
                    (Some(a), Some(b)) => (a.csl_cmp(&b), false),
                    (Some(_), None) => (Ordering::Greater, true),
                    (None, Some(_)) => (Ordering::Less, true),
                    (None, None) => (Ordering::Equal, true),
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

                (
                    if a.len() < b.len() {
                        Ordering::Less
                    } else if a.len() > b.len() {
                        Ordering::Greater
                    } else {
                        Ordering::Equal
                    },
                    false,
                )
            }
            SortKey::Variable { variable: Variable::Number(n), .. } => {
                let a = InstanceContext::variable_sort_instance(a, a_idx)
                    .resolve_number_variable(*n);
                let b = InstanceContext::variable_sort_instance(b, b_idx)
                    .resolve_number_variable(*n);

                match (a, b) {
                    (Some(a), Some(b)) => (a.csl_cmp(&b), false),
                    (Some(_), None) => (Ordering::Greater, true),
                    (None, Some(_)) => (Ordering::Less, true),
                    (None, None) => (Ordering::Equal, true),
                }
            }
            SortKey::Variable { variable: Variable::Page(pv), .. } => {
                let a = InstanceContext::variable_sort_instance(a, a_idx)
                    .resolve_page_variable(*pv);
                let b = InstanceContext::variable_sort_instance(b, b_idx)
                    .resolve_page_variable(*pv);

                match (a, b) {
                    (Some(a), Some(b)) => (a.csl_cmp(&b), false),
                    (Some(_), None) => (Ordering::Greater, true),
                    (None, Some(_)) => (Ordering::Less, true),
                    (None, None) => (Ordering::Equal, true),
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
                    let mut ctx = self.macro_sorting_ctx(
                        entry,
                        idx,
                        entry.locale.as_ref(),
                        term_locale,
                        false,
                    );
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

                (a_rendered.cmp(&b_rendered), false)
            }
        };

        // Per CSL 1.0.2 spec (https://docs.citationstyles.org/en/v1.0.2/specification.html):
        // Entries with empty values are always displayed at the end, even with reversed order.
        // So we always reverse the order if either entry is empty.
        if empty_value || key.sort_direction() == SortDirection::Descending {
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
        citation_number: impl Fn(&T) -> usize,
    ) {
        if let Some(sort) = sort {
            cites.sort_by(|a, b| {
                let mut ordering = Ordering::Equal;
                for key in &sort.keys {
                    ordering = self.cmp_entries(
                        a,
                        citation_number(a.entry),
                        b,
                        citation_number(b.entry),
                        key,
                        term_locale,
                    );
                    if ordering != Ordering::Equal {
                        break;
                    }
                }
                ordering
            });
        }
    }
}
