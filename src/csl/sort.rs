use std::cmp::Ordering;

use citationberg::taxonomy::Variable;
use citationberg::{
    DemoteNonDroppingParticle, InheritableNameOptions, LongShortForm, Sort,
    SortDirection, SortKey,
};

use crate::csl::rendering::RenderCsl;
use crate::csl::taxonomy::{resolve_date_variable, resolve_name_variable};
use crate::csl::BufWriteFormat;

use super::{CitationItem, InstanceContext, StyleContext};

impl<'a> StyleContext<'a> {
    /// Retrieve the ordering of two entries according to the given sort key.
    fn cmp_entries(&self, a: &CitationItem, b: &CitationItem, key: &SortKey) -> Ordering {
        let ordering = match key {
            SortKey::Variable { variable: Variable::Standard(s), .. } => {
                let a = InstanceContext::sort_instance(a)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string());
                let b = InstanceContext::sort_instance(b)
                    .resolve_standard_variable(LongShortForm::default(), *s)
                    .map(|s| s.to_string());

                a.cmp(&b)
            }
            SortKey::Variable { variable: Variable::Date(d), .. } => {
                let a = resolve_date_variable(a.0, *d);
                let b = resolve_date_variable(b.0, *d);

                match (a, b) {
                    (Some(a), Some(b)) => a.csl_cmp(b),
                    (Some(_), None) => Ordering::Greater,
                    (None, Some(_)) => Ordering::Less,
                    (None, None) => Ordering::Equal,
                }
            }
            SortKey::Variable { variable: Variable::Name(n), .. } => {
                let a = resolve_name_variable(a.0, *n);
                let b = resolve_name_variable(b.0, *n);

                for (a_pers, b_pers) in a.iter().zip(b.iter()) {
                    let ord = a_pers.csl_cmp(
                        b_pers,
                        LongShortForm::Long,
                        self.settings.demote_non_dropping_particle
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
                let a = InstanceContext::sort_instance(a).resolve_number_variable(*n);
                let b = InstanceContext::sort_instance(b).resolve_number_variable(*n);

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
                let render = |entry: &CitationItem| {
                    let mut ctx = self.sorting_ctx(entry);
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
                            s
                        })
                    })
                };

                let a_rendered = render(a);
                let b_rendered = render(b);

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
    pub fn sort(&self, cites: &mut [CitationItem], sort: Option<&Sort>) {
        if let Some(sort) = sort {
            cites.sort_by(|a, b| {
                let mut ordering = Ordering::Equal;
                for key in &sort.keys {
                    ordering = self.cmp_entries(a, b, key);
                    if ordering != Ordering::Equal {
                        break;
                    }
                }
                ordering
            });
        }
    }
}
