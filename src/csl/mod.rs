use std::any::Any;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::hash_map::Entry as HmEntry;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Write};
use std::hash::Hash;
use std::num::{NonZeroI16, NonZeroUsize};
use std::sync::Arc;
use std::{mem, vec};

use citationberg::taxonomy::{
    DateVariable, Locator, NameVariable, NumberOrPageVariable, NumberVariable, OtherTerm,
    PageVariable, StandardVariable, Term, Variable,
};
use citationberg::{
    Affixes, BaseLanguage, Citation, CitationFormat, Collapse, CslMacro, Display,
    GrammarGender, IndependentStyle, InheritableNameOptions, Layout,
    LayoutRenderingElement, Locale, LocaleCode, Names, SecondFieldAlign, StyleCategory,
    StyleClass, TermForm, ToAffixes, ToFormatting, taxonomy as csl_taxonomy,
};
use citationberg::{DateForm, LongShortForm, OrdinalLookup, TextCase};
use indexmap::IndexSet;

use crate::csl::elem::{NonEmptyStack, simplify_children};
use crate::csl::rendering::RenderCsl;
use crate::csl::rendering::names::NameDisambiguationProperties;
use crate::csl::taxonomy::NumberOrPageVariableResult;
use crate::lang::CaseFolder;
use crate::types::{ChunkKind, ChunkedString, Date, MaybeTyped, Person};

use self::elem::last_text_mut_child;
pub use self::elem::{
    BufWriteFormat, Elem, ElemChild, ElemChildren, ElemMeta, Formatted, Formatting,
};
use self::taxonomy::{EntryLike, NumberVariableResult, PageVariableResult};

#[cfg(feature = "archive")]
pub mod archive;
mod citation_label;
mod elem;
mod rendering;
mod sort;
mod taxonomy;

/// This struct formats a set of citations according to a style.
#[derive(Debug)]
pub struct BibliographyDriver<'a, T: EntryLike> {
    /// The citations we have seen so far.
    citations: Vec<CitationRequest<'a, T>>,
    /// Offset to apply to the citation number (for numeric styles).
    /// Useful when managing several bibliographies.
    pub citation_number_offset: Option<usize>,
}

impl<T: EntryLike> Default for BibliographyDriver<'_, T> {
    fn default() -> Self {
        Self { 
            citations: Vec::new(),
            citation_number_offset: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct SpeculativeItemRender<'a, T: EntryLike> {
    rendered: ElemChildren,
    entry: &'a T,
    cite_props: CiteProperties<'a>,
    checked_disambiguate: bool,
    first_name: Option<NameDisambiguationProperties>,
    delim_override: Option<&'a str>,
    group_idx: Option<usize>,
    locator: Option<SpecificLocator<'a>>,
    hidden: bool,
    locale: Option<LocaleCode>,
    purpose: Option<CitePurpose>,
    collapse_verdict: Option<CollapseVerdict>,
}

#[derive(Debug, PartialEq)]
struct SpeculativeCiteRender<'a, 'b, T: EntryLike> {
    items: Vec<SpeculativeItemRender<'a, T>>,
    request: &'b CitationRequest<'a, T>,
}

impl<'a, T: EntryLike> BibliographyDriver<'a, T> {
    /// Create a new bibliography driver.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new citation with the given items.
    pub fn citation(&mut self, mut req: CitationRequest<'a, T>) {
        for (i, item) in req.items.iter_mut().enumerate() {
            item.initial_idx = i;
        }
        self.citations.push(req);
    }
}

/// Implementations for finishing the bibliography.
impl<T: EntryLike + Hash + PartialEq + Eq + Debug> BibliographyDriver<'_, T> {
    /// Render the bibliography.
    pub fn finish(mut self, request: BibliographyRequest<'_>) -> Rendered {
        // 1.  Assign citation numbers by bibliography ordering or by citation
        //     order and render them a first time without their locators.
        let bib_style = request.style();

        // Only remember each entry once, even if it is cited multiple times.
        let mut entry_set = IndexSet::new();
        for req in self.citations.iter() {
            for item in req.items.iter() {
                entry_set.insert(item.entry);
            }
        }

        let mut entries: Vec<_> =
            entry_set.into_iter().map(CitationItem::with_entry).collect();
        bib_style.sort(
            &mut entries,
            bib_style.csl.bibliography.as_ref().and_then(|b| b.sort.as_ref()),
            request.locale.as_ref(),
            |_| 0,
        );
        let citation_number = |item: &T| {
            entries.iter().position(|e| e.entry == item).expect("entry not found")
        };

        let mut seen: HashSet<*const T> = HashSet::new();
        let mut res: Vec<SpeculativeCiteRender<T>> = Vec::new();
        let mut last_cite: Option<&CitationItem<T>> = None;

        for citation in &mut self.citations {
            let style = citation.style();

            style.sort(
                &mut citation.items,
                style.csl.citation.sort.as_ref(),
                citation.locale.as_ref(),
                citation_number,
            );

            let items = &citation.items;
            let mut renders: Vec<SpeculativeItemRender<'_, T>> = Vec::new();

            for item in items.iter() {
                let entry = &item.entry;

                let is_near_note = citation.note_number.is_some_and(|_| {
                    res.iter()
                        .rev()
                        .take(style.csl.citation.near_note_distance as usize)
                        .any(|cite| {
                            cite.request.note_number.is_some()
                                && cite.items.iter().any(|item| &item.entry == entry)
                        })
                });

                let first_note_number = citation.note_number.map(|n| {
                    res.iter()
                        .find_map(|cite| {
                            cite.request.note_number.filter(|_| {
                                cite.items.iter().any(|item| &item.entry == entry)
                            })
                        })
                        .unwrap_or(n)
                });

                let mut cite_props = CiteProperties {
                    certain: CertainCiteProperties {
                        note_number: citation.note_number,
                        first_note_number,
                        is_near_note,
                        is_first: seen.insert(*entry),
                        initial_idx: item.initial_idx,
                    },
                    speculative: SpeculativeCiteProperties::speculate(
                        None,
                        citation_number(entry),
                        IbidState::with_last(item, last_cite),
                    ),
                };

                let ctx = style.do_citation(
                    *entry,
                    cite_props.clone(),
                    item.locale.as_ref(),
                    citation.locale.as_ref(),
                    item.purpose,
                    None,
                );

                // Copy the identifier usage from the context. Assume it does
                // not change throughout disambiguation.
                cite_props.speculative.identifier_usage =
                    ctx.instance.identifier_usage.take();

                renders.push(SpeculativeItemRender {
                    entry,
                    cite_props,
                    checked_disambiguate: ctx.writing.checked_disambiguate,
                    first_name: ctx.writing.first_name.clone(),
                    delim_override: None,
                    group_idx: None,
                    locator: item.locator.clone(),
                    rendered: ctx.flush(),
                    hidden: item.hidden,
                    locale: item.locale.clone(),
                    purpose: item.purpose,
                    collapse_verdict: None,
                });

                last_cite = Some(item);
            }

            res.push(SpeculativeCiteRender { items: renders, request: citation });
        }

        // 2.  Disambiguate the citations.
        //
        // If we have set the disambiguation state for an item, we need to set
        // the same state for all entries referencing that item.
        for _ in 0..16 {
            let ambiguous = find_ambiguous_sets(&res);
            if ambiguous.is_empty() {
                break;
            }

            let mut rerender: HashMap<*const T, DisambiguateState> = HashMap::new();
            let mark = |map: &mut HashMap<*const T, DisambiguateState>,
                        entry: &T,
                        state: DisambiguateState| {
                map.entry(entry)
                    .and_modify(|e| *e = e.clone().max(state.clone()))
                    .or_insert(state);
            };

            for group in ambiguous.iter() {
                // 2a. Name Disambiguation loop
                disambiguate_names(&res, group, |entry, state| {
                    mark(&mut rerender, entry, state)
                });

                // Do not try other methods if the previous method succeeded.
                if !rerender.is_empty() {
                    continue;
                }

                // 2b. Disambiguate by allowing `cs:choose` disambiguation.
                disambiguate_with_choose(&res, group, |entry, state| {
                    mark(&mut rerender, entry, state)
                });

                if !rerender.is_empty() {
                    continue;
                }

                // 2c. Disambiguate by year-suffix.
                disambiguate_year_suffix(&res, group, |entry, state| {
                    mark(&mut rerender, entry, state)
                });
            }

            if rerender.is_empty() {
                break;
            }

            for cite in res.iter_mut() {
                let style_ctx = cite.request.style();
                for item in cite.items.iter_mut() {
                    if let Some(state) = rerender.get(&(item.entry as _)) {
                        item.cite_props.speculative.disambiguation = state.clone();
                        item.rendered = do_rerender(&style_ctx, item, cite.request);
                    }
                }
            }
        }

        // 3. Group adjacent citations.
        for cite in res.iter_mut() {
            // This map contains the last index of each entry with this names
            // elem.
            let mut map: HashMap<String, usize> = HashMap::new();
            let mut group_idx = 0;

            for i in 0..cite.items.len() {
                let Some(delim) =
                    cite.request.style.citation.cite_group_delimiter.as_deref().or_else(
                        || {
                            cite.request
                                .style
                                .citation
                                .layout
                                .delimiter
                                .as_deref()
                                .filter(|_| {
                                    cite.request.style.citation.collapse.is_some()
                                })
                        },
                    )
                else {
                    continue;
                };

                let Some(name_elem) = cite.items[i]
                    .rendered
                    .find_meta(ElemMeta::Names)
                    .map(|e| format!("{e:?}"))
                else {
                    continue;
                };

                let mut prev = None;
                let target = *map
                    .entry(name_elem)
                    .and_modify(|i| {
                        prev = Some(*i);
                        *i += 1
                    })
                    .or_insert_with(|| {
                        group_idx += 1;
                        i
                    });

                let mut pos = i;
                while target < pos {
                    cite.items.swap(pos, pos - 1);
                    pos -= 1;
                }

                cite.items[target].delim_override = Some(delim);
                cite.items[target].group_idx = Some(group_idx);
                if let Some(prev) = prev {
                    cite.items[prev].delim_override = None;
                }
            }
        }

        // 4. Render citations with locator.
        // 4a. Make final calls on all [`SpeculativeCiteProperties`].
        //     - Re-check for ibid.
        for i in 0..res.len() {
            for j in 0..res[i].items.len() {
                // TODO filter is not hidden
                let last = if j == 0 && i == 0 {
                    None
                } else if j == 0 {
                    res[i - 1].items.last()
                } else {
                    Some(&res[i].items[j - 1])
                }
                .map(|l| CitationItem::with_locator(l.entry, l.locator.clone()));

                res[i].items[j].cite_props.speculative.ibid = IbidState::with_last(
                    &CitationItem::with_locator(
                        res[i].items[j].entry,
                        res[i].items[j].locator.clone(),
                    ),
                    last.as_ref(),
                );

                //     - Add final locator
                res[i].items[j].cite_props.speculative.locator =
                    res[i].items[j].locator.clone();
            }
        }

        //     - Determine final citation number if bibliography does not sort.
        if bib_style
            .csl
            .bibliography
            .as_ref()
            .and_then(|b| b.sort.as_ref())
            .is_none()
        {
            let mut seen: HashMap<*const T, usize> = HashMap::new();
            let mut start = 0;
            for cite in res.iter_mut() {
                for item in cite.items.iter_mut() {
                    item.cite_props.speculative.citation_number =
                        *seen.entry(item.entry as _).or_insert_with(|| {
                            let num = start;
                            start += 1;
                            num
                        });
                }
            }
        }

        // If asked, offset the citation numbers
        if let Some(citation_number_offset) = self.citation_number_offset {
            for cite in res.iter_mut() {
                for item in cite.items.iter_mut() {
                    item.cite_props.speculative.citation_number += citation_number_offset;
                }
            }
        }

        // Rerender.
        let mut final_citations: Vec<RenderedCitation> = Vec::new();
        for cite in res.iter_mut() {
            let style_ctx = cite.request.style();
            // 5. Collapse grouped citations.
            if cite.request.items.iter().all(|c| c.purpose.is_none()) {
                collapse_items(cite);
            }

            for item in cite.items.iter_mut() {
                item.rendered = last_purpose_render(&style_ctx, item, cite.request);
            }

            // 6. Add affixes.
            let formatting = Formatting::default()
                .apply(cite.request.style.citation.layout.to_formatting());

            final_citations.push(RenderedCitation {
                note_number: cite.request.note_number,
                citation: if cite.items.iter().all(|i| i.hidden) {
                    ElemChildren::new()
                } else {
                    let mut elem_children: Vec<ElemChild> = Vec::new();

                    if let Some(prefix) = cite.request.prefix() {
                        elem_children.push(ElemChild::Text(Formatted {
                            text: prefix.to_string(),
                            formatting,
                        }));
                    }

                    for (i, item) in cite.items.iter().enumerate() {
                        if item.hidden {
                            continue;
                        }

                        if i != 0
                            && let Some(delim) = cite
                                .items
                                .get(i)
                                .and_then(|i: &SpeculativeItemRender<T>| i.delim_override)
                                .or(cite
                                    .request
                                    .style
                                    .citation
                                    .layout
                                    .delimiter
                                    .as_deref())
                        {
                            elem_children.push(ElemChild::Text(Formatted {
                                text: delim.to_string(),
                                formatting,
                            }));
                        }

                        elem_children.push(ElemChild::Elem(Elem {
                            children: item.rendered.clone(),
                            display: None,
                            meta: Some(ElemMeta::Entry(
                                item.cite_props.certain.initial_idx,
                            )),
                        }));
                    }

                    if let Some(suffix) = cite.request.suffix() {
                        let print = last_text_mut_child(&mut elem_children)
                            .is_none_or(|t| !t.text.ends_with(suffix));
                        if print {
                            elem_children.push(
                                Formatted { text: suffix.to_string(), formatting }.into(),
                            );
                        }
                    }

                    simplify_children(ElemChildren(elem_children))
                },
            })
        }

        let bib_render = if let Some(bibliography) = &request.style.bibliography {
            let mut items = Vec::new();
            for entry in entries.into_iter() {
                let cited_item = res
                    .iter()
                    .flat_map(|cite| cite.items.iter())
                    .find(|item| item.entry == entry.entry)
                    .unwrap();

                items.push((
                    simplify_children(
                        bib_style
                            .bibliography(
                                entry.entry,
                                CiteProperties {
                                    certain: cited_item.cite_props.certain,
                                    speculative: cited_item
                                        .cite_props
                                        .speculative
                                        .for_bibliography(),
                                },
                                cited_item.locale.as_ref(),
                                request.locale.as_ref(),
                            )
                            .unwrap(),
                    ),
                    entry.entry.key().to_string(),
                ))
            }

            Some(RenderedBibliography {
                hanging_indent: bibliography.hanging_indent,
                second_field_align: bibliography.second_field_align,
                line_spacing: bibliography.line_spacing,
                entry_spacing: bibliography.entry_spacing,
                items: items
                    .into_iter()
                    .map(|(mut i, key)| {
                        if bibliography.second_field_align.is_some() {
                            BibliographyItem::new(key, i.remove_any_meta(), i)
                        } else {
                            BibliographyItem::new(key, None, i)
                        }
                    })
                    .collect(),
            })
        } else {
            None
        };

        Rendered {
            bibliography: bib_render,
            citations: final_citations,
        }
    }
}

/// Create a new citation with the given items. Bibliography-wide disambiguation
/// and some other features will not be applied.
pub fn standalone_citation<T: EntryLike>(
    mut req: CitationRequest<'_, T>,
) -> ElemChildren {
    let style = req.style();
    style.sort(
        &mut req.items,
        style.csl.citation.sort.as_ref(),
        req.locale.as_ref(),
        |_| 0,
    );
    let mut res = vec![];
    let mut all_hidden = true;
    for item in req.items {
        if item.hidden {
            continue;
        } else {
            all_hidden = false;
        }

        res.push(if let Some(CitePurpose::Year) = item.purpose {
            date_replacement(
                &style,
                item.entry,
                &CiteProperties::for_sorting(item.locator, 0),
                req.locale.as_ref(),
                item.locale.as_ref(),
            )
        } else {
            style.citation(
                item.entry,
                CiteProperties::for_sorting(item.locator, 0),
                item.locale.as_ref(),
                req.locale.as_ref(),
                item.purpose,
                None,
            )
        });
    }

    let non_empty: Vec<_> = res.into_iter().filter(|c| c.has_content()).collect();

    let formatting =
        Formatting::default().apply(style.csl.citation.layout.to_formatting());

    if !non_empty.is_empty() && !all_hidden {
        let mut res = if let Some(prefix) = style.csl.citation.layout.prefix.as_ref() {
            ElemChildren(vec![Formatted { text: prefix.clone(), formatting }.into()])
        } else {
            ElemChildren::new()
        };

        for (i, elem_children) in non_empty.into_iter().enumerate() {
            let first = i == 0;
            if !first {
                res.0.push(
                    Formatted {
                        text: style
                            .csl
                            .citation
                            .layout
                            .delimiter
                            .as_deref()
                            .unwrap_or(Citation::DEFAULT_CITE_GROUP_DELIMITER)
                            .to_string(),
                        formatting,
                    }
                    .into(),
                );
            }

            res.0.extend(elem_children.0)
        }

        if let Some(suffix) = style.csl.citation.layout.suffix.as_ref() {
            let print = res.last_text().is_none_or(|t| !t.text.ends_with(suffix));
            if print {
                res.0.push(Formatted { text: suffix.clone(), formatting }.into());
            }
        }

        simplify_children(res)
    } else {
        ElemChildren::new()
    }
}

fn do_rerender<T: EntryLike>(
    ctx: &StyleContext<'_>,
    item: &SpeculativeItemRender<T>,
    request: &CitationRequest<'_, T>,
) -> ElemChildren {
    ctx.citation(
        item.entry,
        item.cite_props.clone(),
        item.locale.as_ref(),
        request.locale.as_ref(),
        item.purpose,
        item.collapse_verdict,
    )
}

fn date_replacement<T: EntryLike>(
    ctx: &StyleContext<'_>,
    entry: &T,
    cite_props: &CiteProperties,
    term_locale: Option<&LocaleCode>,
    locale: Option<&LocaleCode>,
) -> ElemChildren {
    let date = entry
        .resolve_date_variable(DateVariable::Issued)
        .or_else(|| entry.resolve_date_variable(DateVariable::EventDate))
        .or_else(|| entry.resolve_date_variable(DateVariable::Submitted))
        .or_else(|| entry.resolve_date_variable(DateVariable::OriginalDate));

    ElemChildren(vec![ElemChild::Text(Formatted {
        text: if let Some(date) = date {
            let mut s = String::with_capacity(4);
            let c = ctx.ctx(entry, cite_props.clone(), locale, term_locale, false);
            let ad = c
                .term(Term::Other(OtherTerm::Ad), TermForm::default(), false)
                .unwrap_or(" AD");
            let bc = c
                .term(Term::Other(OtherTerm::Bc), TermForm::default(), false)
                .unwrap_or(" BC");
            write_year(date.year, false, &mut s, ad, bc).unwrap();
            s
        } else if let Some(no_date) = ctx
            .ctx(entry, cite_props.clone(), locale, term_locale, false)
            .term(Term::Other(OtherTerm::NoDate), TermForm::default(), false)
        {
            no_date.to_string()
        } else {
            "n.d.".to_string()
        },
        formatting: Formatting::default(),
    })])
}

pub fn write_year<W: std::fmt::Write>(
    year: i32,
    short: bool,
    w: &mut W,
    ad: &str,
    bc: &str,
) -> std::fmt::Result {
    if short && year >= 1000 {
        return write!(w, "{:02}", year % 100);
    }

    write!(
        w,
        "{}{}",
        if year > 0 { year } else { year.abs() + 1 },
        if year < 1000 {
            if year <= 0 {
                bc
            } else {
                // AD is used as a postfix, see
                // https://docs.citationstyles.org/en/stable/specification.html?#ad-and-bc
                ad
            }
        } else {
            ""
        }
    )
}

fn last_purpose_render<T: EntryLike + Debug>(
    ctx: &StyleContext<'_>,
    item: &SpeculativeItemRender<T>,
    request: &CitationRequest<'_, T>,
) -> ElemChildren {
    if let Some(CitePurpose::Year) = item.purpose {
        date_replacement(
            ctx,
            item.entry,
            &item.cite_props,
            request.locale.as_ref(),
            item.locale.as_ref(),
        )
    } else {
        do_rerender(ctx, item, request)
    }
}

type AmbiguousGroup = Vec<(usize, usize)>;

/// Progressively transform names to disambiguate them.
fn disambiguate_names<F, T>(
    renders: &[SpeculativeCiteRender<'_, '_, T>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    T: EntryLike,
    F: FnMut(&T, DisambiguateState),
{
    for &(cite_idx, item_idx) in group.iter() {
        let style = renders[cite_idx].request.style;
        let item = &renders[cite_idx].items[item_idx];

        if !item.cite_props.speculative.disambiguation.may_disambiguate_names() {
            continue;
        }

        let name_props_slot = if let DisambiguateState::NameDisambiguation(n) =
            &item.cite_props.speculative.disambiguation
        {
            Some(n)
        } else {
            item.first_name.as_ref()
        };

        if let Some(name_props) = name_props_slot {
            let mut name_props = name_props.clone();
            if name_props.disambiguate(
                style.citation.disambiguate_add_givenname,
                style.citation.givenname_disambiguation_rule,
                style.citation.disambiguate_add_names,
            ) {
                mark(item.entry, DisambiguateState::NameDisambiguation(name_props))
            }
        }
    }
}

/// Mark qualifying entries for disambiguation with `cs:choose`.
fn disambiguate_with_choose<F, T>(
    renders: &[SpeculativeCiteRender<'_, '_, T>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    T: EntryLike,
    F: FnMut(&T, DisambiguateState),
{
    if group.iter().any(|&(cite_idx, item_idx)| {
        renders[cite_idx].items[item_idx].checked_disambiguate
            && renders[cite_idx].items[item_idx]
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_choose()
    }) {
        // Do not set this for the first qualifying entry.
        let mut armed = false;

        for &(cite_idx, item_idx) in group.iter() {
            let item = &renders[cite_idx].items[item_idx];
            if item.checked_disambiguate {
                if armed {
                    mark(item.entry, DisambiguateState::Choose);
                } else {
                    armed = true;
                }
            }
        }
    }
}

/// Mark qualifying entries for disambiguation with year suffixes.
fn disambiguate_year_suffix<F, T>(
    renders: &[SpeculativeCiteRender<'_, '_, T>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    T: EntryLike + PartialEq,
    F: FnMut(&T, DisambiguateState),
{
    if renders.iter().flat_map(|r| r.items.iter()).any(|i| {
        let entry_has_date = i
            .entry
            .resolve_date_variable(DateVariable::Issued)
            .or_else(|| i.entry.resolve_date_variable(DateVariable::Accessed))
            .or_else(|| i.entry.resolve_date_variable(DateVariable::AvailableDate))
            .or_else(|| i.entry.resolve_date_variable(DateVariable::EventDate))
            .or_else(|| i.entry.resolve_date_variable(DateVariable::Submitted))
            .or_else(|| i.entry.resolve_date_variable(DateVariable::OriginalDate))
            .is_some();

        i.rendered
            .find_elem_by(&|e| {
                // The citation label will contain the date if there is one.
                e.meta == Some(ElemMeta::Date)
                    || (entry_has_date && e.meta == Some(ElemMeta::CitationLabel))
            })
            .is_some()
    }) && group.iter().any(|&(cite_idx, item_idx)| {
        renders[cite_idx].request.style.citation.disambiguate_add_year_suffix
            && renders[cite_idx].items[item_idx]
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
    }) {
        let mut entries = Vec::new();
        for &(cite_idx, item_idx) in group.iter() {
            let item = &renders[cite_idx].items[item_idx];
            if item
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
                && !entries.contains(&item.entry)
            {
                entries.push(item.entry);
            }
        }

        // Assign year suffixes.
        for (i, entry) in entries.into_iter().enumerate() {
            mark(entry, DisambiguateState::YearSuffix(i as u8));
        }
    }
}

/// Return a vector that contains every group of mutually ambiguous items with
/// cite and item index.
fn find_ambiguous_sets<T: EntryLike + PartialEq>(
    cites: &[SpeculativeCiteRender<'_, '_, T>],
) -> Vec<AmbiguousGroup> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum PotentialDisambiguation {
        /// Two usizes for an item that produced this string.
        Single((usize, usize)),
        /// There were multiple matches. This is the index in the result vector.
        Match(usize),
    }

    let mut map: HashMap<String, PotentialDisambiguation> = HashMap::new();
    let mut res: Vec<Vec<(usize, usize)>> = Vec::new();

    for (i, cite) in cites.iter().enumerate() {
        for (j, item) in cite.items.iter().enumerate() {
            if !item
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
            {
                // There is nothing we can do.
                continue;
            }

            let buf = format!("{:?}", item.rendered);
            match map.entry(buf) {
                HmEntry::Occupied(entry) => match *entry.get() {
                    PotentialDisambiguation::Single(pos) => {
                        *entry.into_mut() = PotentialDisambiguation::Match(res.len());
                        res.push(vec![pos, (i, j)]);
                    }
                    PotentialDisambiguation::Match(idx) => {
                        res[idx].push((i, j));
                    }
                },
                HmEntry::Vacant(vacant) => {
                    vacant.insert(PotentialDisambiguation::Single((i, j)));
                }
            }
        }
    }

    // Only return items if not every item points to the same entry.
    res.into_iter()
        .filter(|e| {
            let Some(first_entry) =
                e.first().map(|(cite, item)| cites[*cite].items[*item].entry)
            else {
                return false;
            };

            e.iter()
                .any(|(cite, item)| cites[*cite].items[*item].entry != first_entry)
        })
        .collect()
}

fn collapse_items<'a, T: EntryLike>(cite: &mut SpeculativeCiteRender<'a, '_, T>) {
    let style = &cite.request.style;

    let after_collapse_delim = style
        .citation
        .after_collapse_delimiter
        .as_deref()
        .or(style.citation.layout.delimiter.as_deref());

    let group_delimiter = style.citation.cite_group_delimiter.as_deref();
    let year_suffix_delimiter = style.citation.year_suffix_delimiter.as_deref();

    match style.citation.collapse {
        Some(Collapse::CitationNumber) => {
            // Option with the start and end of the range.
            let mut range_start: Option<(usize, usize)> = None;
            let mut just_collapsed = false;

            let end_range = |items: &mut [SpeculativeItemRender<'a, T>],
                             range_start: &mut Option<(usize, usize)>,
                             just_collapsed: &mut bool| {
                let use_after_collapse_delim = *just_collapsed;
                *just_collapsed = false;

                if let &mut Some((start, end)) = range_start {
                    // If the previous citation range was collapsed, use the
                    // after-collapse delimiter before the next item.
                    if use_after_collapse_delim {
                        items[start].delim_override = after_collapse_delim;
                    }

                    // There should be at least three items in the range to
                    // collapse.
                    if start + 1 < end {
                        items[end].delim_override = Some("–");

                        for item in &mut items[start + 1..end] {
                            item.hidden = true;
                        }

                        *just_collapsed = true;
                    }
                }

                *range_start = None;
            };

            for i in 0..cite.items.len() {
                let citation_number = {
                    // Item must be borrowed in this block only because it
                    // cannot be mutably borrowed below otherwise.
                    let item = &cite.items[i];
                    if item.hidden
                        || item.rendered.find_meta(ElemMeta::CitationNumber).is_none()
                    {
                        end_range(&mut cite.items, &mut range_start, &mut just_collapsed);
                        continue;
                    }

                    item.cite_props.speculative.citation_number
                };

                let prev_citation_number = match range_start {
                    Some((_, end)) => {
                        Some(cite.items[end].cite_props.speculative.citation_number)
                    }
                    None => None,
                };

                match (range_start, prev_citation_number) {
                    (Some((start, end)), Some(prev_citation_number))
                        if end + 1 == i
                            && prev_citation_number + 1 == citation_number =>
                    {
                        // Extend the range.
                        range_start = Some((start, i));
                    }
                    _ => {
                        end_range(&mut cite.items, &mut range_start, &mut just_collapsed);
                        range_start = Some((i, i));
                    }
                }
            }

            end_range(&mut cite.items, &mut range_start, &mut just_collapsed);
        }
        Some(
            c @ (Collapse::Year | Collapse::YearSuffix | Collapse::YearSuffixRanged),
        ) => {
            // Index of where the current group started and the group we are
            // currently in.
            let mut group_idx: Option<(usize, usize)> = None;
            for i in 0..cite.items.len() {
                match group_idx {
                    // This is our group.
                    Some((_, idx)) if Some(idx) == cite.items[i].group_idx => {
                        if c == Collapse::YearSuffix
                            && let DisambiguateState::YearSuffix(s) =
                                cite.items[i].cite_props.speculative.disambiguation
                            && s > 0
                        {
                            // We want to collapse the years up to the suffix
                            // and the current item has a year suffix greater
                            // than zero, which means it follows another item
                            // with the same year. In this case, we collapse
                            // the year suffix.
                            cite.items[i].delim_override = year_suffix_delimiter;
                            cite.items[i].collapse_verdict =
                                Some(CollapseVerdict::YearSuffix);
                            continue;
                        }
                        // FIXME: Retains delimiter in names.
                        cite.items[i].delim_override = group_delimiter;
                        cite.items[i].collapse_verdict = Some(CollapseVerdict::First);
                    }
                    // This is a different group.
                    Some((start, _)) if start + 1 < i => {
                        cite.items[i].delim_override = after_collapse_delim;
                        group_idx = cite.items[i].group_idx.map(|idx| (i, idx));
                    }
                    // We are at the beginning.
                    _ => {
                        group_idx = cite.items[i].group_idx.map(|idx| (i, idx));
                    }
                }
            }

            // TODO: Year ranged.
        }
        None => {}
    }
}

/// What we have decided for rerendering this item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum CollapseVerdict {
    /// Only the first date should be printed.
    First,
    /// Only the year suffix should be printed.
    YearSuffix,
}

/// The result of [`BibliographyDriver::finish`].
#[derive(Debug, Clone)]
pub struct Rendered {
    /// The bibliography items.
    pub bibliography: Option<RenderedBibliography>,
    /// The citation items.
    pub citations: Vec<RenderedCitation>,
}

/// A fully rendered bibliography.
#[derive(Debug, Clone)]
pub struct RenderedBibliography {
    /// Render the bibliography in a hanging indent.
    pub hanging_indent: bool,
    /// When set, the second field is aligned.
    pub second_field_align: Option<SecondFieldAlign>,
    /// The line spacing within the bibliography as a multiple of regular line
    /// spacing.
    pub line_spacing: NonZeroI16,
    /// Extra space between entries as a multiple of line height.
    pub entry_spacing: i16,
    /// The bibliography items. The first item may be some if
    /// `second_field_align` is set. Then, it is the first field that must be
    /// treated specially.
    pub items: Vec<BibliographyItem>,
}

/// A fully rendered bibliography item.
#[derive(Debug, Clone)]
pub struct BibliographyItem {
    /// The item's key as specified in the bibliography.
    pub key: String,
    /// The item's first field. Only available when required by the
    /// `second-field-align` CSL property.
    pub first_field: Option<ElemChild>,
    /// The rendered item.
    pub content: ElemChildren,
}

impl BibliographyItem {
    fn new(key: String, first_field: Option<ElemChild>, content: ElemChildren) -> Self {
        Self { key, first_field, content }
    }
}

/// A fully rendered citation.
#[derive(Debug, Clone)]
pub struct RenderedCitation {
    /// The footnote number for this citation.
    pub note_number: Option<usize>,
    /// The citation.
    pub citation: ElemChildren,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sorting {
    Variable,
    Macro,
}

/// A context that contains all information related to rendering a single entry.
#[derive(Debug, Clone, PartialEq)]
struct InstanceContext<'a, T: EntryLike> {
    // Entry-dependent data.
    /// The current entry.
    pub entry: &'a T,
    /// The position of this citation in the list of citations.
    pub cite_props: CiteProperties<'a>,
    /// Whether we are sorting or formatting right now and if we are sorting with a variable or macro sort key.
    pub sorting: Option<Sorting>,
    /// The locale for the content in the entry.
    pub locale: Option<&'a LocaleCode>,
    /// The locale for the terms.
    pub term_locale: Option<&'a LocaleCode>,
    /// Whether this citation should respect a special form.
    pub kind: Option<SpecialForm>,
    /// Which labels were written.
    pub identifier_usage: RefCell<IdentifierUsage>,
}

impl<'a, T: EntryLike> InstanceContext<'a, T> {
    fn new(
        entry: &'a T,
        cite_props: CiteProperties<'a>,
        sorting: Option<Sorting>,
        locale: Option<&'a LocaleCode>,
        term_locale: Option<&'a LocaleCode>,
        kind: Option<SpecialForm>,
    ) -> Self {
        Self {
            entry,
            sorting,
            locale,
            term_locale,
            kind,
            identifier_usage: RefCell::new(cite_props.speculative.identifier_usage),
            cite_props,
        }
    }

    fn variable_sort_instance(item: &CitationItem<'a, T>, idx: usize) -> Self {
        Self::new(
            item.entry,
            CiteProperties::for_sorting(item.locator.clone(), idx),
            Some(Sorting::Variable),
            None,
            None,
            None,
        )
    }
}

/// A context that contains information about the style we are using to render.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StyleContext<'a> {
    // Settings from the style.
    /// The settings of the style.
    pub csl: &'a IndependentStyle,
    /// A list of locales defined in their respective locale file.
    locale_files: &'a [Locale],
    /// Which locale we're using.
    locale_override: Option<LocaleCode>,
}

impl<'a> StyleContext<'a> {
    fn new(
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
    ) -> Self {
        Self { csl: style, locale_files, locale_override: locale }
    }

    fn ctx<'b, T: EntryLike>(
        &'b self,
        entry: &'b T,
        cite_props: CiteProperties<'a>,
        locale: Option<&'b LocaleCode>,
        term_locale: Option<&'b LocaleCode>,
        bibliography: bool,
    ) -> Context<'b, T> {
        Context {
            instance: InstanceContext::new(
                entry,
                cite_props,
                None,
                locale,
                term_locale,
                None,
            ),
            style: self,
            writing: WritingContext::new(self.csl.settings.options.clone()),
            bibliography,
        }
    }

    fn macro_sorting_ctx<'b, T: EntryLike>(
        &'b self,
        item: &'b CitationItem<'b, T>,
        idx: usize,
        locale: Option<&'b LocaleCode>,
        term_locale: Option<&'b LocaleCode>,
        bibliography: bool,
    ) -> Context<'b, T> {
        Context {
            instance: InstanceContext::new(
                item.entry,
                CiteProperties::for_sorting(item.locator.clone(), idx),
                Some(Sorting::Macro),
                locale,
                term_locale,
                None,
            ),
            style: self,
            writing: WritingContext::new(self.csl.settings.options.clone()),
            bibliography,
        }
    }

    /// Render the given item within a citation.
    fn citation<T: EntryLike>(
        &self,
        entry: &T,
        props: CiteProperties<'a>,
        locale: Option<&LocaleCode>,
        term_locale: Option<&LocaleCode>,
        kind: Option<CitePurpose>,
        collapse_verdict: Option<CollapseVerdict>,
    ) -> ElemChildren {
        let ctx =
            self.do_citation(entry, props, locale, term_locale, kind, collapse_verdict);
        ctx.flush()
    }

    /// Render the given item within a bibliography.
    fn bibliography<T: EntryLike>(
        &self,
        entry: &T,
        props: CiteProperties<'a>,
        locale: Option<&LocaleCode>,
        term_locale: Option<&LocaleCode>,
    ) -> Option<ElemChildren> {
        self.do_bibliography(entry, props, locale, term_locale)
            .map(|ctx| ctx.flush())
    }

    /// Render the given item within a citation.
    fn do_citation<'b, T: EntryLike>(
        &'b self,
        entry: &'b T,
        props: CiteProperties<'a>,
        locale: Option<&'b LocaleCode>,
        term_locale: Option<&'b LocaleCode>,
        kind: Option<CitePurpose>,
        collapse_verdict: Option<CollapseVerdict>,
    ) -> Context<'b, T> {
        let mut ctx = self.ctx(entry, props, locale, term_locale, false);

        let do_regular = |ctx: &mut Context<'b, T>| {
            let reset = if ctx.instance.kind.is_none() {
                match collapse_verdict {
                    Some(CollapseVerdict::First) => {
                        ctx.set_special_form(Some(SpecialForm::OnlyFirstDate));
                        true
                    }
                    Some(CollapseVerdict::YearSuffix) => {
                        ctx.set_special_form(Some(SpecialForm::OnlyYearSuffix));
                        true
                    }
                    None => false,
                }
            } else {
                false
            };

            ctx.writing.push_name_options(&self.csl.citation.name_options);
            self.csl.citation.layout.render(ctx);
            ctx.writing.pop_name_options();

            if reset {
                ctx.set_special_form(None);
            }
        };

        let do_author = |ctx: &mut Context<'b, T>| {
            let author_var = Variable::Name(
                if entry.resolve_name_variable(NameVariable::Author).is_empty()
                    && !entry.resolve_name_variable(NameVariable::Editor).is_empty()
                {
                    NameVariable::Editor
                } else {
                    NameVariable::Author
                },
            );

            if self.csl.citation.layout.will_render(ctx, author_var) {
                // Render name from citation.
                ctx.set_special_form(Some(SpecialForm::VarOnly(author_var)));
                do_regular(ctx);
            } else {
                let mut needs_bibliography = true;

                // Render name from citation with ibid forced to Different.
                if ctx.instance.cite_props.speculative.ibid != IbidState::Different {
                    let prev_ibid = ctx.instance.cite_props.speculative.ibid;
                    ctx.instance.cite_props.speculative.ibid = IbidState::Different;
                    if self.csl.citation.layout.will_render(ctx, author_var) {
                        ctx.set_special_form(Some(SpecialForm::VarOnly(author_var)));
                        do_regular(ctx);
                        needs_bibliography = false;
                    }
                    ctx.instance.cite_props.speculative.ibid = prev_ibid;
                }

                if needs_bibliography {
                    // Render name from bibliography.
                    ctx.set_special_form(Some(SpecialForm::VarOnly(author_var)));
                    let needs_synthesis =
                        if let Some(bibliography) = &self.csl.bibliography {
                            if bibliography.layout.will_render(ctx, author_var) {
                                ctx.writing.push_name_options(&bibliography.name_options);
                                bibliography.layout.render(ctx);
                                ctx.writing.pop_name_options();
                                false
                            } else {
                                true
                            }
                        } else {
                            true
                        };

                    if needs_synthesis {
                        // We build our own name and render it with the citation's
                        // properties.
                        let layout = Layout::new(
                            vec![LayoutRenderingElement::Names(Names::with_variables(
                                vec![NameVariable::Author],
                            ))],
                            self.csl.citation.layout.to_formatting(),
                            None,
                            None,
                        );
                        ctx.writing.push_name_options(&self.csl.citation.name_options);
                        layout.render(ctx);
                        ctx.writing.pop_name_options();
                    }
                }
            }
            ctx.set_special_form(None);
        };

        match (kind, self.csl.bibliography.as_ref()) {
            (Some(CitePurpose::Author), _) => {
                do_author(&mut ctx);
            }
            (Some(CitePurpose::Full), Some(bib)) => {
                ctx.writing.push_name_options(&bib.name_options);
                bib.layout.render(&mut ctx);
                ctx.writing.pop_name_options();

                if bib.second_field_align.is_some()
                    && let Some(mut t) =
                        ctx.writing.elem_stack.first_mut().remove_any_meta()
                {
                    if let ElemChild::Text(t) = &mut t {
                        t.text.push(' ');
                    } else if let ElemChild::Elem(e) = &mut t
                        && let Some(t) = e.children.last_text_mut()
                    {
                        t.text.push(' ');
                    }

                    ctx.writing.elem_stack.first_mut().0.insert(0, t);
                }
            }
            (Some(CitePurpose::Prose), _) => {
                do_author(&mut ctx);
                if !self
                    .csl
                    .citation
                    .layout
                    .prefix
                    .as_ref()
                    .is_some_and(|f| f.chars().next().is_some_and(char::is_whitespace))
                {
                    ctx.ensure_space();
                }

                if self.csl.info.category.iter().any(|c| {
                    matches!(
                        c,
                        StyleCategory::CitationFormat {
                            format: CitationFormat::Label | CitationFormat::Numeric
                        }
                    )
                }) {
                    // Print the label.
                    if let Some(prefix) = self.csl.citation.layout.prefix.as_ref() {
                        ctx.push_str(prefix);
                    }
                    do_regular(&mut ctx);
                    if let Some(suffix) = self.csl.citation.layout.suffix.as_ref() {
                        ctx.push_str(suffix);
                    }
                } else {
                    // Print the citation surrounded by parentheses and suppress
                    // the author.
                    ctx.push_str(
                        self.csl.citation.layout.prefix.as_deref().unwrap_or("("),
                    );
                    ctx.set_special_form(Some(SpecialForm::SuppressAuthor));
                    do_regular(&mut ctx);
                    ctx.set_special_form(None);
                    ctx.push_str(
                        self.csl.citation.layout.suffix.as_deref().unwrap_or(")"),
                    );
                }
            }
            (Some(CitePurpose::Year) | Some(CitePurpose::Full) | None, _) => {
                do_regular(&mut ctx);
            }
        }
        ctx
    }

    /// Render the given item within a bibliography.
    fn do_bibliography<'b, T: EntryLike>(
        &'b self,
        entry: &'b T,
        props: CiteProperties<'a>,
        locale: Option<&'b LocaleCode>,
        term_locale: Option<&'b LocaleCode>,
    ) -> Option<Context<'b, T>> {
        let mut ctx = self.ctx(entry, props, locale, term_locale, true);
        ctx.writing
            .push_name_options(&self.csl.bibliography.as_ref()?.name_options);

        let layout = &self.csl.bibliography.as_ref()?.layout;
        let affixes = layout.to_affixes();

        let affix_loc = ctx.apply_prefix(&affixes);
        layout.render(&mut ctx);
        ctx.apply_suffix(&affixes, affix_loc);

        Some(ctx)
    }

    /// Return the locale to use for this style.
    fn locale(&self) -> LocaleCode {
        self.locale_override
            .clone()
            .or_else(|| self.csl.default_locale.clone())
            .unwrap_or_else(LocaleCode::en_us)
    }
}

/// A citation request. A citation can contain references to multiple items.
#[derive(Debug, PartialEq)]
pub struct CitationRequest<'a, T: EntryLike> {
    /// The items to format.
    pub items: Vec<CitationItem<'a, T>>,
    /// Which style to use for the citation.
    style: &'a IndependentStyle,
    /// The requested locale for the style's terms.
    ///
    /// This is also the place to store a locale override from a dependent
    /// style.
    pub locale: Option<LocaleCode>,
    /// The files used to retrieve locale settings and terms if the style does
    /// not define all necessary items.
    pub locale_files: &'a [Locale],
    /// The number to use for `first-reference-note-number`.
    ///
    /// `near-note` will always test false if this is none for the referenced
    /// note.
    note_number: Option<usize>,
}

impl<'a, T: EntryLike> CitationRequest<'a, T> {
    /// Create a new citation request.
    pub fn new(
        items: Vec<CitationItem<'a, T>>,
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
        note_number: Option<usize>,
    ) -> Self {
        Self {
            items,
            style,
            locale,
            locale_files,
            note_number: note_number.filter(|_| style.settings.class == StyleClass::Note),
        }
    }

    /// Create a new citation request without a note number.
    pub fn from_items(
        items: Vec<CitationItem<'a, T>>,
        style: &'a IndependentStyle,
        locale_files: &'a [Locale],
    ) -> Self {
        Self::new(items, style, None, locale_files, None)
    }

    fn style(&self) -> StyleContext<'a> {
        StyleContext::new(self.style, self.locale.clone(), self.locale_files)
    }

    fn shall_affix(&self) -> bool {
        self.items.iter().all(|p| p.purpose.is_none())
    }

    fn prefix(&self) -> Option<&'a str> {
        self.style
            .citation
            .layout
            .prefix
            .as_deref()
            .filter(|_| self.shall_affix())
    }

    fn suffix(&self) -> Option<&'a str> {
        self.style
            .citation
            .layout
            .suffix
            .as_deref()
            .filter(|_| self.shall_affix())
    }
}

/// A request to render a bibliography. Use with [`BibliographyDriver::finish`].
#[derive(Debug, PartialEq)]
pub struct BibliographyRequest<'a> {
    /// Which style to use for the bibliography. Some styles do not have a
    /// bibliography, in which case the field of the [`Rendered`] will be
    /// `None`.
    pub style: &'a IndependentStyle,
    /// The requested locale for the style's terms.
    ///
    /// This is also the place to store a locale override from a dependent
    /// style.
    pub locale: Option<LocaleCode>,
    /// The files used to retrieve locale settings and terms if the style does
    /// not define all necessary items.
    pub locale_files: &'a [Locale],
}

impl<'a> BibliographyRequest<'a> {
    /// Create a new bibliography request.
    pub fn new(
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
    ) -> Self {
        Self { style, locale, locale_files }
    }

    fn style(&self) -> StyleContext<'a> {
        StyleContext::new(self.style, self.locale.clone(), self.locale_files)
    }
}

/// A reference to an [`crate::Entry`] within a [`CitationRequest`].
#[derive(Debug, Clone, PartialEq)]
pub struct CitationItem<'a, T: EntryLike> {
    /// The entry to format.
    pub entry: &'a T,
    /// The locator that specifies where in the entry the item is found.
    pub locator: Option<SpecificLocator<'a>>,
    /// A locale code that overrides the assumed locale of the entry. If this is
    /// none, this will default to [`CitationRequest::locale`] and then
    /// [`citationberg::IndependentStyle::default_locale`].
    pub locale: Option<LocaleCode>,
    /// Whether this item will be included in the output.
    pub hidden: bool,
    /// Format the item in a special way.
    pub purpose: Option<CitePurpose>,
    /// The initial index of this item in the list of items.
    initial_idx: usize,
}

impl<'a, T: EntryLike> CitationItem<'a, T> {
    /// Create a new citation item for the given entry.
    pub fn with_entry(entry: &'a T) -> Self {
        Self {
            entry,
            locator: None,
            locale: None,
            hidden: false,
            purpose: None,
            initial_idx: 0,
        }
    }

    /// Create a new citation item with a locator for the given entry.
    pub fn with_locator(entry: &'a T, locator: Option<SpecificLocator<'a>>) -> Self {
        Self {
            entry,
            locator,
            locale: None,
            hidden: false,
            purpose: None,
            initial_idx: 0,
        }
    }

    /// Create a new citation with all fields set.
    pub fn new(
        entry: &'a T,
        locator: Option<SpecificLocator<'a>>,
        locale: Option<LocaleCode>,
        hidden: bool,
        purpose: Option<CitePurpose>,
    ) -> Self {
        Self {
            entry,
            locator,
            locale,
            hidden,
            purpose,
            initial_idx: 0,
        }
    }

    /// Change the `kind` of this item.
    pub fn kind(mut self, purpose: CitePurpose) -> Self {
        self.purpose = Some(purpose);
        self
    }
}

impl<'a> StyleContext<'a> {
    /// Retrieve a macro.
    fn get_macro(&self, name: &str) -> Option<&'a CslMacro> {
        self.csl.macros.iter().find(|m| m.name == name)
    }

    /// Get the locale for the given language in the style.
    fn lookup_locale<F, R>(&self, mut f: F) -> Option<R>
    where
        F: FnMut(&'a Locale) -> Option<R>,
    {
        let mut lookup = |file: &'a [Locale], lang: Option<&LocaleCode>| {
            #[allow(clippy::redundant_closure)]
            file.iter().find(|l| l.lang.as_ref() == lang).and_then(|l| f(l))
        };

        let locale = self.locale();
        let en_us = LocaleCode::en_us();

        for (i, resource) in [self.csl.locale.as_slice(), self.locale_files]
            .into_iter()
            .enumerate()
        {
            let fallback = if i == 0 {
                locale.parse_base().and_then(|base| match base {
                    BaseLanguage::Iso639_1(lang) => {
                        Some(LocaleCode(String::from_utf8(lang.to_vec()).ok()?))
                    }
                    BaseLanguage::Iana(lang) => Some(LocaleCode(lang)),
                    _ => None,
                })
            } else {
                locale.fallback()
            };

            if let Some(output) = lookup(resource, Some(&locale)) {
                return Some(output);
            }

            if fallback.is_some()
                && let Some(output) = lookup(resource, fallback.as_ref())
            {
                return Some(output);
            }

            if i == 0 {
                if let Some(output) = lookup(resource, None) {
                    return Some(output);
                }
            } else if let Some(output) = lookup(resource, Some(&en_us)) {
                return Some(output);
            }
        }

        None
    }

    /// Check whether to do punctuation in quotes.
    fn punctuation_in_quotes(&self) -> bool {
        self.lookup_locale(|f| f.style_options?.punctuation_in_quote)
            .unwrap_or_default()
    }
}

/// This struct contains all information needed to render a single entry. It
/// contains buffers and is mutable.
#[derive(Debug, Clone)]
pub(crate) struct WritingContext {
    // Dynamic settings that change while rendering.
    /// Whether to watch out for punctuation that should be pulled inside the
    /// preceding quoted content.
    pull_punctuation: bool,
    /// Whether we should be using inner quotes.
    inner_quotes: bool,
    /// Whether to strip periods.
    strip_periods: bool,
    /// Whether to add queried variables to the suppression list.
    suppress_queried_variables: bool,
    /// Suppressed variables that must not be rendered.
    suppressed_variables: RefCell<Vec<Variable>>,
    /// Whether this render has checked for `disambiguate` in `cs:choose`.
    checked_disambiguate: bool,
    /// Check whether this is the first date.
    first_date: bool,
    /// The disambiguation-relevant properties of the first `cs:name` element.
    /// This is `None` if no `cs:name` elements were rendered.
    first_name: Option<NameDisambiguationProperties>,

    // Inheritable settings.
    /// A stack of formatting. Always contains the format of the root layout
    /// element at the bottom. Each element with a formatting may push and, if
    /// it did push, must remove. Its formatting shall apply for it and its
    /// children.
    format_stack: NonEmptyStack<Formatting>,
    /// Text cases.
    cases: NonEmptyStack<Option<TextCase>>,
    /// Inheritable name options.
    name_options: NonEmptyStack<InheritableNameOptions>,
    /// Delimiters from ancestor delimiting elements (e.g., `cs:group`).
    /// To be applied within `cs:choose`, but not another delimiting element or a macro.
    delimiters: NonEmptyStack<Option<String>>,

    // Buffers.
    /// The buffer we're writing to. If block-level or formatting changes, we
    /// flush the buffer to the last [`Elem`] in the elem stack.
    buf: CaseFolder,
    /// A list of in-progress subtrees. Elements that are to be nested are
    /// pushed and then either popped or inserted at the end of their ancestor.
    elem_stack: NonEmptyStack<ElemChildren>,
}

impl Default for WritingContext {
    fn default() -> Self {
        Self {
            pull_punctuation: false,
            inner_quotes: false,
            strip_periods: false,
            suppress_queried_variables: false,
            suppressed_variables: RefCell::new(Vec::new()),
            checked_disambiguate: false,
            first_date: true,
            first_name: None,
            format_stack: NonEmptyStack::default(),
            cases: NonEmptyStack::default(),
            name_options: NonEmptyStack::default(),
            delimiters: NonEmptyStack::default(),
            buf: CaseFolder::default(),
            elem_stack: NonEmptyStack::default(),
        }
    }
}

impl WritingContext {
    fn new(options: InheritableNameOptions) -> Self {
        Self {
            name_options: NonEmptyStack::new(options),
            ..Self::default()
        }
    }

    /// Retrieve the current formatting.
    fn formatting(&self) -> &Formatting {
        self.format_stack.last()
    }

    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) -> FormatIdx {
        if format.is_empty() {
            return FormatIdx(self.format_stack.len());
        }

        self.save_to_block();
        let pos = self.format_stack.len();
        self.format_stack.push(self.formatting().apply(format));
        FormatIdx(pos)
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, pos: FormatIdx) {
        if pos.0 == self.format_stack.len() {
            return;
        }

        self.save_to_block();
        self.format_stack.drain(pos.0).for_each(drop);
    }

    /// Saves the current buffer to the last element in the finished list. This
    /// must happen if [`Display`] or formatting changes, as well as when we're
    /// done with an element.
    fn save_to_block(&mut self) {
        if self.buf.is_empty() {
            return;
        }

        let format = *self.formatting();

        // Append to last child if formats match.
        if let Some(child) = self.elem_stack.last_mut().0.last_mut()
            && let ElemChild::Text(child) = child
            && format == child.formatting
        {
            child.text.push_str(&mem::take(&mut self.buf).finish());
            return;
        }

        let formatted = format.add_text(mem::take(&mut self.buf).finish());
        self.elem_stack.last_mut().0.push(ElemChild::Text(formatted))
    }

    /// Add another subtree to the children element. This must be done to
    /// include a new element or to check that the subtree is empty.
    fn push_elem(&mut self, format: citationberg::Formatting) -> DisplayLoc {
        self.save_to_block();
        let pos = self.elem_stack.len();
        self.elem_stack.push(ElemChildren::new());
        DisplayLoc::new(pos, self.push_format(format))
    }

    /// Remove the last subtree and discard it.
    ///
    /// Will panic if the location does not match.
    fn discard_elem(&mut self, loc: DisplayLoc) {
        if self.elem_stack.len().get() != loc.0.get() + 1 {
            panic!("stack location does not match");
        }
        assert_eq!(
            self.elem_stack.len().get(),
            loc.0.get() + 1,
            "stack location does not match"
        );
        self.pop_format(loc.1);

        self.save_to_block();
        self.elem_stack.drain(loc.0).for_each(drop);
    }

    fn has_content_since(&mut self, loc: &(DisplayLoc, usize)) -> bool {
        self.save_to_block();
        let children = self.elem_stack.last();
        let has_content = match children.0.first() {
            Some(ElemChild::Text(t)) if loc.1 < t.text.len() => {
                t.text[loc.1..].chars().any(|c| !c.is_whitespace())
            }
            Some(ElemChild::Text(_)) => false,
            Some(ElemChild::Elem(e)) => e.has_content(),
            Some(
                ElemChild::Markup(_)
                | ElemChild::Link { .. }
                | ElemChild::Transparent { .. },
            ) => true,
            None => false,
        };

        has_content || children.0.iter().skip(1).any(ElemChild::has_content)
    }

    /// Apply a prefix, but return a tuple that allows us to undo it if it
    /// wasn't followed by anything.
    fn apply_prefix(&mut self, affixes: &Affixes) -> (DisplayLoc, usize) {
        let pos = self.push_elem(citationberg::Formatting::default());
        if let Some(prefix) = &affixes.prefix {
            self.buf.push_str(prefix);
        };

        (pos, affixes.prefix.as_ref().map(|p| p.len()).unwrap_or_default())
    }

    /// Nest the last subtree into its ancestor. If the `display` argument is
    /// some, a new element child will be created there. Otherwise, we will
    /// append the children of the last subtree to the children of its ancestor.
    fn commit_elem(
        &mut self,
        loc: DisplayLoc,
        display: Option<Display>,
        meta: Option<ElemMeta>,
    ) {
        assert_eq!(
            self.elem_stack.len().get(),
            loc.0.get() + 1,
            "stack location does not match"
        );
        self.pop_format(loc.1);

        self.save_to_block();
        let children = self.elem_stack.pop().unwrap();

        if display.is_some() || meta.is_some() {
            self.elem_stack
                .last_mut()
                .0
                .push(Elem { children, display, meta }.into());
        } else {
            self.elem_stack.last_mut().0.extend(children.0);
        }
    }

    /// Push an item on the name options stack.
    pub fn push_name_options(&mut self, options: &InheritableNameOptions) {
        self.name_options.push(self.name_options.last().apply(options));
    }

    /// Pop an item from the name options stack.
    pub fn pop_name_options(&mut self) {
        self.name_options.pop();
    }

    /// Ensure that the buffer is either empty or the last character is a space.
    pub fn ensure_space(&mut self) {
        if !self.buf.is_empty() {
            if !self.buf.ends_with(' ') && !self.buf.ends_with('\u{a0}') {
                self.buf.push(' ');
            }
        } else if let Some(l) = self.elem_stack.last_mut().last_text_mut()
            && !l.text.ends_with(' ')
            && !l.text.ends_with('\u{a0}')
        {
            l.text.push(' ');
        }
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(mut self) -> ElemChildren {
        self.save_to_block();

        assert_eq!(
            self.format_stack.len().get(),
            1,
            "formatting stack is not one but {}",
            self.format_stack.len()
        );

        self.elem_stack.finish()
    }

    /// Set whether to strip periods.
    fn may_strip_periods(&mut self, strip: bool) {
        self.strip_periods = strip;
    }

    /// Stop stripping periods.
    fn stop_stripping_periods(&mut self) {
        self.strip_periods = false;
    }

    /// Start suppressing the queried variables.
    fn start_suppressing_queried_variables(&mut self) {
        self.suppress_queried_variables = true;
    }

    /// Stop suppressing the queried variables.
    fn stop_suppressing_queried_variables(&mut self) {
        self.suppress_queried_variables = false;
    }

    /// Set the case of the next text.
    fn push_case(&mut self, case: Option<TextCase>) -> CaseIdx {
        let idx = self.cases.len();
        self.cases.push(case);
        CaseIdx(idx)
    }

    /// Clear the case of the next text.
    fn pop_case(&mut self, idx: CaseIdx) {
        if idx.0 == self.cases.len() {
            return;
        }

        self.cases.drain(idx.0).for_each(drop);
    }
    /// Reconfigures the case folder's case to the current
    fn reconfigure(&mut self) {
        self.buf
            .reconfigure((*self.cases.last()).map(Into::into).unwrap_or_default());
    }

    /// Set the delimiter of the children of a [`citationberg::Group`] or
    /// [`citationberg::Layout`]. It is inherited by [`citationberg::Choose`]
    /// for its children as well.
    fn push_delimiter(&mut self, delimiter: Option<String>) -> DelimiterIdx {
        let idx = self.delimiters.len();
        self.delimiters.push(delimiter);
        DelimiterIdx(idx)
    }
    /// Clear the delimiter after a [`citationberg::Group`] or
    /// [`citationberg::Layout`].
    fn pop_delimiter(&mut self, idx: DelimiterIdx) {
        if idx.0 == self.delimiters.len() {
            return;
        }

        self.delimiters.drain(idx.0).for_each(drop);
    }

    /// Push a new suppressed variable if we are suppressing queried variables.
    fn maybe_suppress(&self, variable: Variable) {
        if self.suppress_queried_variables {
            self.suppressed_variables.borrow_mut().push(variable);
        }
    }

    fn prepare_variable_query<V>(&self, variable: V) -> Option<Variable>
    where
        V: Into<Variable>,
    {
        let general: Variable = variable.into();
        if self.suppressed_variables.borrow().contains(&general) {
            return None;
        }

        self.maybe_suppress(general);
        Some(general)
    }

    /// Return the sum of the lengths of strings in the finished elements. This
    /// may not monotonically increase.
    fn len(&self) -> usize {
        self.buf.len()
            + self
                .elem_stack
                .iter()
                .flat_map(|e| e.0.iter().map(ElemChild::str_len))
                .sum::<usize>()
    }

    /// Write the [`NameDisambiguationProperties`] that the first `cs:name`
    /// element used. Do not change if this is not the first `cs:name` element.
    /// Returns whether this was the first `cs:name` element.
    fn first_name_properties<F>(&mut self, f: F) -> bool
    where
        F: FnOnce() -> NameDisambiguationProperties,
    {
        if self.first_name.is_none() {
            self.first_name = Some(f());
            true
        } else {
            false
        }
    }
}

pub(crate) struct Context<'a, T: EntryLike> {
    instance: InstanceContext<'a, T>,
    style: &'a StyleContext<'a>,
    writing: WritingContext,
    bibliography: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct CiteProperties<'a> {
    pub certain: CertainCiteProperties,
    pub speculative: SpeculativeCiteProperties<'a>,
}

impl<'a> CiteProperties<'a> {
    fn for_sorting(locator: Option<SpecificLocator<'a>>, citation_number: usize) -> Self {
        Self {
            certain: CertainCiteProperties::new(),
            speculative: SpeculativeCiteProperties::speculate(
                locator,
                citation_number,
                IbidState::Different,
            ),
        }
    }
}

/// Item properties that can be determined before the first render of a
/// citation or bibliography entry.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
struct CertainCiteProperties {
    /// The number of the footnote this citation appears in.
    ///
    /// We can determine this because it depends on citation order only. May be
    /// none if the current style is in-text.
    pub note_number: Option<usize>,
    /// The number of the first footnote this citation appears in.
    pub first_note_number: Option<usize>,
    /// Whether the item is within `near-note-distance` to the previous citation
    /// of the same item.
    pub is_near_note: bool,
    /// Whether this citation is the first citation of the entry.
    ///
    /// Only depends on insertion order. The entry cannot be switched with
    /// itself during cite grouping.
    pub is_first: bool,
    /// The index of the item in the original citation request.
    pub initial_idx: usize,
}

impl CertainCiteProperties {
    fn new() -> Self {
        Self::default()
    }
}

/// Item properties that can only be determined after one or multiple renders.
/// These require validation.
#[derive(Debug, Clone, PartialEq)]
struct SpeculativeCiteProperties<'a> {
    /// Locator with its type.
    ///
    /// This will be none during the first render because it would interfere
    /// with disambiguation otherwise.
    pub locator: Option<SpecificLocator<'a>>,
    /// The position the item appears at in the bibliography.
    ///
    /// We can determine this using bibliography sort and cite order once we
    /// have seen all cites.
    pub citation_number: usize,
    /// Whether this citation is exactly the same as the previous citation.
    ///
    /// Although cite ordering is well-defined, grouping occurs only after the
    /// first render and can change the order.
    pub ibid: IbidState,
    /// Whether this citation is a disambiguation.
    ///
    /// Is always false for the first set of renders. Only the rendered entries
    /// allow us to determine if we need to disambiguate.
    pub disambiguation: DisambiguateState,
    /// Whether this citation includes an identifier.
    pub identifier_usage: IdentifierUsage,
}

impl<'a> SpeculativeCiteProperties<'a> {
    /// We can guess about the [`IbidState`] under the assumption that neither
    /// this item nor its predecessors will be subject to cite grouping.
    fn speculate(
        locator: Option<SpecificLocator<'a>>,
        citation_number: usize,
        ibid: IbidState,
    ) -> Self {
        Self {
            locator,
            citation_number,
            ibid,
            disambiguation: DisambiguateState::default(),
            identifier_usage: IdentifierUsage::default(),
        }
    }

    /// Get the speculative cite properties for a bibliography entry.
    fn for_bibliography(&self) -> Self {
        Self {
            locator: None,
            citation_number: self.citation_number,
            ibid: self.ibid,
            disambiguation: self.disambiguation.clone(),
            identifier_usage: self.identifier_usage,
        }
    }
}

/// Which type of citation label was used when this entry was cited.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum IdentifierUsage {
    /// No label was used.
    #[default]
    None,
    /// Only the alphanumeric citation label was used.
    OnlyCitationLabel,
    /// Only the citation number was used.
    OnlyCitationNumber,
    /// Both the alphanumeric citation label and the citation number were used.
    Mixed,
}

impl IdentifierUsage {
    /// Return the identifier usage given that a label was used.
    fn label(self) -> Self {
        match self {
            Self::None | Self::OnlyCitationLabel => Self::OnlyCitationLabel,
            Self::OnlyCitationNumber | Self::Mixed => Self::Mixed,
        }
    }

    /// Return the identifier usage given that a number was used.
    fn number(self) -> Self {
        match self {
            Self::None | Self::OnlyCitationNumber => Self::OnlyCitationNumber,
            Self::OnlyCitationLabel | Self::Mixed => Self::Mixed,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum DisambiguateState {
    #[default]
    None,
    NameDisambiguation(NameDisambiguationProperties),
    Choose,
    YearSuffix(u8),
}

impl DisambiguateState {
    /// Name disambiguation can be iterated upon, so we can stay here.
    fn may_disambiguate_names(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_))
    }

    /// We can only disambiguate with choose once.
    fn may_disambiguate_with_choose(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_))
    }

    /// We can only disambiguate with year suffix once.
    fn may_disambiguate_with_year_suffix(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_) | Self::Choose)
    }

    /// Return the more advanced disambiguation state between the two.
    fn max(self, other: Self) -> Self {
        match (self, other) {
            (Self::None, other) => other,
            (self_, Self::None) => self_,
            (Self::NameDisambiguation(a), Self::NameDisambiguation(b)) => {
                Self::NameDisambiguation(a.max(b))
            }
            (Self::NameDisambiguation(_), other) => other,
            (self_, Self::NameDisambiguation(_)) => self_,
            (Self::Choose, other) => other,
            (self_, Self::Choose) => self_,
            (Self::YearSuffix(a), Self::YearSuffix(b)) => Self::YearSuffix(a.max(b)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IbidState {
    /// The previous cite referenced another entry, or this cite has no locator
    /// when the previous one did.
    Different,
    /// The previous cite referenced the same entry, but with a different
    /// locator.
    IbidWithLocator,
    /// The previous cite referenced the same entry with the same locator.
    Ibid,
}

impl IbidState {
    fn with_last<T>(this: &CitationItem<T>, last: Option<&CitationItem<T>>) -> Self
    where
        T: EntryLike + PartialEq,
    {
        if let Some(last) = last {
            if last.entry == this.entry && !last.hidden {
                if last.locator == this.locator {
                    IbidState::Ibid
                } else if this.locator.is_none() {
                    // Per CSL spec: "Preceding cite does have a locator: (...)
                    // If the current cite lacks a locator its only position is
                    // 'subsequent'."
                    IbidState::Different
                } else {
                    // - Both have locators, but they differ
                    // - Previous one has no locator, this one does
                    IbidState::IbidWithLocator
                }
            } else {
                IbidState::Different
            }
        } else {
            IbidState::Different
        }
    }
}

/// This struct allows to add context to where the information in a cite is
/// found in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct SpecificLocator<'a>(pub Locator, pub LocatorPayload<'a>);

/// The type of content a `cs:text` element should yield for a locator.
#[derive(Debug, Clone, PartialEq)]
pub enum LocatorPayload<'a> {
    /// Just print the string.
    Str(&'a str),
    /// An element with the original index of the locator will be yielded. The
    /// consumer can then recognize this and replace it with their own content.
    Transparent(TransparentLocator),
}

/// Wraps an object that is used to determine whether two locators are the same.
#[derive(Clone)]
pub struct TransparentLocator(Arc<dyn TransparentLocatorPayload>);

impl TransparentLocator {
    /// Creates a new [`TransparentLocator`] which wraps the given payload.
    pub fn new<T: PartialEq + 'static>(payload: T) -> Self {
        Self(Arc::new(payload))
    }
}

impl std::fmt::Debug for TransparentLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TransparentLocator").finish_non_exhaustive()
    }
}

impl PartialEq for TransparentLocator {
    fn eq(&self, other: &Self) -> bool {
        TransparentLocatorPayload::dyn_eq(self.0.as_ref(), other.0.as_ref())
    }
}

trait TransparentLocatorPayload: Any {
    fn dyn_eq(&self, other: &dyn TransparentLocatorPayload) -> bool;
}

impl<T: PartialEq + 'static> TransparentLocatorPayload for T {
    fn dyn_eq(&self, other: &dyn TransparentLocatorPayload) -> bool {
        Some(self) == (other as &dyn Any).downcast_ref::<Self>()
    }
}

impl<'a, T: EntryLike> Context<'a, T> {
    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) -> FormatIdx {
        self.writing.push_format(format)
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, pos: FormatIdx) {
        self.writing.pop_format(pos)
    }

    /// Add another subtree to the children element. This must be done to
    /// include a new element or to check that the subtree is empty.
    fn push_elem(&mut self, format: citationberg::Formatting) -> DisplayLoc {
        self.writing.push_elem(format)
    }

    /// Remove the last subtree and discard it.
    ///
    /// Will panic if the location does not match.
    fn discard_elem(&mut self, loc: DisplayLoc) {
        self.writing.discard_elem(loc)
    }

    /// Nest the last subtree into its ancestor. If the `display` argument is
    /// some, a new element child will be created there. Otherwise, we will
    /// append the children of the last subtree to the children of its ancestor.
    fn commit_elem(
        &mut self,
        loc: DisplayLoc,
        display: Option<Display>,
        meta: Option<ElemMeta>,
    ) {
        self.writing.commit_elem(loc, display, meta)
    }

    /// Ensure that the buffer is either empty or the last character is a space.
    pub fn ensure_space(&mut self) {
        self.writing.ensure_space()
    }

    /// Add the appropriate opening quotation marks.
    fn push_quotes(&mut self) {
        let mark = self.term(
            if self.writing.inner_quotes {
                OtherTerm::OpenInnerQuote
            } else {
                OtherTerm::OpenQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.push_str(mark);
        }

        self.writing.inner_quotes = !self.writing.inner_quotes;
    }

    /// Add the appropriate closing quotation marks.
    fn pop_quotes(&mut self) {
        self.writing.inner_quotes = !self.writing.inner_quotes;

        let mark = self.term(
            if self.writing.inner_quotes {
                OtherTerm::CloseInnerQuote
            } else {
                OtherTerm::CloseQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.push_str(mark);
        }
    }

    /// Pull punctuation into a quote if applicable
    fn do_pull_punctuation<'s>(&mut self, mut s: &'s str) -> &'s str {
        if self.writing.pull_punctuation && s.starts_with(['.', ',', ';', '!', '?']) {
            let close_quote =
                self.term(OtherTerm::CloseQuote.into(), TermForm::default(), false);
            let close_inner_quote =
                self.term(OtherTerm::CloseInnerQuote.into(), TermForm::default(), false);

            let mut used_buf = false;
            let buf = if self.writing.buf.is_empty() {
                match self
                    .writing
                    .elem_stack
                    .last_mut_predicate(|p| !p.is_empty())
                    .and_then(|p| p.0.last_mut())
                {
                    Some(ElemChild::Text(f)) => &mut f.text,
                    // Get the text element if it is contained in an `Elem`.
                    Some(ElemChild::Elem(Elem { children, .. }))
                        if children.0.len() == 1
                            && matches!(children.0[0], ElemChild::Text(_)) =>
                    {
                        match &mut children.0[0] {
                            ElemChild::Text(f) => &mut f.text,
                            _ => unreachable!(),
                        }
                    }
                    _ => {
                        used_buf = true;
                        self.writing.buf.as_string_mut()
                    }
                }
            } else {
                used_buf = true;
                self.writing.buf.as_string_mut()
            };

            for quote in [close_quote, close_inner_quote].iter().flatten() {
                // Check if buf ends with a close quote.
                // If it does, replace it with the punctuation.
                if let Some(head) = buf.strip_suffix(quote) {
                    buf.truncate(head.len());

                    let punctuation = s.chars().next().unwrap();
                    s = &s[punctuation.len_utf8()..];
                    buf.push(punctuation);
                    buf.push_str(quote);

                    if used_buf {
                        self.writing.buf.mark_changed();
                    }

                    break;
                }
            }
        }

        s
    }

    /// Add a string to the buffer.
    fn push_str(&mut self, s: &str) {
        let s = self.do_pull_punctuation(s);

        self.writing.reconfigure();

        fn last_buffer(ctx: &mut WritingContext) -> Option<&mut String> {
            let last = ctx
                .elem_stack
                .last_mut_predicate(|s| !s.is_empty())
                .and_then(|e| e.last_text_mut());
            if !ctx.buf.is_empty() {
                Some(ctx.buf.as_string_mut())
            } else if let Some(last) = last {
                Some(&mut last.text)
            } else {
                None
            }
        }

        let ends_with_space = last_buffer(&mut self.writing)
            .is_some_and(|s| s.chars().next_back().is_some_and(|c| c.is_whitespace()));

        // Punctuation eats spaces. Whitespace should be trimmed.
        if ends_with_space
            && s.chars().next().is_some_and(|c| {
                c.is_whitespace() || c == '.' || c == ',' || c == ']' || c == ')'
            })
            && let Some(buf) = last_buffer(&mut self.writing)
        {
            buf.truncate(buf.trim_end().len());
        }

        // Do not print duplicate affixes.
        if s.chars().all(|c| !c.is_alphabetic()) {
            let trimmed = s.trim_end();
            if let Some(last) = last_buffer(&mut self.writing)
                && let Some(strip) = last.strip_suffix(trimmed)
            {
                last.truncate(strip.len());
            }
        }

        if self.writing.strip_periods {
            // Replicate citeproc.js behavior: remove a period if the
            // preceding character in the original string is not a period.
            let mut last_period = false;
            for c in s.chars() {
                let is_period = c == '.';
                if !is_period || last_period {
                    self.writing.buf.push(c);
                }
                last_period = is_period;
            }
        } else {
            self.writing.buf.push_str(s);
        }

        self.writing.pull_punctuation = false;
    }

    /// Push a chunked string to the buffer.
    pub fn push_chunked(&mut self, chunked: &ChunkedString) {
        for chunk in &chunked.0 {
            match chunk.kind {
                ChunkKind::Normal => self.push_str(&chunk.value),
                ChunkKind::Verbatim => {
                    self.writing.buf.push_verbatim(&chunk.value);
                    self.writing.pull_punctuation = false;
                }
                ChunkKind::Math => {
                    self.writing.buf.prevent_trimming();
                    self.writing.save_to_block();
                    self.writing
                        .elem_stack
                        .last_mut()
                        .0
                        .push(ElemChild::Markup(chunk.value.clone()));
                    self.writing.reconfigure();
                    self.writing.buf.prevent_trimming();
                }
            }
        }
    }

    /// Push a link into the buffer.
    pub fn push_link(&mut self, chunked: &ChunkedString, url: String) {
        let format = *self.writing.formatting();
        self.writing.save_to_block();
        self.writing
            .elem_stack
            .last_mut()
            .0
            .push(ElemChild::Link { text: format.add_text(chunked.to_string()), url })
    }

    /// Push a transparent element child into the buffer.
    pub fn push_transparent(&mut self, idx: usize) {
        let format = *self.writing.formatting();
        self.writing.save_to_block();
        self.writing
            .elem_stack
            .last_mut()
            .0
            .push(ElemChild::Transparent { cite_idx: idx, format })
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(self) -> ElemChildren {
        self.writing.flush()
    }

    /// Get a term from the style.
    fn term(&self, mut term: Term, form: TermForm, plural: bool) -> Option<&'a str> {
        if term == Term::NumberVariable(csl_taxonomy::NumberVariable::Locator)
            && let Some(locator) = &self.instance.cite_props.speculative.locator
        {
            term = locator.0.into();
        }

        let mut form = Some(form);
        while let Some(current_form) = form {
            if let Some(localization) = self.style.lookup_locale(|l| {
                let term = l.term(term, current_form)?;
                Some(if plural { term.multiple() } else { term.single() })
            }) {
                return localization;
            }

            form = current_form.fallback();
        }

        None
    }

    /// Get the gender of a term.
    fn gender(&self, term: Term) -> Option<GrammarGender> {
        if let Some(localization) =
            self.style.lookup_locale(|l| l.term(term, TermForm::default()))
        {
            localization.gender
        } else {
            None
        }
    }

    /// Get a localized date format.
    fn localized_date(&self, form: DateForm) -> Option<&'a citationberg::Date> {
        self.style
            .lookup_locale(|l| l.date.iter().find(|d| d.form == Some(form)))
    }

    /// Get the ordinal lookup object.
    fn ordinal_lookup(&self) -> OrdinalLookup<'a> {
        self.style
            .lookup_locale(|l| l.ordinals())
            .unwrap_or_else(OrdinalLookup::empty)
    }

    /// Pull the next punctuation character into the preceding quoted content
    /// if appropriate for the locale.
    fn may_pull_punctuation(&mut self) {
        self.writing.pull_punctuation |= self.style.punctuation_in_quotes();
    }

    /// Set whether to strip periods.
    fn may_strip_periods(&mut self, strip: bool) {
        self.writing.may_strip_periods(strip)
    }

    /// Stop stripping periods.
    fn stop_stripping_periods(&mut self) {
        self.writing.stop_stripping_periods()
    }

    /// Set the case of the next text.
    fn push_case(&mut self, case: Option<TextCase>) -> CaseIdx {
        if case.is_none_or(|c| c.is_language_independent())
            || self
                .instance
                .entry
                .is_english()
                .or_else(|| self.instance.locale.map(LocaleCode::is_english))
                .or_else(|| self.instance.term_locale.map(LocaleCode::is_english))
                .or_else(|| {
                    self.style.csl.default_locale.as_ref().map(LocaleCode::is_english)
                })
                .unwrap_or(true)
        {
            self.writing.push_case(case)
        } else {
            self.writing.push_case(None)
        }
    }

    /// Clear the case of the next text.
    fn pop_case(&mut self, idx: CaseIdx) {
        self.writing.pop_case(idx)
    }

    /// Resolve a number variable.
    ///
    /// Honors suppressions.
    fn resolve_number_variable(
        &self,
        variable: NumberVariable,
    ) -> Option<NumberVariableResult<'a>> {
        // Replace the citation label with citation number if necessary.
        if variable == NumberVariable::CitationNumber {
            if self.bibliography {
                if matches!(
                    *self.instance.identifier_usage.borrow(),
                    IdentifierUsage::OnlyCitationLabel
                ) {
                    return self
                        .instance
                        .resolve_standard_variable(
                            LongShortForm::default(),
                            StandardVariable::CitationLabel,
                        )
                        .map(|c| {
                            NumberVariableResult::Regular(MaybeTyped::String(
                                c.to_string(),
                            ))
                        });
                }
            } else {
                *self.instance.identifier_usage.borrow_mut() =
                    self.instance.identifier_usage.take().number();
            }
        }

        self.writing.prepare_variable_query(variable)?;

        self.instance.resolve_number_variable(variable)
    }

    fn resolve_page_variable(
        &self,
        variable: PageVariable,
    ) -> Option<PageVariableResult> {
        self.writing.prepare_variable_query(variable)?;
        self.instance.resolve_page_variable(variable)
    }

    fn resolve_number_or_page_variable(
        &self,
        variable: NumberOrPageVariable,
    ) -> Option<NumberOrPageVariableResult<'a>> {
        match variable {
            NumberOrPageVariable::Page(p) => {
                self.resolve_page_variable(p).map(NumberOrPageVariableResult::Page)
            }
            NumberOrPageVariable::Number(n) => self
                .resolve_number_variable(n)
                .map(NumberOrPageVariableResult::Number),
        }
    }

    /// Resolve a name variable.
    ///
    /// Honors suppressions.
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: csl_taxonomy::StandardVariable,
    ) -> Option<Cow<'a, ChunkedString>> {
        // Replace the citation label with citation number if necessary.
        if variable == StandardVariable::CitationLabel {
            if self.bibliography {
                if matches!(
                    *self.instance.identifier_usage.borrow(),
                    IdentifierUsage::OnlyCitationNumber
                ) {
                    return self
                        .instance
                        .resolve_number_variable(NumberVariable::CitationNumber)
                        .map(|n| match n {
                            NumberVariableResult::Regular(n) => {
                                Cow::Owned(ChunkedString::from(n.to_string()))
                            }
                            NumberVariableResult::Transparent(_) => {
                                panic!("unexpected transparent")
                            }
                        });
                }
            } else {
                *self.instance.identifier_usage.borrow_mut() =
                    self.instance.identifier_usage.take().label();
            }
        }

        self.writing.prepare_variable_query(variable)?;

        self.instance.resolve_standard_variable(form, variable)
    }

    /// Resolve a date variable.
    ///
    /// Honors suppressions.
    fn resolve_date_variable(
        &self,
        variable: csl_taxonomy::DateVariable,
    ) -> Option<Cow<'a, Date>> {
        self.writing.prepare_variable_query(variable)?;

        self.instance.entry.resolve_date_variable(variable)
    }

    /// Resolve a name variable.
    ///
    /// Honors suppressions.
    fn resolve_name_variable(
        &self,
        variable: csl_taxonomy::NameVariable,
    ) -> Vec<Cow<'a, Person>> {
        if self.writing.prepare_variable_query(variable).is_none() {
            return Vec::new();
        }

        self.instance.entry.resolve_name_variable(variable)
    }

    /// Apply a prefix, but return a tuple that allows us to undo it if it
    /// wasn't followed by anything.
    fn apply_prefix(&mut self, affixes: &Affixes) -> (DisplayLoc, usize) {
        self.writing.apply_prefix(affixes)
    }

    /// Apply a suffix, but only if the location was followed by something.
    /// Delete any prefix if not.
    fn apply_suffix(&mut self, affixes: &Affixes, loc: (DisplayLoc, usize)) {
        if !self.writing.has_content_since(&loc) {
            self.discard_elem(loc.0);
        } else {
            if let Some(suffix) = &affixes.suffix {
                let do_suffix = if !self.writing.buf.is_empty() {
                    !self.writing.buf.as_string_mut().ends_with(suffix.as_str())
                } else if let Some(l) = self.writing.elem_stack.last_mut().last_text() {
                    !l.text.ends_with(suffix)
                } else {
                    true
                };

                if do_suffix {
                    self.push_str(suffix);
                }
            }
            self.commit_elem(loc.0, None, None);
        }
    }

    /// Test whether this entry should disambiguate via `cs:choose`.
    fn should_disambiguate(&mut self) -> bool {
        self.writing.checked_disambiguate = true;
        self.instance.cite_props.speculative.disambiguation == DisambiguateState::Choose
    }

    /// Check whether this is the first date and set the first date flag.
    fn is_first_date(&mut self) -> bool {
        let res = self.writing.first_date;
        self.writing.first_date = false;
        res
    }

    /// Check whether this is the first date.
    fn peek_is_first_date(&self) -> bool {
        self.writing.first_date
    }

    /// Checks that neither the bibliography nor the citation layout render the
    /// year suffix.
    fn renders_year_suffix_implicitly(&mut self) -> bool {
        !self
            .style
            .csl
            .citation
            .layout
            .will_render(self, StandardVariable::YearSuffix.into())
            && !self.style.csl.bibliography.as_ref().is_some_and(|b| {
                b.layout.will_render(self, StandardVariable::YearSuffix.into())
            })
    }

    /// Set the [`SpecialForm`]
    fn set_special_form(&mut self, form: Option<SpecialForm>) {
        self.instance.kind = form;
    }
}

#[must_use = "format stack must be popped"]
struct FormatIdx(NonZeroUsize);

#[must_use = "element stack must be popped"]
struct DisplayLoc(NonZeroUsize, FormatIdx);

impl DisplayLoc {
    fn new(pos: NonZeroUsize, format_idx: FormatIdx) -> Self {
        Self(pos, format_idx)
    }
}

#[must_use = "case stack must be popped"]
struct CaseIdx(NonZeroUsize);

#[must_use = "delimiter stack must be popped"]
struct DelimiterIdx(NonZeroUsize);

impl<T: EntryLike> Write for Context<'_, T> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct UsageInfo {
    has_vars: bool,
    has_non_empty_vars: bool,
    has_used_macros: bool,
    has_non_empty_group: bool,
}

impl UsageInfo {
    /// Merge usage info with the info of a child.
    fn merge_child(self, child: Self) -> Self {
        Self {
            has_vars: self.has_vars || child.has_vars,
            has_non_empty_vars: self.has_non_empty_vars || child.has_non_empty_vars,
            has_used_macros: self.has_used_macros || child.has_used_macros,
            has_non_empty_group: self.has_non_empty_group || child.has_non_empty_group,
        }
    }

    /// Whether a group with this usage info should be rendered.
    fn should_render_group(&self) -> bool {
        !self.has_vars
            || (self.has_non_empty_vars
                || self.has_used_macros
                || self.has_non_empty_group)
    }
}

/// Describes the bracket preference of a citation style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Brackets {
    /// "(...)" Parentheses.
    Round,
    /// "[...]" Brackets.
    Square,
    /// No brackets.
    None,
}

impl Brackets {
    /// Get the according left bracket.
    pub fn left(&self) -> Option<char> {
        match self {
            Brackets::Round => Some('('),
            Brackets::Square => Some('['),
            Brackets::None => None,
        }
    }

    /// Get the according right bracket.
    pub fn right(&self) -> Option<char> {
        match self {
            Brackets::Round => Some(')'),
            Brackets::Square => Some(']'),
            Brackets::None => None,
        }
    }
}

/// For what purpose to generate a citation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CitePurpose {
    /// The citation will only contain the name of the author.
    Author,
    /// The citation will only contain the year. Year Affixes will not be
    /// included.
    Year,
    /// The citation will equate to the bibliography entry of the item.
    Full,
    /// The citation will be well-suited for inclusion in prose.
    Prose,
}

/// A special citation form to use for the [`CitationItem`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SpecialForm {
    /// Only output the author.
    VarOnly(Variable),
    /// Only print the first date.
    OnlyFirstDate,
    /// Only print the year suffix.
    OnlyYearSuffix,
    /// Output everything but the author.
    SuppressAuthor,
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use citationberg::LocaleFile;

    use super::*;
    use crate::{
        io::from_yaml_str,
        types::{Numeric, NumericValue},
    };

    #[test]
    fn test_csl() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"));
        let en_locale =
            fs::read_to_string(workspace.join("tests/data/locales-en-US.xml")).unwrap();
        let en_locale = LocaleFile::from_xml(&en_locale).unwrap();

        let yaml = fs::read_to_string(workspace.join("tests/data/basic.yml")).unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let en_locale = [en_locale.into()];

        for style_thing in fs::read_dir(workspace.join("styles/")).unwrap().take(100) {
            let thing = style_thing.unwrap();
            if thing.file_type().unwrap().is_dir() {
                continue;
            }

            let path = thing.path();
            let extension = path.extension();
            if let Some(extension) = extension {
                if extension.to_str() != Some("csl") {
                    continue;
                }
            } else {
                continue;
            }

            println!("testing {path:?}");
            let style = fs::read_to_string(path).unwrap();
            let style = IndependentStyle::from_xml(&style).unwrap();
            let mut driver = BibliographyDriver::new();

            for n in (0..bib.len()).step_by(2) {
                let items = vec![
                    CitationItem::with_entry(bib.nth(n).unwrap()),
                    CitationItem::with_entry(bib.nth(n + 1).unwrap()),
                ];
                driver.citation(CitationRequest::new(
                    items, &style, None, &en_locale, None,
                ));
            }

            driver.finish(BibliographyRequest {
                style: &style,
                locale: None,
                locale_files: &en_locale,
            });

            // for cite in finished.citations {
            //     println!("{}", cite.citation.to_string(BufWriteFormat::Plain))
            // }

            // if let Some(bib) = finished.bibliography {
            //     for (prefix, item) in bib.items {
            //         if let Some(prefix) = prefix {
            //             print!("{} ", prefix.to_string(BufWriteFormat::Plain))
            //         }
            //         println!("{}", item.to_string(BufWriteFormat::Html))
            //     }
            // }
        }
    }

    #[test]
    fn test_chapter_field() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"));
        let yaml = fs::read_to_string(workspace.join("tests/data/basic.yml")).unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let book = bib.get("lamb").unwrap();
        assert_eq!(book.chapter().unwrap(), &MaybeTyped::Typed(Numeric::new(20)));
        assert_eq!(book.volume().unwrap(), &MaybeTyped::Typed(Numeric::new(10)));
        assert_eq!(book.keyed_serial_number("serial"), None);
        assert_eq!(book.resolve_number_variable(NumberVariable::Number), None);

        let book = bib.get("snail").unwrap();
        assert_eq!(
            book.chapter().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(2),
                prefix: None,
                suffix: Some(Box::new("A".into()))
            })
        );
        assert_eq!(
            book.volume().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(4),
                prefix: None,
                suffix: Some(Box::new("B".into()))
            })
        );
        assert_eq!(book.keyed_serial_number("serial"), None);
        assert_eq!(book.resolve_number_variable(NumberVariable::Number), None);

        let chapter = bib.get("lamb-chapter").unwrap();
        assert_eq!(chapter.chapter().unwrap(), &MaybeTyped::Typed(Numeric::new(3)));
        assert_eq!(chapter.keyed_serial_number("serial"), None);
        assert_eq!(
            chapter.resolve_number_variable(NumberVariable::Number),
            Some(MaybeTyped::Typed(Cow::Borrowed(&Numeric::new(3))))
        );
        assert_eq!(chapter.volume(), None);
        assert_eq!(
            chapter.parents().first().unwrap().volume().unwrap(),
            &MaybeTyped::Typed(Numeric::new(10))
        );

        let chapter = bib.get("snail-chapter").unwrap();
        assert_eq!(chapter.chapter(), None);
        assert_eq!(chapter.keyed_serial_number("serial").unwrap(), "3");

        assert_eq!(chapter.volume(), None);
        assert_eq!(
            chapter.parents().first().unwrap().volume().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(4),
                prefix: None,
                suffix: Some(Box::new("B".into()))
            })
        );
    }

    #[test]
    fn low_year_test() {
        let yield_year = |year, short| {
            let mut s = String::new();
            write_year(year, short, &mut s, "AD", "BC").unwrap();
            s
        };

        assert_eq!(yield_year(2021, false), "2021");
        assert_eq!(yield_year(2021, true), "21");
        assert_eq!(yield_year(0, false), "1BC");
        assert_eq!(yield_year(-1, false), "2BC");
        assert_eq!(yield_year(1, false), "1AD");
        assert_eq!(yield_year(0, true), "1BC");
        assert_eq!(yield_year(-1, true), "2BC");
        assert_eq!(yield_year(1, true), "1AD");
    }

    #[test]
    fn test_transparent_locator_ibid() {
        let test_data = r#"
            katalog:
                type: Book
                title: "Book"
                author: "Surname, Name"
                location: Warsaw
                date: 1971
            katalog2:
                type: Book
                title: "Book2"
                author: "Surname, Name"
                location: Warsaw
                date: 1971
        "#;
        let test_style = r#"<?xml version="1.0" encoding="utf-8"?>
        <style xmlns="http://purl.org/net/xbiblio/csl" class="note" version="1.0" demote-non-dropping-particle="sort-only">
           <info>
              <title>Citing style</title>
              <id>http://www.example.com/</id>
           </info>

           <citation>
              <layout prefix="" suffix="." delimiter="; ">
                 <choose>
                    <if position="first">
                      <text value="first" />
                    </if>
                    <else-if position="ibid-with-locator">
                      <text value="ibid-with-locator" />
                    </else-if>
                    <else-if position="ibid">
                       <text value="ibid" />
                    </else-if>
                    <else-if position="subsequent">
                      <text value="subsequent" />
                    </else-if>
                    <else>
                      <text value="other" />
                    </else>
                 </choose>
              </layout>
           </citation>
          <bibliography>
            <sort>
              <key macro="contributors-biblio"/>
            </sort>
            <layout suffix=".">
              <group delimiter=", ">
                <group delimiter=", ">
                  <text macro="locators-chapter"/>
                </group>
              </group>
            </layout>
          </bibliography>
        </style>"#;

        let library = from_yaml_str(test_data).unwrap();
        let style = citationberg::Style::from_xml(test_style).unwrap();
        let citationberg::Style::Independent(style) = style else { unreachable!() };

        let mut driver = BibliographyDriver::new();
        let citations = [
            ("katalog", Some([2, 3]), "first."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog", Some([2, 4]), "ibid-with-locator."),
            ("katalog", None, "subsequent."),
            ("katalog", Some([2, 3]), "ibid-with-locator."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog2", Some([2, 3]), "first."),
        ];

        for (entry, supplement, _) in citations {
            let entry = library.get(entry).unwrap();
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_locator(
                    entry,
                    supplement.map(|s| {
                        SpecificLocator(
                            Locator::Supplement,
                            LocatorPayload::Transparent(TransparentLocator(Arc::new(s))),
                        )
                    }),
                )],
                &style,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &style,
            locale: None,
            locale_files: &[],
        });

        for (citation, (_, _, expected_value)) in finished.citations.iter().zip(citations)
        {
            let mut s = String::new();
            citation.citation.write_buf(&mut s, BufWriteFormat::Plain).unwrap();
            assert_eq!(s, expected_value);
        }
    }

    #[test]
    #[cfg(feature = "archive")]
    fn test_alphanumeric_disambiguation() {
        let bibtex = r#"@article{chenTransMorphTransformerUnsupervised2021,
        title = {{{TransMorph}}: {{Transformer}} for Unsupervised Medical Image Registration},
        author = {Chen, Junyu and Frey, Eric C. and He, Yufan and Segars, William P. and Li, Ye and Du, Yong},
        date = {2021},
        }

        @article{chenViTVNetVisionTransformer2021,
        title = {{{ViT-V-Net}}: {{Vision Transformer}} for {{Unsupervised Volumetric Medical Image Registration}}},
        author = {Chen, Junyu and He, Yufan and Frey, Eric C. and Li, Ye and Du, Yong},
        date = {2021},
        }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let alphanumeric = archive::ArchivedStyle::Alphanumeric.get();
        let citationberg::Style::Independent(alphanumeric) = alphanumeric else {
            unreachable!()
        };

        let mut driver = BibliographyDriver::new();
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_entry(entry)],
                &alphanumeric,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &alphanumeric,
            locale: None,
            locale_files: &[],
        });

        let mut c1 = String::new();
        let mut c2 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "[Che+21a]");
        assert_eq!(c2, "[Che+21b]");
    }

    #[test]
    #[cfg(feature = "archive")]
    /// See https://github.com/typst/hayagriva/issues/243
    fn issue_243() {
        let bibtex = r#"@book{downs57,
            title = {An Economic Theory of Democracy},
            author = {Downs, Anthony},
            date = {1957},
            edition = {1},
            publisher = {Harper \& Row},
            location = {New York},
            langid = {english}
            }

            @book{brady_collier10,
            title = {Rethinking Social Inquiry. Diverse Tools, Shared Standards},
            editor = {Brady, Henry E. and Collier, David},
            date = {2010},
            edition = {2},
            publisher = {Rowman \& Littlefield Publishers},
            location = {Maryland}
            }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let apa = archive::ArchivedStyle::AmericanPsychologicalAssociation.get();
        let citationberg::Style::Independent(apa) = apa else { unreachable!() };

        let mut driver = BibliographyDriver::new();
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::new(
                    entry,
                    None,
                    None,
                    false,
                    Some(CitePurpose::Prose),
                )],
                &apa,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &apa,
            locale: None,
            locale_files: &[],
        });

        let mut c1 = String::new();
        let mut c2 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "Downs (1957)");
        assert_eq!(c2, "Brady & Collier (2010)");
    }
    #[test]
    #[cfg(feature = "archive")]
    /// See https://github.com/typst/hayagriva/issues/48
    fn issue_48() {
        let bibtex = r#"@article{chenTransMorphTransformerUnsupervised2021,
        title = {{{TransMorph}}: {{Transformer}} for Unsupervised Medical Image Registration},
        author = {Chen, Junyu and Frey, Eric C. and He, Yufan and Segars, William P. and Li, Ye and Du, Yong},
        date = {2021},
        }

        @article{chenViTVNetVisionTransformer2021,
        title = {{{ViT-V-Net}}: {{Vision Transformer}} for {{Unsupervised Volumetric Medical Image Registration}}},
        author = {Chen, Junyu and He, Yufan and Frey, Eric C. and Li, Ye and Du, Yong},
        date = {2021},
        }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let alphanumeric = archive::ArchivedStyle::Alphanumeric.get();
        let citationberg::Style::Independent(alphanumeric) = alphanumeric else {
            unreachable!()
        };

        let mut driver = BibliographyDriver::new();

        let locator = SpecificLocator(Locator::Custom, LocatorPayload::Str("12"));
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_locator(entry, Some(locator.clone()))],
                &alphanumeric,
                None,
                &[],
                None,
            ));
        }

        driver.citation(CitationRequest::new(
            vec![CitationItem::with_locator(library.iter().next().unwrap(), None)],
            &alphanumeric,
            None,
            &[],
            None,
        ));

        let finished = driver.finish(BibliographyRequest {
            style: &alphanumeric,
            locale: None,
            locale_files: &[],
        });

        let mut c1 = String::new();
        let mut c2 = String::new();
        let mut c3 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[2]
            .citation
            .write_buf(&mut c3, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "[Che+21a, 12]");
        assert_eq!(c2, "[Che+21b, 12]");
        assert_eq!(c3, "[Che+21a]");
    }

    #[test]
    #[cfg(feature = "archive")]
    fn ibid_handling_with_deutsche_sprache_csl() {
        let bibtex = r#"@book{ITEM,
            title = {A},
            type = {book},
            }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let style = archive::ArchivedStyle::DeutscheSprache.get();
        let citationberg::Style::Independent(style) = style else { unreachable!() };

        let mut driver = BibliographyDriver::new();
        let entry = library.iter().next().unwrap();

        for locator in ["33", "33", "34"] {
            driver.citation(CitationRequest::new(
                vec![CitationItem::new(
                    entry,
                    Some(SpecificLocator(Locator::Page, LocatorPayload::Str(locator))),
                    None,
                    false,
                    Some(CitePurpose::Prose),
                )],
                &style,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &style,
            locale: None,
            locale_files: &[],
        });

        let actual = finished
            .citations
            .iter()
            .map(|c| {
                let mut s = String::new();
                c.citation.write_buf(&mut s, BufWriteFormat::Plain).unwrap();
                s
            })
            .collect::<Vec<_>>();

        assert_eq!(actual, ["(33)", "()", "(34)"]);
    }
}
