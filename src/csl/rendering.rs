use std::borrow::Cow;
use std::fmt::Write;
use std::str::FromStr;

use citationberg::taxonomy::{NameVariable, NumberVariable, OtherTerm, Term, Variable};
use citationberg::{
    ChooseBranch, DateDayForm, DateMonthForm, DatePartName, DateParts, DateStrongAnyForm,
    DelimiterBehavior, DemoteNonDroppingParticle, LabelPluralize, LayoutRenderingElement,
    LongShortForm, NameAnd, NameAsSortOrder, NameForm, Names, NumberForm, TestPosition,
    TextCase, ToAffixes, ToFormatting,
};
use citationberg::{TermForm, TextTarget};

use crate::lang::{Case, SentenceCase, TitleCase};
use crate::types::{Date, MaybeTyped, Numeric, Person};

use super::taxonomy::matches_entry_type;
use super::Context;

pub(crate) trait RenderCsl {
    fn render(&self, ctx: &mut Context);
}

impl RenderCsl for citationberg::Text {
    fn render(&self, ctx: &mut Context) {
        let depth = ctx.push_elem(self.formatting);

        let affix_loc = ctx.apply_prefix(&self.affixes);

        if self.quotes {
            ctx.push_quotes();
        }

        ctx.may_strip_periods(self.strip_periods);
        let cidx = ctx.push_case(self.text_case);

        match &self.target {
            TextTarget::Variable { var, form } => ctx.push_chunked(
                match var {
                    Variable::Standard(var) => ctx.resolve_standard_variable(*form, *var),
                    Variable::Number(var) => ctx
                        .resolve_number_variable(*var)
                        .map(|t| Cow::Owned(t.to_chunked_string())),
                    _ => None,
                }
                .unwrap_or_default()
                .as_ref(),
            ),
            TextTarget::Macro { name } => {
                let len = ctx.len();
                let mac = ctx.get_macro(name);

                if let Some(mac) = &mac {
                    for child in &mac.children {
                        child.render(ctx);
                    }

                    if len < ctx.len() {
                        ctx.printed_non_empty_macro();
                    }
                }
            }
            TextTarget::Term { term, form, plural } => {
                ctx.push_str(ctx.term(*term, *form, *plural).unwrap_or_default())
            }
            TextTarget::Value { val } => ctx.push_str(val),
        }

        ctx.pop_case(cidx);
        ctx.stop_stripping_periods();

        if self.quotes {
            ctx.pop_quotes();
            ctx.may_pull_punctuation();
        }

        ctx.apply_suffix(&self.affixes, affix_loc);
        ctx.commit_elem(depth, self.display);
    }
}

impl RenderCsl for citationberg::Number {
    fn render(&self, ctx: &mut Context) {
        let depth = ctx.push_elem(self.formatting);

        let affix_loc = ctx.apply_prefix(&self.affixes);

        let cidx = ctx.push_case(self.text_case);

        let value = ctx.resolve_number_variable(self.variable);
        match value {
            Some(MaybeTyped::Typed(num)) if num.will_transform() => {
                let normal_num = if self.form == NumberForm::Numeric
                    && self.variable == NumberVariable::Page
                {
                    if let Some(range) = num.range() {
                        ctx.settings
                            .page_range_format
                            .unwrap_or_default()
                            .format(
                                range,
                                ctx,
                                ctx.term(
                                    OtherTerm::PageRangeDelimiter.into(),
                                    TermForm::default(),
                                    false,
                                ),
                            )
                            .unwrap();
                        false
                    } else {
                        true
                    }
                } else {
                    true
                };

                if normal_num {
                    num.as_ref().with_form(ctx, self.form, ctx.ordinal_lookup()).unwrap();
                }
            }
            Some(MaybeTyped::Typed(num)) => write!(ctx, "{}", num).unwrap(),
            Some(MaybeTyped::String(s)) => ctx.push_str(&s),
            None => {}
        }

        ctx.pop_case(cidx);
        ctx.apply_suffix(&self.affixes, affix_loc);
        ctx.commit_elem(depth, self.display);
    }
}

impl RenderCsl for citationberg::Label {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = ctx.resolve_number_variable(self.variable) else {
            return;
        };

        let plural = match self.label.plural {
            LabelPluralize::Always => true,
            LabelPluralize::Never => false,
            LabelPluralize::Contextual => match variable {
                MaybeTyped::String(_) => false,
                MaybeTyped::Typed(n) => {
                    n.is_plural(self.variable.is_number_of_variable())
                }
            },
        };

        let content = ctx
            .term(Term::from(self.variable), self.label.form, plural)
            .unwrap_or_default();

        render_label_with_var(&self.label, ctx, content)
    }
}

fn render_label_with_var(
    label: &citationberg::VariablelessLabel,
    ctx: &mut Context,
    content: &str,
) {
    if content.is_empty() {
        return;
    }

    let idx = ctx.push_format(label.formatting);

    let affixes = &label.affixes;
    let affix_loc = ctx.apply_prefix(affixes);

    ctx.may_strip_periods(label.strip_periods);
    let cidx = ctx.push_case(label.text_case);

    ctx.push_str(content);

    ctx.pop_case(cidx);
    ctx.stop_stripping_periods();
    ctx.apply_suffix(affixes, affix_loc);
    ctx.pop_format(idx);
}

impl RenderCsl for citationberg::Date {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = self.variable else { return };
        let Some(date) = ctx.resolve_date_variable(variable) else { return };

        let base = if let Some(form) = self.form {
            let Some(base) = ctx.localized_date(form) else { return };
            Some(base)
        } else {
            None
        };

        let formatting = base
            .map(|b| self.formatting.apply(b.formatting))
            .unwrap_or(self.formatting);
        let depth = ctx.push_elem(formatting);

        let affix_loc = ctx.apply_prefix(&self.affixes);

        let cidx = ctx.push_case(self.text_case.or(base.and_then(|b| b.text_case)));

        let parts = if let Some(base) = base {
            base.parts.unwrap_or_default()
        } else {
            DateParts::default()
        };

        // TODO: Date ranges
        let mut last_was_empty = true;
        for part in &base.unwrap_or(self).date_part {
            match part.name {
                DatePartName::Month if !parts.has_month() => continue,
                DatePartName::Day if !parts.has_day() => continue,
                _ => {}
            }

            let cursor = ctx.len();
            if !last_was_empty {
                if let Some(delim) = &self.delimiter {
                    ctx.push_str(delim);
                }
            }

            let over_ride = base
                .is_some()
                .then(|| self.date_part.iter().find(|p| p.name == part.name))
                .flatten();

            render_date_part(part, date, ctx, over_ride);
            last_was_empty = cursor == ctx.len();
        }

        ctx.pop_case(cidx);
        ctx.apply_suffix(&self.affixes, affix_loc);
        ctx.commit_elem(depth, self.display);
    }
}

fn render_date_part(
    date_part: &citationberg::DatePart,
    date: &Date,
    ctx: &mut Context,
    over_ride: Option<&citationberg::DatePart>,
) {
    let Some(val) = (match date_part.name {
        DatePartName::Day => date.day.map(|i| i as i32 + 1),
        DatePartName::Month => date.month.map(|i| i as i32 + 1),
        DatePartName::Year => Some(date.year),
    }) else {
        return;
    };

    let formatting = over_ride
        .map(|p| p.formatting.apply(date_part.formatting))
        .unwrap_or(date_part.formatting);

    let idx = ctx.push_format(formatting);

    let affixes = &date_part.affixes;
    let affix_loc = ctx.apply_prefix(affixes);

    let cidx = ctx.push_case(over_ride.and_then(|o| o.text_case).or(date_part.text_case));

    let form = over_ride
        .map(citationberg::DatePart::form)
        .unwrap_or_else(|| date_part.form());
    match form {
        DateStrongAnyForm::Day(DateDayForm::NumericLeadingZeros)
        | DateStrongAnyForm::Month(DateMonthForm::NumericLeadingZeros) => {
            write!(ctx, "{:02}", val).unwrap();
        }
        DateStrongAnyForm::Day(DateDayForm::Ordinal)
            if val != 1
                || !ctx
                    .lookup_locale(|l| {
                        Some(
                            l.style_options
                                .and_then(|o| o.limit_day_ordinals_to_day_1)
                                .unwrap_or_default(),
                        )
                    })
                    .unwrap_or_default() =>
        {
            write!(
                ctx,
                "{}{}",
                val,
                ctx.ordinal_lookup().lookup(val).unwrap_or_default()
            )
            .unwrap();
        }
        DateStrongAnyForm::Day(DateDayForm::Numeric | DateDayForm::Ordinal)
        | DateStrongAnyForm::Month(DateMonthForm::Numeric) => {
            write!(ctx, "{}", val).unwrap();
        }
        DateStrongAnyForm::Month(DateMonthForm::Long) => {
            if let Some(month) = ctx.term(
                OtherTerm::month((val - 1) as u8).unwrap().into(),
                TermForm::Long,
                false,
            ) {
                ctx.push_str(month);
            } else {
                write!(ctx, "{}", val).unwrap();
            }
        }
        DateStrongAnyForm::Month(DateMonthForm::Short) => {
            if let Some(month) = ctx.term(
                OtherTerm::month((val - 1) as u8).unwrap().into(),
                TermForm::Short,
                false,
            ) {
                ctx.push_str(month);
            } else {
                write!(ctx, "{}", val).unwrap();
            }
        }
        DateStrongAnyForm::Year(LongShortForm::Short) => {
            write!(ctx, "{:02}", (val % 100).abs()).unwrap();
        }
        DateStrongAnyForm::Year(LongShortForm::Long) => {
            write!(ctx, "{}", val.abs()).unwrap();
        }
    }

    if let DateStrongAnyForm::Year(_) = form {
        if val < 1000 {
            ctx.push_str(if val < 0 { "BC" } else { "AD" });
        }
    }

    ctx.apply_suffix(affixes, affix_loc);
    ctx.pop_case(cidx);
    ctx.pop_format(idx);
}

impl RenderCsl for Names {
    fn render(&self, ctx: &mut Context) {
        let people: Vec<(Vec<&Person>, Term)> = if self.variable.len() == 2
            && self.variable.contains(&NameVariable::Editor)
            && self.variable.contains(&NameVariable::Translator)
        {
            let editors = ctx.resolve_name_variable(NameVariable::Editor);
            let translators = ctx.resolve_name_variable(NameVariable::Translator);

            let mut res = Vec::new();
            if !editors.is_empty() && editors == translators {
                res.push((editors, NameVariable::EditorTranslator.into()))
            } else {
                if !editors.is_empty() {
                    res.push((editors, NameVariable::Editor.into()));
                }

                if !translators.is_empty() {
                    res.push((translators, NameVariable::Translator.into()));
                }
            }

            res
        } else {
            self.variable
                .iter()
                .map(|v| (ctx.resolve_name_variable(*v), Term::from(*v)))
                .collect()
        };

        ctx.push_name_options(&self.options);

        let is_empty = people.iter().all(|(p, _)| p.is_empty());
        if is_empty {
            if let Some(substitute) = &self.substitute {
                ctx.start_suppressing_queried_variables();

                for child in &substitute.children {
                    let len = ctx.len();
                    // TODO deal with name shorthand
                    child.render(ctx);
                    if len < ctx.len() {
                        break;
                    }
                }

                ctx.stop_suppressing_queried_variables();
            }

            ctx.pop_name_options();
            return;
        }

        let depth = ctx.push_elem(self.formatting);
        let affix_loc = ctx.apply_prefix(&self.affixes);

        for (i, (persons, term)) in people.into_iter().enumerate() {
            let plural = persons.len() != 1;
            add_names(self, ctx, persons);

            if let Some(label) = &self.label {
                render_label_with_var(
                    label,
                    ctx,
                    ctx.term(term, label.form, plural).unwrap_or_default(),
                )
            }

            if i > 0 {
                let delim = self.delimiter(ctx.name_options.last());
                if !delim.is_empty() {
                    let delim = delim.to_string();
                    ctx.push_str(&delim);
                }
            }
        }

        ctx.apply_suffix(&self.affixes, affix_loc);
        ctx.commit_elem(depth, self.display);
        ctx.pop_name_options();
    }
}

#[derive(Debug, Clone, Copy)]
enum EndDelim {
    Delim,
    And(NameAnd),
    DelimAnd(NameAnd),
}

// TODO differentiate subsequent cites.
fn add_names(names: &citationberg::Names, ctx: &mut Context, persons: Vec<&Person>) {
    let take =
        if persons.len() >= names.options.et_al_min.map_or(usize::MAX, |u| u as usize) {
            (names.options.et_al_use_first.map_or(usize::MAX, |u| u as usize))
                .min(persons.len())
        } else {
            persons.len()
        };

    let has_et_al = persons.len() > take;
    let et_al_use_last =
        names.options.et_al_use_last.unwrap_or_default() && take + 2 <= persons.len();
    let mut last_inverted = false;
    let reverse = matches!(
        ctx.settings.demote_non_dropping_particle,
        DemoteNonDroppingParticle::DisplayAndSort
    );
    let name_opts = names.name.options(&names.options);

    for (i, name) in persons.iter().take(take).enumerate() {
        let last = i + 1 == take;

        if i != 0 {
            let mut delim = EndDelim::Delim;
            if last && i > 0 && !has_et_al {
                if let Some(d) = names.options.and {
                    delim =
                        match names.options.delimiter_precedes_last.unwrap_or_default() {
                            DelimiterBehavior::Contextual if i >= 2 => {
                                EndDelim::DelimAnd(d)
                            }
                            DelimiterBehavior::AfterInvertedName if last_inverted => {
                                EndDelim::DelimAnd(d)
                            }
                            DelimiterBehavior::Always => EndDelim::DelimAnd(d),
                            _ => EndDelim::And(d),
                        }
                }
            }

            match delim {
                EndDelim::Delim => ctx.push_str(name_opts.delimiter),
                EndDelim::And(and) => {
                    ctx.push_str(" ");
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.push_str(" ");
                }
                EndDelim::DelimAnd(and) => {
                    ctx.push_str(name_opts.delimiter);
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.push_str(" ");
                }
            }
        }

        let inverted = match names.options.name_as_sort_order {
            Some(NameAsSortOrder::First) if i == 0 => true,
            Some(NameAsSortOrder::All) => true,
            _ => false,
        };

        write_name(name, ctx, name_opts.form == NameForm::Long, inverted, reverse, names);

        last_inverted = inverted;
    }

    if et_al_use_last {
        if let Some(name) = persons.last() {
            ctx.push_str(name_opts.delimiter);
            ctx.push_str("… ");
            write_name(
                name,
                ctx,
                name_opts.form == NameForm::Long,
                matches!(names.options.name_as_sort_order, Some(NameAsSortOrder::All)),
                reverse,
                names,
            );
        }
    } else if has_et_al {
        if let Some(term) = ctx.term(names.et_al.term.into(), TermForm::default(), false)
        {
            let delim = match names.options.delimiter_precedes_et_al {
                Some(DelimiterBehavior::Always) => true,
                Some(DelimiterBehavior::Contextual) if take >= 2 => true,
                Some(DelimiterBehavior::AfterInvertedName) if last_inverted => true,
                _ => false,
            };

            if delim {
                ctx.push_str(name_opts.delimiter);
            }

            let idx = ctx.push_format(names.et_al.formatting);
            ctx.push_str(term);
            ctx.pop_format(idx);
        }
    }
}

fn write_name(
    name: &Person,
    ctx: &mut Context,
    long: bool,
    reverse: bool,
    demote_non_dropping: bool,
    names: &citationberg::Names,
) {
    let hyphen_init = ctx.settings.initialize_with_hyphen;
    let initialize = names.options.initialize.unwrap_or(true);
    let initialize_with = names.options.initialize_with.as_deref();
    let sort_sep = names.options.sort_separator.as_deref().unwrap_or(", ");

    let first_part = names.name.name_part_given();
    let family_part = names.name.name_part_family();
    let first_format = first_part.map(|p| p.formatting).unwrap_or_default();
    let first_case = first_part.map(|p| p.text_case).unwrap_or_default();
    let first_affixes = [
        first_part.map(|p| &p.affixes).and_then(|f| f.prefix.as_ref()),
        first_part.map(|p| &p.affixes).and_then(|f| f.suffix.as_ref()),
    ];
    let family_format = family_part.map(|p| p.formatting).unwrap_or_default();
    let family_case = family_part.map(|p| p.text_case).unwrap_or_default();
    let family_affixes = [
        family_part.map(|p| &p.affixes).and_then(|f| f.prefix.as_ref()),
        family_part.map(|p| &p.affixes).and_then(|f| f.suffix.as_ref()),
    ];

    let first_name = |ctx: &mut Context| {
        if let Some(first) = &name.given_name {
            if let Some(initialize_with) = initialize_with {
                if initialize {
                    name.initials(ctx, Some(initialize_with), hyphen_init).unwrap();
                } else {
                    name.first_name_with_delimiter(ctx, Some(initialize_with)).unwrap();
                }
            } else {
                ctx.push_str(first);
            }

            true
        } else {
            false
        }
    };

    let simple = |ctx: &mut Context| {
        let idx = ctx.push_format(family_format);
        let cidx = ctx.push_case(family_case);
        if let Some(prefix) = family_affixes[0] {
            ctx.push_str(prefix);
        }
        ctx.push_str(&name.name);
        ctx.pop_case(cidx);
        ctx.pop_format(idx);
        if let Some(suffix) = family_affixes[1] {
            ctx.push_str(suffix);
        }
    };

    match (long, reverse, demote_non_dropping) {
        _ if name.is_institutional() => simple(ctx),
        (true, _, _) if name.is_cjk() => {
            let idx = ctx.push_format(family_format);
            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }
            ctx.push_str(&name.name);
            ctx.pop_format(idx);
            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if let Some(given) = &name.given_name {
                let idx = ctx.push_format(first_format);
                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                ctx.push_str(given);
                ctx.pop_format(idx);

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }
        }
        (true, false, _) => {
            let idx = ctx.push_format(first_format);
            let cidx = ctx.push_case(first_case);

            if let Some(prefix) = first_affixes[0] {
                ctx.push_str(prefix);
            }

            first_name(ctx);
            ctx.ensure_space();
            if let Some(prefix) = &name.prefix {
                ctx.push_str(prefix);
            }

            ctx.pop_format(idx);
            ctx.pop_case(cidx);

            if let Some(suffix) = first_affixes[1] {
                ctx.push_str(suffix);
            }

            ctx.ensure_space();
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = &name.suffix {
                ctx.ensure_space();
                ctx.push_str(suffix);
            }

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }
        }
        (true, true, false) => {
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                let idx = ctx.push_format(first_format);
                let cidx = ctx.push_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.pop_case(cidx);
                ctx.pop_format(idx);

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }

            if let Some(suffix) = &name.suffix {
                ctx.push_str(sort_sep);
                ctx.ensure_space();
                ctx.push_str(suffix);
            }
        }
        (true, true, true) => {
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(name.name_without_particle());

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                let idx = ctx.push_format(first_format);
                let cidx = ctx.push_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.pop_case(cidx);
                ctx.pop_format(idx);

                if let Some(particle) = &name.name_particle() {
                    ctx.ensure_space();
                    ctx.push_str(particle);
                }

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }

            if let Some(suffix) = &name.suffix {
                ctx.push_str(sort_sep);
                ctx.ensure_space();
                ctx.push_str(suffix);
            }
        }
        (false, _, _) => {
            simple(ctx);
        }
    }
}

impl RenderCsl for citationberg::Choose {
    fn render(&self, ctx: &mut Context) {
        for branch in self.branches() {
            if branch.match_.test(BranchConditionIter::from_branch(branch, ctx)) {
                render_with_delimiter(&branch.children, self.delimiter.as_deref(), ctx);
                return;
            }
        }

        if let Some(fallthrough) = &self.otherwise {
            render_with_delimiter(&fallthrough.children, self.delimiter.as_deref(), ctx);
        }
    }
}

fn render_with_delimiter(
    children: &[LayoutRenderingElement],
    delimiter: Option<&str>,
    ctx: &mut Context,
) {
    let mut last_empty = true;
    let mut loc = None;

    for child in children {
        if !last_empty {
            if let Some(delim) = delimiter {
                let prev_loc = std::mem::take(&mut loc);

                if let Some(prev_loc) = prev_loc {
                    ctx.commit_elem(prev_loc, None);
                }

                loc = Some(ctx.push_elem(citationberg::Formatting::default()));
                ctx.push_str(delim);
            }
        }

        let pos = ctx.push_elem(citationberg::Formatting::default());

        match child {
            LayoutRenderingElement::Text(text) => text.render(ctx),
            LayoutRenderingElement::Number(num) => num.render(ctx),
            LayoutRenderingElement::Label(label) => label.render(ctx),
            LayoutRenderingElement::Date(date) => date.render(ctx),
            LayoutRenderingElement::Names(names) => names.render(ctx),
            LayoutRenderingElement::Choose(choose) => choose.render(ctx),
            LayoutRenderingElement::Group(_group) => _group.render(ctx),
        }

        last_empty = ctx.last_is_empty();
        if last_empty {
            ctx.discard_elem(pos);
        } else {
            ctx.commit_elem(pos, None);
        }
    }

    if let Some(loc) = loc {
        if last_empty {
            ctx.discard_elem(loc);
        } else {
            ctx.commit_elem(loc, None);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BranchConditionPos {
    Disambiguate,
    IsNumeric,
    IsUncertainDate,
    Locator,
    Position,
    Type,
    Variable,
}

impl Iterator for BranchConditionPos {
    type Item = Self;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Disambiguate => {
                *self = Self::IsNumeric;
                Some(Self::Disambiguate)
            }
            Self::IsNumeric => {
                *self = Self::IsUncertainDate;
                Some(Self::IsNumeric)
            }
            Self::IsUncertainDate => {
                *self = Self::Locator;
                Some(Self::IsUncertainDate)
            }
            Self::Locator => {
                *self = Self::Position;
                Some(Self::Locator)
            }
            Self::Position => {
                *self = Self::Type;
                Some(Self::Position)
            }
            Self::Type => {
                *self = Self::Variable;
                Some(Self::Type)
            }
            Self::Variable => None,
        }
    }
}

struct BranchConditionIter<'a, 'b> {
    cond: &'a ChooseBranch,
    ctx: &'a mut Context<'b>,
    pos: BranchConditionPos,
    idx: usize,
}

impl<'a, 'b> BranchConditionIter<'a, 'b> {
    fn from_branch(cond: &'a ChooseBranch, ctx: &'a mut Context<'b>) -> Self {
        Self {
            cond,
            ctx,
            pos: BranchConditionPos::Disambiguate,
            idx: 0,
        }
    }

    fn next_case(&mut self) {
        self.pos.next();
        self.idx = 0;
    }
}

impl<'a, 'b> Iterator for BranchConditionIter<'a, 'b> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pos {
            BranchConditionPos::Disambiguate => {
                self.pos.next();
                if let Some(d) = self.cond.disambiguate {
                    Some(d == self.ctx.cite_props.map_or(false, |p| p.is_disambiguation))
                } else {
                    self.next()
                }
            }
            BranchConditionPos::IsNumeric => {
                if let Some(vars) = &self.cond.is_numeric {
                    if self.idx >= vars.len() {
                        self.next_case();
                        return self.next();
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(match var {
                        Variable::Standard(var) => self
                            .ctx
                            .resolve_standard_variable(LongShortForm::default(), var)
                            .map(|v| Numeric::from_str(&v.to_string()).is_ok())
                            .unwrap_or_default(),
                        Variable::Number(var) => matches!(
                            self.ctx.resolve_number_variable(var),
                            Some(MaybeTyped::Typed(_))
                        ),
                        _ => false,
                    })
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::IsUncertainDate => {
                if let Some(vars) = &self.cond.is_uncertain_date {
                    if self.idx >= vars.len() {
                        self.next_case();
                        return self.next();
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(
                        self.ctx
                            .resolve_date_variable(var)
                            .map_or(false, |d| d.approximate),
                    )
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Locator => {
                if let Some(locs) = &self.cond.locator {
                    if self.idx >= locs.len() {
                        self.next_case();
                        return self.next();
                    }

                    let loc = locs[self.idx];
                    self.idx += 1;

                    Some(
                        self.ctx
                            .cite_props
                            .and_then(|c| c.locator)
                            .map(|l| l.0)
                            .map_or(false, |l| l == loc),
                    )
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Position => {
                if let Some(pos) = &self.cond.position {
                    if self.idx >= pos.len() {
                        self.next_case();
                        return self.next();
                    }

                    let spec_pos = pos[self.idx];
                    self.idx += 1;

                    let Some(props) = self.ctx.cite_props else {
                        self.next_case();
                        return Some(false);
                    };

                    Some(match spec_pos {
                        TestPosition::First => props.is_first,
                        TestPosition::Subsequent => !props.is_first,
                        TestPosition::Ibid => props.is_ibid,
                        TestPosition::IbidWithLocator => {
                            props.is_ibid || props.is_ibid_with_locator
                        }
                        TestPosition::NearNote => props.is_near_note,
                    })
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Type => {
                if let Some(kind) = &self.cond.type_ {
                    if self.idx >= kind.len() {
                        self.next_case();
                        return self.next();
                    }

                    let kind = kind[self.idx];
                    self.idx += 1;

                    Some(matches_entry_type(self.ctx.entry, kind))
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Variable => {
                if let Some(vars) = &self.cond.variable {
                    if self.idx >= vars.len() {
                        return None;
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(match var {
                        Variable::Standard(s) => {
                            let val = self
                                .ctx
                                .resolve_standard_variable(LongShortForm::default(), s);
                            val.map_or(false, |s| {
                                !s.to_string().chars().all(char::is_whitespace)
                            })
                        }
                        Variable::Number(n) => {
                            let val = self.ctx.resolve_number_variable(n);
                            val.is_some()
                        }
                        Variable::Date(d) => self.ctx.resolve_date_variable(d).is_some(),
                        Variable::Name(n) => {
                            !self.ctx.resolve_name_variable(n).is_empty()
                        }
                    })
                } else {
                    None
                }
            }
        }
    }
}

impl RenderCsl for citationberg::Group {
    fn render(&self, ctx: &mut Context) {
        let info = ctx.push_usage_info();
        let idx = ctx.push_elem(self.to_formatting());
        let affixes = self.to_affixes();

        let affix_loc = ctx.apply_prefix(&affixes);

        render_with_delimiter(&self.children, self.delimiter.as_deref(), ctx);

        ctx.apply_suffix(&affixes, affix_loc);

        let info = ctx.pop_usage_info(info);
        if info.has_vars
            && (!info.has_non_empty_vars
                && !info.has_used_macros
                && !info.has_non_empty_group)
        {
            ctx.discard_elem(idx);
        } else {
            ctx.commit_elem(idx, self.display);
            ctx.printed_non_empty_group()
        }
    }
}

impl RenderCsl for citationberg::LayoutRenderingElement {
    fn render(&self, ctx: &mut Context) {
        match self {
            citationberg::LayoutRenderingElement::Text(text) => text.render(ctx),
            citationberg::LayoutRenderingElement::Number(num) => num.render(ctx),
            citationberg::LayoutRenderingElement::Label(label) => label.render(ctx),
            citationberg::LayoutRenderingElement::Date(date) => date.render(ctx),
            citationberg::LayoutRenderingElement::Names(names) => names.render(ctx),
            citationberg::LayoutRenderingElement::Choose(choose) => choose.render(ctx),
            citationberg::LayoutRenderingElement::Group(group) => group.render(ctx),
        }
    }
}

impl RenderCsl for citationberg::Layout {
    fn render(&self, ctx: &mut Context) {
        let fidx = ctx.push_format(self.to_formatting());

        let affixes = self.to_affixes();
        let affix_pos = ctx.apply_prefix(&affixes);

        render_with_delimiter(&self.elements, self.delimiter.as_deref(), ctx);

        ctx.apply_suffix(&affixes, affix_pos);
        ctx.pop_format(fidx);
    }
}

impl RenderCsl for citationberg::RenderingElement {
    fn render(&self, ctx: &mut Context) {
        match self {
            citationberg::RenderingElement::Layout(l) => l.render(ctx),
            citationberg::RenderingElement::Other(o) => o.render(ctx),
        }
    }
}

impl From<TextCase> for Case {
    fn from(case: TextCase) -> Self {
        match case {
            TextCase::Uppercase => Case::Uppercase,
            TextCase::Lowercase => Case::Lowercase,
            TextCase::TitleCase => Case::Title(TitleCase::default()),
            TextCase::SentenceCase => Case::Sentence(SentenceCase::default()),
            TextCase::CapitalizeFirst => Case::FirstUpper,
            TextCase::CapitalizeAll => Case::AllUpper,
        }
    }
}
