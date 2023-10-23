use std::fmt;
use std::mem;
use std::num::NonZeroUsize;

use citationberg::taxonomy::NameVariable;
use citationberg::{
    Display, FontStyle, FontVariant, FontWeight, TextDecoration, VerticalAlign,
};

/// A container for elements with useful methods.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Elem {
    /// The children of this element.
    pub children: ElemChildren,
    /// The inline or block display of this element.
    pub display: Option<Display>,
    /// The CSL construct that created this element.
    pub meta: Option<ElemMeta>,
}

impl Elem {
    pub(super) fn str_len(&self) -> usize {
        self.children
            .0
            .iter()
            .map(|c| match c {
                ElemChild::Text(t) => t.text.len(),
                ElemChild::Elem(e) => e.str_len(),
                ElemChild::Markup(m) => m.len(),
                ElemChild::Link { text, .. } => text.text.len(),
            })
            .sum()
    }

    pub(super) fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match (format, self.display) {
            (BufWriteFormat::Html, Some(Display::Block)) => w.write_str("<div>")?,
            (BufWriteFormat::Html, Some(Display::Indent)) => {
                w.write_str("<div style=\"padding-left: 4em;\">")?
            }
            (BufWriteFormat::Html, Some(Display::LeftMargin)) => {
                w.write_str("<div style=\"float: left;\">")?
            }
            (BufWriteFormat::Html, Some(Display::RightInline)) => {
                w.write_str("<div style=\"float: right; clear: both;\">")?
            }
            (_, Some(Display::Block)) => w.write_char('\n')?,
            (_, _) => {}
        }

        for child in &self.children.0 {
            child.write_buf(w, format)?;
        }

        match (format, self.display) {
            (BufWriteFormat::Html, Some(_)) => w.write_str("</div>")?,
            (_, Some(Display::Block)) => w.write_char('\n')?,
            (_, _) => {}
        }

        Ok(())
    }

    pub(super) fn is_empty(&self) -> bool {
        if self.children.is_empty() {
            true
        } else {
            self.children.is_empty()
        }
    }

    pub(super) fn has_content(&self) -> bool {
        self.children.has_content()
    }

    pub(super) fn simplify(self) -> Self {
        Self { children: simplify_children(self.children), ..self }
    }

    pub(super) fn may_inline(&self) -> bool {
        self.display.is_none() && self.meta.is_none()
    }
}

impl fmt::Display for Elem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.write_buf(f, BufWriteFormat::Plain)
        } else {
            self.write_buf(f, BufWriteFormat::VT100)
        }
    }
}

/// Merge adjacent text nodes with the same formatting.
pub(super) fn simplify_children(children: ElemChildren) -> ElemChildren {
    ElemChildren(children.0.into_iter().fold(Vec::new(), |mut acc, child| {
        match (child, acc.last_mut()) {
            (ElemChild::Text(t), Some(ElemChild::Text(last)))
                if last.formatting == t.formatting =>
            {
                last.text.push_str(&t.text);
                return acc;
            }
            (ElemChild::Elem(e), _) if e.may_inline() => {
                acc.extend(e.children.0);
            }
            (ElemChild::Elem(e), _) => {
                acc.push(ElemChild::Elem(e.simplify()));
            }
            (child, _) => {
                acc.push(child);
            }
        }
        acc
    }))
}

/// Which CSL construct created an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElemMeta {
    /// The element is the output of `cs:names`.
    Names,
    /// The element is the output of `cs:date`.
    Date,
    /// The element is the output of `cs:text`.
    Text,
    /// The element is the output of `cs:number`.
    Number,
    /// The element is the output of `cs:label`.
    Label,
    /// The element is the output of `cs:number (variable="citation-number")`.
    CitationNumber,
    /// The element is the output of a single name.
    /// It notes which name variable was used and the index within the variable.
    Name(NameVariable, usize),
}

/// A container for element children with useful methods.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ElemChildren(pub Vec<ElemChild>);

impl ElemChildren {
    /// Create an empty `ElemChildren`.
    pub(super) fn new() -> Self {
        Self(Vec::new())
    }

    /// Whether this container is empty.
    pub fn is_empty(&self) -> bool {
        self.0.iter().all(|e| e.is_empty())
    }

    /// Whether this container has any content other than whitespace.
    pub(super) fn has_content(&self) -> bool {
        self.0.iter().any(|e| e.has_content())
    }

    /// Retrieve a reference to the first child with a matching meta by
    /// DFS.
    pub(super) fn get_meta(&self, meta: ElemMeta) -> Option<&Elem> {
        for child in &self.0 {
            match child {
                ElemChild::Elem(e) if e.meta == Some(meta) => return Some(e),
                ElemChild::Elem(e) => {
                    if let Some(e) = e.children.get_meta(meta) {
                        return Some(e);
                    }
                }
                _ => {}
            }
        }

        None
    }

    /// Remove the first child with a matching meta by DFS.
    pub(super) fn remove_meta(&mut self, meta: ElemMeta) -> bool {
        for i in 0..self.0.len() {
            if let ElemChild::Elem(e) = &mut self.0[i] {
                if e.meta == Some(meta) {
                    self.0.remove(i);
                    return true;
                }

                if e.children.remove_meta(meta) {
                    return true;
                }
            }
        }

        false
    }

    /// Remove the first child with any meta by DFS.
    pub(super) fn remove_any_meta(&mut self) -> Option<ElemChild> {
        for i in 0..self.0.len() {
            if let ElemChild::Elem(e) = &mut self.0[i] {
                if e.meta.is_some() {
                    return Some(self.0.remove(i));
                }

                if let Some(elem) = e.children.remove_any_meta() {
                    return Some(elem);
                }
            }
        }

        None
    }

    /// Write the children to the given buffer.
    pub fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        for child in &self.0 {
            child.write_buf(w, format)?;
        }
        Ok(())
    }
}

impl fmt::Display for ElemChildren {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.write_buf(f, BufWriteFormat::Plain)
        } else {
            self.write_buf(f, BufWriteFormat::VT100)
        }
    }
}

/// Various formattable elements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElemChild {
    /// This is some text.
    Text(Formatted),
    /// A child element.
    Elem(Elem),
    /// This should be processed by Typst.
    Markup(String),
    /// This is a link.
    Link {
        /// The anchor text.
        text: Formatted,
        /// The URL.
        url: String,
    },
}

impl ElemChild {
    /// Write the child to a buffer.
    pub fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match self {
            ElemChild::Text(t) => {
                t.formatting.write_start(w, format)?;
                w.write_str(&t.text)?;
                t.formatting.write_end(w, format)?;
                Ok(())
            }
            ElemChild::Elem(e) => e.write_buf(w, format),
            ElemChild::Markup(m) => w.write_str(m),
            ElemChild::Link { text, url } if format == BufWriteFormat::Html => {
                w.write_str("<a href=\"")?;
                w.write_str(url)?;
                w.write_str("\">")?;
                text.formatting.write_start(w, format)?;
                w.write_str(&text.text)?;
                text.formatting.write_end(w, format)?;
                w.write_str("</a>")
            }
            ElemChild::Link { text, .. } => {
                text.formatting.write_start(w, format)?;
                w.write_str(&text.text)?;
                text.formatting.write_end(w, format)
            }
        }
    }

    pub(super) fn str_len(&self) -> usize {
        match self {
            ElemChild::Text(t) => t.text.len(),
            ElemChild::Elem(e) => e.str_len(),
            ElemChild::Markup(m) => m.len(),
            ElemChild::Link { text, .. } => text.text.len(),
        }
    }

    pub(super) fn has_content(&self) -> bool {
        match self {
            ElemChild::Text(Formatted { text, .. }) | ElemChild::Markup(text) => {
                text.chars().any(|c| !c.is_whitespace())
            }
            ElemChild::Elem(e) => e.has_content(),
            ElemChild::Link { .. } => true,
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        match self {
            ElemChild::Text(Formatted { text, .. }) | ElemChild::Markup(text) => {
                text.is_empty()
            }
            ElemChild::Elem(e) => e.is_empty(),
            ElemChild::Link { text, .. } => text.text.is_empty(),
        }
    }
}

impl fmt::Display for ElemChild {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.write_buf(f, BufWriteFormat::Plain)
        } else {
            self.write_buf(f, BufWriteFormat::VT100)
        }
    }
}

impl From<Elem> for ElemChild {
    fn from(e: Elem) -> Self {
        ElemChild::Elem(e)
    }
}

impl From<Formatted> for ElemChild {
    fn from(f: Formatted) -> Self {
        ElemChild::Text(f)
    }
}

/// The format with which to write an [element](Elem).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum BufWriteFormat {
    /// Just write text.
    #[default]
    Plain,
    /// Write with terminal colors.
    VT100,
    /// Write HTML.
    Html,
}

/// A piece of formatted text.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Formatted {
    /// The text.
    pub text: String,
    /// The formatting.
    pub formatting: Formatting,
}

/// Some formatting information.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Formatting {
    /// Whether the text is italic.
    pub font_style: FontStyle,
    /// Whether the text is small caps.
    pub font_variant: FontVariant,
    /// The font weight.
    pub font_weight: FontWeight,
    /// Whether the text is underlined.
    pub text_decoration: TextDecoration,
    /// Whether the text is superscript or subscript.
    pub vertical_align: VerticalAlign,
}

impl Formatting {
    pub(super) fn add_text(self, text: String) -> Formatted {
        Formatted { text, formatting: self }
    }

    /// Apply a partial formatting on top of this formatting.
    pub(super) fn apply(mut self, other: citationberg::Formatting) -> Self {
        if let Some(style) = other.font_style {
            self.font_style = style;
        }

        if let Some(variant) = other.font_variant {
            self.font_variant = variant;
        }

        if let Some(weight) = other.font_weight {
            self.font_weight = weight;
        }

        if let Some(decoration) = other.text_decoration {
            self.text_decoration = decoration;
        }

        if let Some(align) = other.vertical_align {
            self.vertical_align = align;
        }

        self
    }

    pub(super) fn write_vt100(
        &self,
        buf: &mut impl fmt::Write,
    ) -> Result<(), fmt::Error> {
        if self.font_style == FontStyle::Italic {
            buf.write_str("\x1b[3m")?;
        }

        if self.font_weight == FontWeight::Bold {
            buf.write_str("\x1b[1m")?;
        } else if self.font_weight == FontWeight::Light {
            buf.write_str("\x1b[2m")?;
        }

        if self.text_decoration == TextDecoration::Underline {
            buf.write_str("\x1b[4m")?;
        }

        Ok(())
    }

    pub(super) fn write_css(&self, buf: &mut impl fmt::Write) -> Result<(), fmt::Error> {
        if self.font_style == FontStyle::Italic {
            buf.write_str("font-style: italic;")?;
        }

        match self.font_weight {
            FontWeight::Bold => buf.write_str("font-weight: bold;")?,
            FontWeight::Light => buf.write_str("font-weight: lighter;")?,
            _ => {}
        }

        if self.text_decoration == TextDecoration::Underline {
            buf.write_str("text-decoration: underline;")?;
        }

        if self.font_variant == FontVariant::SmallCaps {
            buf.write_str("font-variant: small-caps;")?;
        }

        match self.vertical_align {
            VerticalAlign::Sub => buf.write_str("vertical-align: sub;")?,
            VerticalAlign::Sup => buf.write_str("vertical-align: super;")?,
            _ => {}
        }

        Ok(())
    }

    pub(super) fn write_start(
        &self,
        buf: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match format {
            BufWriteFormat::Plain => Ok(()),
            BufWriteFormat::VT100 => self.write_vt100(buf),
            BufWriteFormat::Html => {
                let is_default = self == &Formatting::default();
                if !is_default {
                    buf.write_str("<span style=\"")?;
                    self.write_css(buf)?;
                    buf.write_str("\">")?;
                }
                Ok(())
            }
        }
    }

    pub(super) fn write_end(
        &self,
        buf: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match format {
            BufWriteFormat::Plain => Ok(()),
            BufWriteFormat::VT100 => buf.write_str("\x1b[0m"),
            BufWriteFormat::Html => {
                let is_default = self == &Formatting::default();
                if !is_default {
                    buf.write_str("</span>")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct NonEmptyStack<T> {
    head: Vec<T>,
    last: T,
}

impl<T> NonEmptyStack<T> {
    pub fn new(last: T) -> Self {
        Self { head: Vec::new(), last }
    }

    pub fn last(&self) -> &T {
        &self.last
    }

    pub fn last_mut(&mut self) -> &mut T {
        &mut self.last
    }

    pub fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.head.len() + 1).unwrap()
    }

    pub fn push(&mut self, elem: T) {
        self.head.push(mem::replace(&mut self.last, elem));
    }

    pub fn pop(&mut self) -> Option<T> {
        let new_last = self.head.pop()?;
        Some(mem::replace(&mut self.last, new_last))
    }

    /// Drains all elements including and after the given index.
    pub fn drain(&mut self, idx: NonZeroUsize) -> impl Iterator<Item = T> + '_ {
        let idx = idx.get();
        mem::swap(&mut self.head[idx - 1], &mut self.last);
        let mut drain = self.head.drain(idx - 1..);
        let first = drain.next();
        drain.chain(first)
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.head.iter().chain(std::iter::once(&self.last))
    }

    pub fn finish(self) -> T {
        if !self.head.is_empty() {
            panic!("NonEmptyStack::finish called with non-empty stack")
        }

        self.last
    }
}

impl<T: Default> Default for NonEmptyStack<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
