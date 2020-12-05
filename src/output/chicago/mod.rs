//! Both the author / date and notes / bibliograpghy citation styles as defined
//! in the 17th edition of the Chicago Manual of Style.

// pub mod author_date;
pub mod nb;

use crate::selectors::{Id, Neg, Wc};
use crate::types::EntryType::*;
use crate::{attrs, sel, Entry};

/// Configure when to print access dates. Also see Chicago 14.12.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AccessDateConfig {
    /// Never print access dates.
    Never,
    /// Print if no other date was found.
    NoDate,
    /// Print if no other date was found or if this is a inherently online
    /// media item like `Web`, `Blog`, or `Repository`.
    InherentlyOnline,
    /// Print if no other date was found or if the media item is not
    /// traditionally formally published, like `Thesis`, `Manuscript`, `Report`.
    NotFormallyPublished,
    /// Always print access dates.
    Always,
}

impl AccessDateConfig {
    /// Will determine, given the value of `self` and a [`Entry`], if an access
    /// date should be provided.
    pub fn needs_date(&self, entry: &Entry) -> bool {
        match self {
            AccessDateConfig::Never => false,
            AccessDateConfig::NoDate => entry.get_any_date().is_none(),
            AccessDateConfig::InherentlyOnline => {
                sel!(alt
                    Id(Web),
                    sel!(sel!(alt Id(Misc), Id(Web)) => Id(Web)),
                )
                .matches(entry)
                    || entry.get_any_date().is_none()
            }
            AccessDateConfig::NotFormallyPublished => {
                !is_formally_published(entry) || entry.get_any_date().is_none()
            }
            AccessDateConfig::Always => true,
        }
    }
}

fn is_authoritative(entry: &Entry) -> bool {
    sel!(alt
        Id(Book),
        Id(Proceedings),
        attrs!(Id(Periodical), "volume"),
        attrs!(Id(Periodical), "issue"),
        Id(Patent),
        Id(Thesis),
        Id(Report),
        Id(Reference),
        sel!(Wc() => Id(Conference)),
        sel!(Wc() => Id(Book)),
        sel!(Wc() => Id(Proceedings)),
        sel!(Wc() => attrs!(Id(Periodical), "volume")),
        sel!(Wc() => attrs!(Id(Periodical), "issue")),
        sel!(Id(Article) => Id(Anthology)),
        Id(Case),
        Id(Legislation),
    )
    .matches(entry)
}

fn is_formally_published(entry: &Entry) -> bool {
    sel!(alt
        Id(Book),
        Id(Proceedings),
        Id(Periodical),
        Id(Patent),
        Id(Case),
        Id(Legislation),
        Id(Newspaper),
        sel!(Neg(Id(Manuscript)) => attrs!(Wc(), "publisher")),
        sel!(Wc() => Id(Book)),
        sel!(Wc() => Id(Proceedings)),
        sel!(Wc() => Id(Periodical)),
        sel!(Id(Article) => Id(Newspaper)),
        sel!(sel!(alt Id(Article), Id(Anthos)) => Id(Anthology)),
    )
    .matches(entry)
}
