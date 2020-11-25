//! Notes and Bibliography (nb) referencing as defined in chapter 14 of the
//! 17th edition of the Chicago Manual of Style.

use crate::types::{FormattableString, Title};

pub mod bibliography;
pub mod notes;

fn omit_initial_articles(s: &str) -> String {
    let parts = s.split(' ').collect::<Vec<_>>();
    if parts.len() < 2 {
        return s.to_string();
    }

    if ["a", "an", "the"].contains(&parts.first().unwrap().to_lowercase().as_ref()) {
        (&parts[1 ..]).join(" ")
    } else {
        s.to_string()
    }
}

fn shorthand(title: &Title) -> FormattableString {
    if let Some(sh) = title.clone().shorthand {
        sh
    } else {
        FormattableString {
            value: omit_initial_articles(&title.value.value),
            title_case: title
                .value
                .title_case
                .as_ref()
                .map(|tc| omit_initial_articles(tc)),
            sentence_case: title
                .value
                .sentence_case
                .as_ref()
                .map(|tc| omit_initial_articles(tc)),
            verbatim: title.value.verbatim,
        }
    }
}
