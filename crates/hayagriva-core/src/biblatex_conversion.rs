use biblatex::{self as tex, DateValue};

use crate::{Date, Person};

pub fn date(date: tex::Date) -> Date {
    let approximate = date.uncertain || date.approximate;

    match date.value {
        DateValue::At(x) | DateValue::After(x) | DateValue::Before(x) => Date {
            year: x.year,
            month: x.month,
            day: x.day,
            approximate,
            season: None,
        },
        DateValue::Between(_, x) => Date {
            year: x.year,
            month: x.month,
            day: x.day,
            approximate,
            season: None,
        },
    }
}

pub fn person(person: &tex::Person) -> Person {
    fn optional(part: &str) -> Option<String> {
        if !part.is_empty() { Some(part.to_string()) } else { None }
    }

    Person {
        name: person.name.clone(),
        given_name: optional(&person.given_name),
        prefix: optional(&person.prefix),
        suffix: optional(&person.suffix),
        comma_suffix: false,
        alias: None,
    }
}
