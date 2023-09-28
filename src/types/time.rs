use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use thiserror::Error;
use unscanny::Scanner;
use yaml_rust::Yaml;

use super::{DeserializationError, HayagrivaValue, ParseContext, YamlDictExt, YamlExt};

/// A date that can be as coarse as a year and as fine-grained as a day.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Date {
    /// The year (1 B.C.E. is represented as 0 and so forth).
    pub year: i32,
    /// The optional month (0-11).
    pub month: Option<u8>,
    /// The optional day (0-30).
    pub day: Option<u8>,
    /// Whether the date is approximate
    pub approximate: bool,
}

/// This error can occur when trying to get a date from a string.
#[derive(Clone, Copy, Debug, Error, PartialEq, Eq)]
pub enum DateError {
    /// The string does not conform to the date interval.
    #[error("date format unknown")]
    UnknownFormat,
    /// The month is out of bounds.
    #[error("month not in interval 1-12")]
    MonthOutOfBounds,
    /// The day is out of bounds.
    #[error("month has no day {0}")]
    DayOutOfBounds(u8),
}

impl FromStr for Date {
    type Err = DateError;

    /// Parse a date from a string.
    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let mut s = Scanner::new(source);
        s.eat_whitespace();
        let approx = s.eat_if('~');

        let idx = s.cursor();
        match parse_full_date(&mut s) {
            Ok((year, month, day)) => {
                return Ok(Self {
                    year,
                    month: Some(month),
                    day: Some(day),
                    approximate: approx,
                });
            }
            Err(DateError::UnknownFormat) => {
                s.jump(idx);
            }
            Err(e) => {
                return Err(e);
            }
        }

        match parse_month_with_year(&mut s) {
            Ok((year, month)) => {
                return Ok(Self {
                    year,
                    month: Some(month),
                    day: None,
                    approximate: approx,
                });
            }
            Err(DateError::UnknownFormat) => {
                s.jump(idx);
            }
            Err(e) => {
                return Err(e);
            }
        }

        let year = parse_year(&mut s)?;
        s.eat_whitespace();
        if !s.done() {
            return Err(DateError::UnknownFormat);
        }

        Ok(Self { year, month: None, day: None, approximate: approx })
    }
}

impl Date {
    /// Get a date from an integer.
    pub fn from_year(year: i32) -> Self {
        Self { year, month: None, day: None, approximate: false }
    }

    /// Returns the year as a human-readable gregorian year.
    ///
    /// Non-positive values will be marked with a "BCE" postfix.
    pub fn display_year(&self) -> String {
        self.display_year_opt(true, false, false, false)
    }

    /// Returns the year as a human-readable gregorian year with controllable
    /// pre- and postfixes denominating the year's positivity.
    ///
    /// ## Arguments
    /// - `secular`   Switches between "BC" and "BCE"
    /// - `periods`   Determines whether to use punctuation in the abbreviations
    /// - `designate_positive`    Show a denomination for positive years
    /// - `ad_prefix` Use the "AD" designation for positive years in a prefix
    ///               position. Will be ignored if `designate_positive` is negative.
    pub fn display_year_opt(
        &self,
        secular: bool,
        periods: bool,
        designate_positive: bool,
        ad_prefix: bool,
    ) -> String {
        let np_postfix = match (secular, periods) {
            (true, false) => "BCE",
            (true, true) => "B.C.E.",
            (false, false) => "BC",
            (false, true) => "B.C.",
        };

        let positive_dn = match (periods, ad_prefix) {
            (true, false) => "C.E.",
            (false, false) => "CE",
            (true, true) => "AD",
            (false, true) => "A.D.",
        };

        if self.year > 0 {
            if designate_positive && ad_prefix {
                format!("{} {}", positive_dn, self.year)
            } else if designate_positive && !ad_prefix {
                format!("{} {}", self.year, positive_dn)
            } else {
                self.year.to_string()
            }
        } else {
            format!("{} {}", -(self.year as i64 - 1), np_postfix)
        }
    }
}

impl HayagrivaValue for Date {
    fn from_yaml(
        yaml: &Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => {
                let date = Self::from_str(s)?;
                Ok(date)
            }
            Yaml::Integer(i) => Ok(Self::from_year(*i as i32)),
            Yaml::Hash(hash) => {
                let approx = hash.get_with_str("approximate", ctx)?;
                let Yaml::Boolean(approx) = approx else {
                    return Err(DateError::UnknownFormat.into());
                };
                ctx.pop_dict_key();

                let year = hash.get_with_str("year", ctx)?;
                let year = match year {
                    Yaml::Integer(i) => *i as i32,
                    Yaml::String(s) => parse_year(&mut Scanner::new(s))?,
                    _ => return Err(DateError::UnknownFormat.into()),
                };
                ctx.pop_dict_key();

                let month = hash.get_with_str("month", ctx).ok();
                let month = match month {
                    Some(Yaml::Integer(i)) => Some(*i as u8),
                    Some(Yaml::String(s)) => Some(parse_month(&mut Scanner::new(s))?),
                    None => None,
                    _ => return Err(DateError::UnknownFormat.into()),
                };
                ctx.pop_dict_key();

                let day = if month.is_some() {
                    let yaml = hash.get_with_str("day", ctx).ok();
                    let res = match yaml {
                        Some(Yaml::Integer(i)) => Some(*i as u8),
                        Some(Yaml::String(s)) => Some(parse_day(&mut Scanner::new(s))?),
                        None => None,
                        _ => return Err(DateError::UnknownFormat.into()),
                    };

                    if yaml.is_some() {
                        ctx.pop_dict_key();
                    }

                    res
                } else {
                    None
                };

                Ok(Self { year, month, day, approximate: *approx })
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        let mut buf = String::with_capacity(10);
        if self.approximate {
            buf.push('~');
        }

        write!(&mut buf, "{:04}", self.year).unwrap();

        if let Some(month) = self.month {
            write!(&mut buf, "-{:02}", month + 1).unwrap();

            if let Some(day) = self.day {
                write!(&mut buf, "-{:02}", day + 1).unwrap();
            }
        }

        yaml_rust::Yaml::String(buf)
    }

    fn explain() -> &'static str {
        "A date in the format YYYY-MM-DD"
    }
}

/// A duration.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub struct Duration {
    /// Days (24 hours).
    pub days: u32,
    /// Hours (60 minutes).
    pub hours: u32,
    /// Minutes (60 seconds).
    pub minutes: u32,
    /// Seconds (1000 milliseconds).
    pub seconds: u8,
    /// Milliseconds.
    pub milliseconds: u16,
}

impl Duration {
    fn scan(s: &mut Scanner, require_end: bool) -> Result<Self, DurationError> {
        let mut days = 0;
        let mut hours = 0;
        let mut minutes = 0;
        let mut seconds = 0;
        let mut milliseconds = 0;

        let idx = s.cursor();
        let colons =
            s.eat_while(|c: char| c != '-').chars().filter(|c| c == &':').count();
        s.jump(idx);

        if !(1..=3).contains(&colons) {
            return Err(DurationError::Malformed);
        }

        let start = 3 - colons;

        for i in start..5 {
            match i {
                0 => days = parse_unsigned_int(s, 1..).ok_or(DurationError::Malformed)?,
                1 => {
                    hours = if start == i {
                        parse_unsigned_int(s, 2..)
                    } else {
                        parse_unsigned_int(s, 2..=2)
                    }
                    .ok_or(DurationError::Malformed)?
                }
                2 => {
                    minutes = if start == i {
                        parse_unsigned_int(s, 2..)
                    } else {
                        parse_unsigned_int(s, 2..=2)
                    }
                    .ok_or(DurationError::Malformed)?
                }
                3 => {
                    seconds =
                        parse_unsigned_int(s, 2..=2).ok_or(DurationError::Malformed)?
                }
                4 => {
                    s.eat_whitespace();
                    let num = s.eat_while(char::is_numeric);
                    if num.is_empty() {
                        return Err(DurationError::Malformed);
                    }
                    let str = format!("0.{}", num);
                    let ms: f64 = str.parse().map_err(|_| DurationError::Malformed)?;
                    milliseconds = (ms * 1000.0).round() as u16;
                }
                _ => unreachable!(),
            }

            if s.done() {
                if i < 3 {
                    return Err(DurationError::Malformed);
                } else {
                    break;
                }
            }

            s.eat_whitespace();

            if i < 3 && !s.eat_if(':') {
                return Err(DurationError::Malformed);
            } else if i == 3 {
                if s.peek() == Some('-') {
                    break;
                }
                if !s.eat_if(",") {
                    return Err(DurationError::Malformed);
                }
            }
        }

        s.eat_whitespace();
        if require_end && !s.done() {
            return Err(DurationError::Malformed);
        }

        for i in (0..=start).rev() {
            match i {
                0 => {}
                1 => {
                    days = hours / 24;
                    hours %= 24;
                }
                2 => {
                    hours += minutes / 60;
                    minutes %= 60;
                }
                _ => unreachable!(),
            }
        }

        if hours >= 24 || minutes >= 60 || seconds >= 60 || milliseconds >= 1000 {
            return Err(DurationError::TooLarge);
        }

        Ok(Duration { days, hours, minutes, seconds, milliseconds })
    }
}

impl PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Duration {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hours
            .cmp(&other.hours)
            .then(self.minutes.cmp(&other.minutes))
            .then(self.seconds.cmp(&other.seconds))
            .then(self.milliseconds.cmp(&other.milliseconds))
    }
}

/// Errors that can occur when parsing a string to a duration
#[derive(Clone, Copy, Error, Debug, PartialEq, Eq)]
pub enum DurationError {
    /// The string is malformed.
    #[error("duration string malformed")]
    Malformed,
    /// The value is out of bounds when another, subsequent value is present (i.e. `01:61:48`).
    #[error("out of bounds value when greater order value is specified")]
    TooLarge,
}

impl FromStr for Duration {
    type Err = DurationError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = Scanner::new(s);
        Self::scan(&mut s, true)
    }
}

impl Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.days > 0 {
            write!(f, "{}:", self.days)?;
        }

        if self.hours > 0 {
            write!(f, "{:02}:", self.hours)?;
        }

        write!(f, "{:02}:{:02}", self.minutes, self.seconds)?;

        if self.milliseconds > 0 {
            write!(f, ",{:03}", self.milliseconds)?;
        }

        Ok(())
    }
}

impl HayagrivaValue for Duration {
    fn from_yaml(
        yaml: &Yaml,
        _: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        Self::from_str(yaml.as_deserialized_str()?).map_err(Into::into)
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        yaml_rust::Yaml::String(self.to_string())
    }

    fn explain() -> &'static str {
        "a duration string in the format DD:HH:MM:SS,MMM"
    }
}

/// An half-open interval of durations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DurationRange(Duration, Duration);

impl DurationRange {
    fn new(dur: Duration, dur2: Option<Duration>) -> Self {
        Self(dur, dur2.unwrap_or(dur))
    }

    fn scan(s: &mut Scanner) -> Result<Self, DurationError> {
        let dur = Duration::scan(s, false)?;
        let hyphens = s.eat_while('-');

        if hyphens.is_empty() {
            s.eat_whitespace();

            if s.done() {
                return Ok(Self(dur, dur));
            } else {
                return Err(DurationError::Malformed);
            }
        }

        let dur2 = Duration::scan(s, true)?;

        Ok(Self(dur, dur2))
    }
}

impl Display for DurationRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if self.0 != self.1 {
            write!(f, "-{}", self.1)?;
        }

        Ok(())
    }
}

impl FromStr for DurationRange {
    type Err = DurationError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = Scanner::new(s);
        Self::scan(&mut s)
    }
}

impl HayagrivaValue for DurationRange {
    fn from_yaml(
        yaml: &yaml_rust::Yaml,
        ctx: &mut ParseContext<'_>,
    ) -> Result<Self, DeserializationError>
    where
        Self: Sized,
    {
        match yaml {
            Yaml::String(s) => {
                let range = Self::from_str(s)?;
                Ok(range)
            }
            Yaml::Hash(h) => {
                let from = h.get_with_str("from", ctx)?.as_deserialized_str()?;
                let from = Duration::from_str(from)?;
                ctx.pop_dict_key();
                let to = h.get_with_str("to", ctx)?.as_deserialized_str().ok();
                let res = Ok(Self::new(from, to.map(Duration::from_str).transpose()?));
                ctx.pop_dict_key();
                res
            }
            _ => Err(Self::expected_error()),
        }
    }

    fn to_yaml(&self) -> yaml_rust::Yaml {
        Yaml::String(self.to_string())
    }

    fn explain() -> &'static str {
        "a duration string in the format DD:HH:MM:SS,MMM or a hash with keys \"from\" and \"to\""
    }
}

fn parse_int<R>(s: &mut Scanner, digits: R) -> Option<i32>
where
    R: std::ops::RangeBounds<usize>,
{
    s.eat_whitespace();

    let sign = s.eat_if(|c| c == '+' || c == '-');
    let positive = if sign { !s.before().ends_with('-') } else { true };

    s.eat_whitespace();
    let num = s.eat_while(char::is_numeric);
    if !digits.contains(&num.len()) {
        return None;
    }

    let num = num.parse::<i32>().unwrap() * if positive { 1 } else { -1 };
    Some(num)
}

fn parse_unsigned_int<T, R>(s: &mut Scanner, digits: R) -> Option<T>
where
    T: FromStr + Ord + Debug,
    <T as FromStr>::Err: Debug,
    R: std::ops::RangeBounds<usize>,
{
    s.eat_whitespace();
    let num = s.eat_while(char::is_numeric);
    if !digits.contains(&num.len()) {
        return None;
    }

    let num = num.parse::<T>().unwrap();
    Some(num)
}

/// Parse a string with a plus/minus and one to four digits into an integer.
fn parse_year(s: &mut Scanner) -> Result<i32, DateError> {
    parse_int(s, 4..=4).ok_or(DateError::UnknownFormat)
}

fn parse_short_year(s: &mut Scanner) -> Result<i32, DateError> {
    let year = parse_int(s, 2..=2).ok_or(DateError::UnknownFormat)?;
    let year = if year < 50 { year + 2000 } else { year + 1900 };
    Ok(year)
}

fn parse_month(s: &mut Scanner) -> Result<u8, DateError> {
    let month: u8 = parse_unsigned_int(s, 1..=2).ok_or(DateError::UnknownFormat)?;
    if !(1..=12).contains(&month) {
        return Err(DateError::MonthOutOfBounds);
    }

    Ok(month - 1)
}

fn parse_day(s: &mut Scanner) -> Result<u8, DateError> {
    let day: u8 = parse_unsigned_int(s, 1..=2).ok_or(DateError::UnknownFormat)?;
    if !(1..=31).contains(&day) {
        return Err(DateError::MonthOutOfBounds);
    }

    Ok(day - 1)
}

fn parse_month_with_year(s: &mut Scanner) -> Result<(i32, u8), DateError> {
    let year = parse_year(s).unwrap();
    s.eat_whitespace();

    if !s.eat_if('-') {
        return Err(DateError::UnknownFormat);
    }

    let month = parse_month(s).unwrap();

    s.eat_whitespace();
    if !s.done() {
        return Err(DateError::UnknownFormat);
    }

    Ok((year, month))
}

fn parse_full_date(s: &mut Scanner) -> Result<(i32, u8, u8), DateError> {
    let idx = s.cursor();
    let year = parse_short_year(s).or_else(|_| {
        s.jump(idx);
        parse_year(s)
    })?;

    s.eat_whitespace();
    if !s.eat_if('-') {
        return Err(DateError::UnknownFormat);
    }

    let month = parse_month(s)?;

    s.eat_whitespace();
    if !s.eat_if('-') {
        return Err(DateError::UnknownFormat);
    }

    let day = parse_day(s)?;

    if day + 1 > days_in_month(month, year) {
        return Err(DateError::DayOutOfBounds(day + 1));
    }

    s.eat_whitespace();
    if !s.done() {
        return Err(DateError::UnknownFormat);
    }

    Ok((year, month, day))
}

fn days_in_month(month: u8, year: i32) -> u8 {
    if month == 1 {
        if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
            29
        } else {
            28
        }
    } else if month < 7 {
        31 - month % 2
    } else {
        30 + month % 2
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duration_parse() {
        assert_eq!(
            Duration::from_str("01:00").unwrap(),
            Duration {
                days: 0,
                hours: 0,
                minutes: 1,
                seconds: 0,
                milliseconds: 0,
            }
        );
        assert_eq!(
            Duration::from_str("00:01:00").unwrap(),
            Duration {
                days: 0,
                hours: 0,
                minutes: 1,
                seconds: 0,
                milliseconds: 0,
            }
        );
        assert_eq!(
            Duration::from_str("00 : 00 : 01 : 00").unwrap(),
            Duration {
                days: 0,
                hours: 0,
                minutes: 1,
                seconds: 0,
                milliseconds: 0,
            }
        );
        assert_eq!(
            Duration::from_str("01:00,000").unwrap(),
            Duration {
                days: 0,
                hours: 0,
                minutes: 1,
                seconds: 0,
                milliseconds: 0,
            }
        );
        assert!(Duration::from_str("01:00,").is_err());
        assert!(Duration::from_str("010:00,").is_err());
    }

    #[test]
    fn test_duration_range_parse() {
        assert_eq!(
            DurationRange::from_str("01:00").unwrap(),
            DurationRange(
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 1,
                    seconds: 0,
                    milliseconds: 0,
                },
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 1,
                    seconds: 0,
                    milliseconds: 0,
                }
            )
        );
        assert_eq!(
            DurationRange::from_str("01:00-02:00").unwrap(),
            DurationRange(
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 1,
                    seconds: 0,
                    milliseconds: 0,
                },
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 2,
                    seconds: 0,
                    milliseconds: 0,
                }
            )
        );
    }
}
