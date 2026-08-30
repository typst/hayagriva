use std::{fmt::Display, str::FromStr};

use thiserror::Error;
use unscanny::Scanner;

use hayagriva_core::{
    derive_or_from_str, deserialize_from_str, parse_unsigned_int, serialize_display,
};

/// A duration.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
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

serialize_display!(Duration);
deserialize_from_str!(Duration);

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
                    let str = format!("0.{num}");
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

    /// Get the duration in milliseconds.
    fn milliseconds(self) -> u64 {
        self.milliseconds as u64
            + self.seconds as u64 * 1000
            + self.minutes as u64 * 60 * 1000
            + self.hours as u64 * 60 * 60 * 1000
            + self.days as u64 * 24 * 60 * 60 * 1000
    }
}

impl PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Duration {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.milliseconds().cmp(&other.milliseconds())
    }
}

/// Errors that can occur when parsing a string to a duration
#[derive(Clone, Copy, Error, Debug, PartialEq, Eq, Hash)]
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

derive_or_from_str! {
    /// An half-open interval of durations.
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct DurationRange where "two durations separated by a hyphen or a map with a `from` and `to` field" {
        /// The start of the interval.
        pub start: Duration,
        /// The end of the interval.
        pub end: Duration,
    }
}

impl DurationRange {
    /// Create a new duration range.
    pub fn new(start: Duration, end: Option<Duration>) -> Self {
        Self { start, end: end.unwrap_or(start) }
    }

    fn scan(s: &mut Scanner) -> Result<Self, DurationError> {
        let start = Duration::scan(s, false)?;
        let hyphens = s.eat_while('-');

        if hyphens.is_empty() {
            s.eat_whitespace();

            if s.done() {
                return Ok(Self::new(start, None));
            } else {
                return Err(DurationError::Malformed);
            }
        }

        let end = Duration::scan(s, true)?;

        Ok(Self { start, end })
    }
}

impl Display for DurationRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.start)?;
        if self.start != self.end {
            write!(f, "-{}", self.end)?;
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

serialize_display!(DurationRange);

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
            DurationRange::new(
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 1,
                    seconds: 0,
                    milliseconds: 0,
                },
                None
            )
        );
        assert_eq!(
            DurationRange::from_str("01:00-02:00").unwrap(),
            DurationRange::new(
                Duration {
                    days: 0,
                    hours: 0,
                    minutes: 1,
                    seconds: 0,
                    milliseconds: 0,
                },
                Some(Duration {
                    days: 0,
                    hours: 0,
                    minutes: 2,
                    seconds: 0,
                    milliseconds: 0,
                })
            )
        );
    }
}
