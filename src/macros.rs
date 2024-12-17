/// A nice wrapper for handling errors in the `main()`.
/// Must be called from `main()`.
#[macro_export]
macro_rules! err {
    (@private, $result:expr, $format_string:literal, $exit_code:expr) => {
        match $result {
            Ok(v) => v,
            Err(err) => {
                eprintln!($format_string, err);
                return $exit_code;
            }
        }
    };
    ($result:expr, $exit_code:expr) => {
        err!(@private, $result, "{}", ExitCode::from($exit_code))
    };
    ($result:expr) => {
        err!(@private, $result, "{}", ExitCode::FAILURE)
    };
}

/// Like `err!()`, but requires a format string with `"{}"`.
/// Must be called from `main()`.
#[macro_export]
macro_rules! err_fmt {
    ($result:expr, $format_string:literal, $exit_code:literal) => {
        err!(@private, $result, $format_string, ExitCode::from($exit_code))
    };
    ($result:expr, $format_string:literal) => {
        err!(@private, $result, $format_string, ExitCode::FAILURE)
    };
}

/// Like `err!()`, but you can directly specify the error message with `&str`/`String`.
/// Must be called from `main()`.
#[macro_export]
macro_rules! err_str {
    ($error_string:expr, $exit_code:expr) => {
        err!(Err(Error::OtherError($error_string.into())), $exit_code)
    };
    ($error_string:expr) => {
        err_str!($error_string, ExitCode::FAILURE)
    };
}
