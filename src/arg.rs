use lazy_static::lazy_static;
use regex::Regex;
use std::{iter::Peekable, str::CharIndices};

use crate::Error;

/// Iterates over the individual arguments in a command accounting for quoted arguments.
pub struct ArgumentTraverser<'cmd> {
    command: &'cmd str,
    chars: Peekable<CharIndices<'cmd>>,
    anchor: usize,
    index: usize,
}

impl<'cmd> ArgumentTraverser<'cmd> {
    /// Creates a traverser over the given command, stripping off the initial '/' if it exists.
    pub fn new(command: &'cmd str) -> Self {
        let mut traverser = ArgumentTraverser {
            command,
            chars: command.char_indices().peekable(),
            anchor: 0,
            index: 0,
        };
        traverser.skip_leading_spaces();
        traverser
    }

    /// Returns the remaining portion of the string being traversed, including the argument which
    /// was last read.
    pub fn gobble_remaining(&mut self) -> &'cmd str {
        self.index = self.command.len();
        &self.command[self.anchor ..]
    }

    /// Returns the remaining portion of the string being traversed from the current anchor
    /// position to the end of the string.
    pub fn gobble_remaining_truncated(&self, truncate_to: usize) -> &'cmd str {
        let mut end = self.anchor;
        let mut spanned_chars = 0;
        let mut chars = self.command[self.anchor ..].chars();
        while spanned_chars < truncate_to {
            match chars.next() {
                Some(ch) => {
                    end += ch.len_utf8();
                    spanned_chars += 1;
                }
                None => break,
            }
        }

        &self.command[self.anchor .. end]
    }

    /// Returns whether or not this traverser has more arguments. If this function returns true, then
    /// `next` will not return `None`.
    pub fn has_next(&self) -> bool {
        self.index < self.command.len()
    }

    /// Returns the next argument in the string being traversed, or `None` if no arguments remain.
    pub fn next(&mut self) -> Option<&'cmd str> {
        if self.index >= self.command.len() {
            return None;
        }

        self.skip_leading_spaces();

        self.anchor = self.index;

        // Single or double quotes
        let mut quote_type: char = '\0';
        // Whether we're in quotes and should ignore spaces
        let mut in_quotes = false;
        // Used for escaping quotes with the '\' character
        let mut ignore_quote = false;

        self.index = loop {
            if !in_quotes {
                match self.chars.peek().copied() {
                    Some((index, ' ')) => break index,
                    _ => {}
                }
            }

            let ch = match self.chars.next() {
                Some((_, ch)) => ch,
                None => break self.command.len(),
            };

            // Manage strings
            if (ch == '\'' || ch == '\"') && !ignore_quote {
                if in_quotes {
                    if ch == quote_type {
                        in_quotes = false;
                    }
                } else {
                    quote_type = ch;
                    in_quotes = true;
                }
            }

            // Unset the ignore quote variable
            if ignore_quote {
                ignore_quote = false;
            }

            // Set the ignore quote variable if the escape character is present
            if ch == '\\' {
                ignore_quote = true;
            }
        };

        Some(&self.command[self.anchor .. self.index])
    }

    fn skip_leading_spaces(&mut self) {
        while matches!(self.chars.peek().copied(), Some((_, ' '))) {
            self.index += 1;
            self.chars.next();
        }
    }
}

/// Trait for matching and converting string arguments to concrete types. Any type which implements this
/// trait can be used as a node type in a command module.
pub trait FromArgument<'a, C>: Sized {
    /// Returns whether or not the given argument matches the format of `Self`. Returning true does
    /// not imply that [`from_arg`](crate::arg::FromArgument::from_arg) will succeed, but it does
    /// imply that the argument could succeed given the correct context. In other words, this function
    /// should return true for any input that is in the set of all possible string representations
    /// of `Self` given all possible contexts.
    fn matches(arg: &str) -> bool;

    /// This function follows similar rules to [`matches`](crate::arg::FromArgument::matches), however
    /// it is not guaranteed that a full argument will be given to this function. This test is primarily
    /// used in suggestion generation while the user is part-way through typing an argument, and hence
    /// should return true whenever [`matches`](crate::arg::FromArgument::matches) returns true, and
    /// should also return true for every truncation of every input for which
    /// [`matches`](crate::arg::FromArgument::matches) returns true.
    fn partial_matches(partial_arg: &str) -> bool {
        Self::matches(partial_arg)
    }

    /// Parses the given argument given a context. This function should only fail if it is impossible
    /// to construct a valid `Self` from the given string and context. Any logical checks should be
    /// performed in execution blocks.
    fn from_arg(arg: &'a str, context: &C) -> Result<Self, Error>;
}

macro_rules! impl_from_arg_for_int {
    ($int:ty) => {
        impl<'a, C> FromArgument<'a, C> for $int {
            fn matches(arg: &str) -> bool {
                arg.chars().all(|ch| ch.is_digit(10))
            }

            fn from_arg(arg: &'a str, _ctx: &C) -> Result<Self, Error> {
                arg.parse::<$int>().map_err(|e| e.to_string())
            }
        }
    };
}

impl_from_arg_for_int!(u8);
impl_from_arg_for_int!(i8);
impl_from_arg_for_int!(u16);
impl_from_arg_for_int!(i16);
impl_from_arg_for_int!(u32);
impl_from_arg_for_int!(i32);
impl_from_arg_for_int!(u64);
impl_from_arg_for_int!(i64);
impl_from_arg_for_int!(u128);
impl_from_arg_for_int!(i128);
impl_from_arg_for_int!(usize);
impl_from_arg_for_int!(isize);

macro_rules! impl_from_arg_fo_float {
    ($float:ty, $full_regex:literal, $partial_regex:literal) => {
        impl<'a, C> FromArgument<'a, C> for $float {
            fn matches(arg: &str) -> bool {
                lazy_static! {
                    static ref FULL: Regex = Regex::new($full_regex).unwrap();
                }

                FULL.is_match(arg)
            }

            fn partial_matches(partial_arg: &str) -> bool {
                lazy_static! {
                    static ref PARTIAL: Regex = Regex::new($partial_regex).unwrap();
                }

                if partial_arg.is_empty() {
                    return true;
                }

                PARTIAL.is_match(partial_arg)
            }

            fn from_arg(arg: &'a str, _ctx: &C) -> Result<Self, Error> {
                let suffix_index = arg
                    .char_indices()
                    .rev()
                    .find(|&(_, ch)| !ch.is_digit(10) && ch != '.')
                    .map(|(index, _)| index)
                    .unwrap_or(arg.len());

                let trimmed = &arg[.. suffix_index];
                if trimmed.is_empty() {
                    return Err(format!("Invalid float: hanging suffix \"{}\"", arg));
                }

                if trimmed == "." {
                    return Err(format!("Invalid float: hanging point"));
                }

                let first = trimmed.chars().next().unwrap();
                let last = trimmed.chars().last().unwrap();

                if first == '.' {
                    ("0".to_owned() + trimmed)
                        .parse::<$float>()
                        .map_err(|e| e.to_string())
                } else if last == '.' {
                    (trimmed.to_owned() + "0")
                        .parse::<$float>()
                        .map_err(|e| e.to_string())
                } else {
                    trimmed.parse::<$float>().map_err(|e| e.to_string())
                }
            }
        }
    };
}

impl_from_arg_fo_float!(
    f32,
    r"^(\d+\.\d+|\d+\.|\.\d+|\d+)[fF]?$",
    r"^(\d+\.|\.\d*|\d+)[fF]?"
);
impl_from_arg_fo_float!(
    f64,
    r"^(\d+\.\d+|\d+\.|\.\d+|\d+)[dD]?$",
    r"^(\d+\.|\.\d*|\d+)[dD]?"
);

impl<'a, C> FromArgument<'a, C> for String {
    fn matches(_arg: &str) -> bool {
        true
    }

    fn from_arg(arg: &'a str, _ctx: &C) -> Result<Self, Error> {
        if arg.len() < 2 {
            Ok(arg.to_owned())
        } else {
            let first = arg.chars().next().unwrap();
            let last = arg.chars().last().unwrap();

            if first == last && (first == '"' || first == '\'') {
                Ok(arg[1 .. arg.len() - 1].to_owned())
            } else {
                Ok(arg.to_owned())
            }
        }
    }
}

impl<'a, C> FromArgument<'a, C> for &'a str {
    fn matches(_arg: &str) -> bool {
        true
    }

    fn from_arg(arg: &'a str, _ctx: &C) -> Result<Self, Error> {
        if arg.len() < 2 {
            Ok(arg)
        } else {
            let first = arg.chars().next().unwrap();
            let last = arg.chars().last().unwrap();

            if first == last && (first == '"' || first == '\'') {
                Ok(&arg[1 .. arg.len() - 1])
            } else {
                Ok(arg)
            }
        }
    }
}

impl<'a, C> FromArgument<'a, C> for bool {
    fn matches(arg: &str) -> bool {
        arg == "true" || arg == "false"
    }

    fn partial_matches(partial_arg: &str) -> bool {
        "true".starts_with(partial_arg) || "false".starts_with(partial_arg)
    }

    fn from_arg(arg: &str, _ctx: &C) -> Result<Self, Error> {
        match arg {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => Err(format!(
                "\"{}\" is not a valid boolean, must be \"true\" or \"false\"",
                arg
            )),
        }
    }
}

impl<'a, C> FromArgument<'a, C> for char {
    fn matches(arg: &str) -> bool {
        arg.chars().count() == 1
    }

    fn partial_matches(partial_arg: &str) -> bool {
        partial_arg.chars().count() <= 1
    }

    fn from_arg(arg: &str, _ctx: &C) -> Result<Self, Error> {
        if arg.is_empty() {
            Err("Cannot parse an empty argument into a character".to_owned())
        } else {
            let mut chars = arg.chars();
            let ch = chars.next().unwrap();
            match chars.next() {
                Some(_) => Err(format!(
                    "Cannot parse \"{}\" into a character, length is greater than one",
                    arg
                )),
                None => Ok(ch),
            }
        }
    }
}

impl<'a, C, T> FromArgument<'a, C> for Option<T>
where
    T: FromArgument<'a, C>,
{
    fn matches(arg: &str) -> bool {
        T::matches(arg)
    }

    fn partial_matches(partial_arg: &str) -> bool {
        T::partial_matches(partial_arg)
    }

    fn from_arg(arg: &'a str, context: &C) -> Result<Self, Error> {
        Ok(Some(T::from_arg(arg, context)?))
    }
}
