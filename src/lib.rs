#![warn(missing_docs)]

//!

mod arg;

pub use arg::*;
pub use qc_macros::*;

/// The crate's error type. This is subject to change in the future.
pub type Error = String;

/// Defines a command module, or a set of command definitions with an associated parser and
/// suggestion generator.
pub trait CommandModule<C> {
    /// Dispatches the given command for execution.
    fn dispatch(&self, command: &str, context: C) -> Result<(), Error>;

    /// Generates a list of suggestions to complete the final argument in the given command.
    fn get_suggestions(&self, command: &str, context: &C) -> Vec<String>;
}
