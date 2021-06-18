use crate::Error;

/// Defines a command module, or a set of command definitions with an associated parser and
/// suggestion generator.
pub trait CommandModule<C> {
    /// Dispatches the given command for execution.
    fn dispatch(&self, command: &str, context: C) -> Result<(), Error>;

    /// Generates a list of suggestions to complete the final argument in the given command.
    fn get_suggestions(&self, command: &str, context: &C) -> Vec<String>;
}
