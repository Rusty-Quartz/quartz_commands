#![warn(missing_docs)]

/*!
Generates a parser at compile-time for handling commands similar in structure to those of
Minecraft. Using a custom-designed syntax, one can efficiently specify the format of a number
of commands within our [`module`](crate::module) macro.

# High-level Overview

In one sentence, this macro takes a series of command definitions as input, and outputs a
parser with a suggestion generator based on those command definitions. The fundamental unit
of a command module is a node. A node is simply an argument, literal, or other object which
may or may not have an executor or suggestion generator, and which may or may not have
successors. A successor is just a fancy name for a node that is allowed to come after another
node. Let's look at an example:

```
quartz_commands::module! {
    mod example;
    type Context = ();

    command node_example
    where a: i32 => b: i32
    {
        // Ignore this, it's to make the compiler happy
        b executes |_ctx| Ok(());
    }
}
```

`a` and `b` are nodes, more specifically, these are considered "argument nodes" because they
take on a value during parsing, in this case an `i32`. `b` is a successor to `a`, and the
entire statement `a: i32 => b: i23` is called a branch. The command compiler will look at all
the defined nodes and branches and generate a text parser for the given command. In this case,
the parser will expect the following arguments: the literal "node_examples" since that is the
name of the command, then an `i32` (argument `a`), then another `i32` (argument `b`). The
generated parser can be accessed through a generated unit struct named `Example`. The macro
will convert the specified module name to pascal case (in this case it converts `example` to
`Example`). A command can then be dispatched as follows:

```
# quartz_commands::module! {
#     mod example;
#     type Context = ();
# 
#     command node_example
#     where a: i32 => b: i32
#     {
#         // Ignore this, it's to make the compiler happy
#         b executes |_ctx| Ok(());
#     }
# }
use quartz_commands::CommandModule;
assert!((Example).dispatch("node_example 1 2", ()).is_ok());
```

The trait [`CommandModule`] must be in scope in order to call `dispatch` and/or
`get_suggestions` on the generated parser.

[`CommandModule`]: crate::CommandModule
*/

mod arg;

pub use arg::*;

/// Parses Quartz's custom command syntax and generates a command module. See the crate-level
/// documentation for an extensive overview of how this works, and see the github repository
/// for examples.
pub use qc_macros::module;

/// Derives the trait FromArgument for enums whose variants are field-less.
///
/// ```
/// use quartz_commands::FromArgument;
///
/// #[derive(FromArgument, PartialEq, Eq, Debug)]
/// enum ValidArgs {
///     Foo,
///     Bar,
///     FizzBuzz
/// }
///
/// assert_eq!(ValidArgs::from_arg("foo", &()), Ok(ValidArgs::Foo));
/// assert_eq!(ValidArgs::from_arg("fizz-buzz", &()), Ok(ValidArgs::FizzBuzz));
/// ```
pub use qc_macros::FromArgument;

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
