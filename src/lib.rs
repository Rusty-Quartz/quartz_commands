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
# use quartz_commands::module;
module! {
    // The macro will convert the specified module name to pascal case (in this case it
    // converts `example` to `Example`).
    mod example;
    type Context = ();

    command node_example
    where a: i32 => b: i32
    {
        // Ignore this for now, it's to make the compiler happy
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
generated parser can be accessed through a generated unit struct named `Example`. A command can
then be dispatched as follows:

```
# quartz_commands::module! {
#     mod example;
#     type Context = ();
#
#     command node_example
#     where a: i32 => b: i32
#     {
#         b executes |_ctx| Ok(());
#     }
# }
use quartz_commands::CommandModule;
assert!((Example).dispatch("node_example 1 2", ()).is_ok());
```

The trait [`CommandModule`] must be in scope in order to call `dispatch` and/or
`get_suggestions` on the generated parser. Calling `dispatch` will parse the arguments and call the
appropriate execution block. Calling `get_suggestions` will attempt to parse the arguments, and
attempt to match the arguments against a suggestion block. Default suggestions for literals will
be generated, for example:

```
# quartz_commands::module! {
#     mod example;
#     type Context = ();
#
#     command node_example
#     where a: i32 => b: i32
#     {
#         b executes |_ctx| Ok(());
#     }
# }
use quartz_commands::CommandModule;
assert_eq!((Example).get_suggestions("node_ex", &()), ["node_example"]);
```

The rest of this documentation page goes into an in-depth explanation of each component of this
crate.

# Types of Nodes

There are three main types of nodes: arguments, literals, and implied nodes. Argument nodes are given
a rust type which implements [`FromArgument`], and "take on" a value during parsing. These values
are stored in a generated, hidden struct which contains fields for all argument nodes in a command.
Literal nodes are just string literals, and only add lexical information. They are used for adding
"grammar" to commands. For example, `a: i32 => "then" => b: i32` will force the user to type the
word "then" between arguments `a` and `b`. Even though these nodes do not take on a value, they
can still be bound to executors or suggestion generators. Implied nodes are a little more
theoretical, but the only one which users need to care about is the root node of each command,
which can be referred to by the identifier `root`. The most common use for this is for binding the
root to an executor in the instance where a command is just a single literal.

## Argument Nodes

Argument nodes are required to have a name and type, but can also be given a default value, or be
marked as `greedy`. Default argument nodes are always well-defined since they are guaranteed to
always have a value, so they can be used in any execution or suggestion block. To specify a default
value, simply add an assignment operation to the node definition.
```
# use quartz_commands::module;
module! {
    mod example;
    type Context = ();

    command default_example
    where default_node: i32 = 0
    {
        root executes |_ctx| {
            println!("Default value for node is {}", default_node);
            Ok(())
        };

        default_node executes |_ctx| {
            println!("default_node assigned new value: {}", default_node);
            Ok(())
        };
    }
}
```

Greedy nodes consume the rest of the command buffer in its entirety, ergo they must terminate a
branch. Currently only `String` and `&'cmd str` can be declared as greedy. An example is shown
below (with the boilerplate elided):
```
# use quartz_commands::module;
# module! {
# mod example;
# type Context = ();
command echo
where msg: greedy &'cmd str
{
    msg executes |_ctx| {
        println!("{}", msg);
        Ok(())
    };
}
# }
```

## Literal Nodes

Literal nodes, as mentioned before, mostly just add grammar to a command. Literal nodes must only
contain non-whitespace ASCII characters, and cannot contain single or double quotes, or
backslashes. Literals can also be renamed to help add clarity to more complex commands.
```
# use quartz_commands::module;
# module! {
# mod example;
# type Context = ();
command renamed_literal
where "normal" => "renamed" as new_name
{
    "normal" executes |_ctx| {
        println!("Refer to regular literals like this");
        Ok(())
    };

    new_name executes |_ctx| {
        println!("It works!");
        Ok(())
    };
}
# }
```
Note that, once a literal is renamed, it must be referred to by its new name:
```compile_fail
# use quartz_commands::module;
# module! {
# mod example;
# type Context = ();
# command renamed_literal
# where "renamed" as new_name
# {
// Illegal, use `new_name` instead
"renamed" executes |_ctx| {
    println!("This doesn't work!");
    Ok(())
};
# }
# }
```

## Implied Nodes

Savvy readers may have gathered at this point that all of this talk about nodes is because a
command module is really a directed graph, and the generated parser defines the legal paths through
that graph. The two main implied node types are the root of each command, and the module root. The
command root can be referred to within the module with the identifier `root`. This node
doesn't have any special behavior other then that it cannot have explicitly defined successors.
For example, the first snippet below is legal, whereas the second is not.
```
# quartz_commands::module! {
# mod example;
# type Context = ();
command legal
where "foo" => root
{}
# }
```

```compile_fail
# quartz_commands::module! {
# mod example;
# type Context = ();
command illegal
// Just do "foo" => root, "bar" instead
where "foo" => root => "bar"
{}
# }
```

The implied module node is one with edges directed toward the root of each defined command. This
node has no special syntax, since it is essentially just the generated module, which is accessed
through the corresponding generated unit struct.

## The `any` "node"

For large commands, it is useful to specify a bunch of successors in a compact form. This is what
the `any` node was created for. An example is shown below:
```
# quartz_commands::module! {
# mod example;
# type Context = ();
command syntax_shortcut
where x: f64 => any ["add", "subtract"]
{
    // Implementation not shown..it's just an example
    "add" executes |_ctx| Ok(());
    "subtract" executes |_ctx| Ok(());
}
# }
```

The most important thing to understand about this node type is that it's just syntax sugar. All
`any` nodes are flattened out during command compilation. If you need to "capture" which node
was visited, then consider implementing or deriving [`FromArgument`] on an enum.

`any` nodes can be reused by giving them a name when they are first defined, and can also be used
to bind multiple nodes to an executor.
```
# quartz_commands::module! {
# mod example;
# type Context = ();
command flags_example
where
    any flag ["-a", "-b", "-c"],
    "-a" => flag,
    "-b" => b: i32 => flag,
    "-c" => c: String => flag
{
    any ["-a", b, c] executes |_ctx| Ok(());
}
# }
```
Note that, by default, a user will receive an error for visiting the same node twice. Node cycles
are discussed in greater detail in a later section.

# The Module Environment

Internally, command parsing is stateful (in order to keep track of arguments), however this crate's
abstractions make it stateless to you, the user. The `Context` type can optionally be used to
give more information to the command for execution and suggestion generation. Currently, the
context cannot be edited during parsing, however, in the future we may add the ability to define
additional state information for individual commands.

## Lifetimes

There are two main reference lifetimes associated with command dispatching: `'cmd` and `'ctx`.
The implicit lifetime on the `&str` passed to the dispatch function on the module is called `'cmd`
within the parser. Hence, if an argument wishes to borrow from the original command string, it must
use the lifetime `'cmd`. This allows for zero-copy parsing of string type arguments, such as
`&'cmd str` and [`Help`]`<'cmd>`.

The `'ctx` lifetime can be optionally defined with the context type, and is mostly used to just
keep `rustc` happy. If this lifetime is specified, then the implementation of [`CommandModule`]
on the generated struct will be generic over `'ctx`.

## The `Context` type

The primary use of this type is to pass additional information to executors and suggestion
generators. Executors receive an owned context, whereas suggestion generators receive a
shared reference to a context. If the context type is to have a lifetime, it must be called `'ctx`.
This is just to simplify parsing and code generation, and restriction will likely be lifted in
future versions of this crate.

All argument types used in the command definition must implement [`FromArgument`] for the context
type specified in the module root. If you wish to add additional argument types, you may implement
`FromArgument` for your specific context type, and use it to validate or parse more complex
arguments in advance of execution. Currently, it is not possible to store arguments which borrow
from the context provided to implementors of `FromArgument`, and this is unlikely to change in
the future.

# Command and Branch Definitions

Command definitions are prefixed by the keyword `command`, and branch definitions are prefixed
by the keyword `where`. Command names can be followed by a number of aliases, as shown below:
```
# quartz_commands::module! {
# mod example;
# type Context = ();
command alias_example | alias1 | alias2
{
    root executes |_ctx| Ok(());
}
# }
```

Commands are not required to have branches, however any command which takes arguments will require
branches. As you may have surmised, branches are constructed by linking nodes with fat arrows
(`=>`). Branches are read left-to-right and top-to-bottom, and nodes are considered "defined" in
the order that they are encountered. Duplicate definitions, including semantically-equivalent
redundant definitions, are disallowed and will be caught by the command compiler. After a node is
defined, henceforth it must be referred to by its name (including if it's renamed) or literal
value.

After branches follows the body of the command, which contains handler bindings. Handler bindings
simply tell the command compiler the code that nodes execute, as well as the suggestions that nodes
give based on the command context and the last argument in the buffer. An example of each is shown
below:
```
# quartz_commands::module! {
# mod example;
# type Context = ();
command bindings_example
{
    root executes |context| {
        // Do something with context
        Ok(())
    };

    root suggests |context, arg| {
        // Do something with arg and context to make a list of suggestions
        Vec::<String>::new()
    };
}
# }
```

All rust code is valid within execution or suggestion blocks. All in-scope imported items will be
the same as those which are valid for functions defined in the same scope as the module. The only
artifacts from the parser which are in-scope and could cause weird behavior are `__data.__visited`
and `__args`, so just don't touch those. Arguments which are guaranteed to have a well-defined
value can be accessed through their name, as shown in the example below. Arguments which are not
guaranteed to have a well-defined value are still in-scope, however they are wrapped in an option
which must be checked or unwrapped.

**Note:** all arguments are wrapped in an option in suggestion blocks. The parser takes a fairly
lossy approach when generating suggestions, so you can never be sure an argument was fully parsed.

```
# quartz_commands::module! {
# mod example;
# type Context = ();
command add
where
    // See the cycles section under semantics for why `a` needs to be transient
    transient a: i32 => b: i32 => a
{
    b executes |_ctx| {
        // Since we got to `b`, we know `a` also exists
        println!("a + b = {}", a + b);
        Ok(())
    };

    a executes |_ctx| {
        // Cycles are hard! We don't know if `b` is defined, since the user could have
        // typed out "add 10", meaning there is no value for `b`, or "add 1 2 3",
        // meaning `b` exists
        println!("a is {}. Is b present? {}", a, b.is_some());
        Ok(())
    };
}
# }
```

Executors must return a `Result<(), `[`quartz_commands::Error`](crate::Error)`>`, and suggestion
generators must return a type which implements `IntoIter<Item = T>` where `T` dereferences to `str`
and implements `Into<String>`. The side effect of this generality is that the compiler may give
fairly unintelligible errors about type annotations needed if you don't give enough type hints to
the compiler about `T`. That's why the example above has to specify `Vec::<String>`, for instance.
This may be changed in the future to require a concrete type.

Currently, the parser will auto-generate suggestion functions for literals. These suggesters simply
suggest the appropriate literals (if any) which match against the last argument in the provided
command string. Because of this, you should almost never have to manually implement a suggester
for literals.

Implementation note: currently, the bodies of execution and suggestion blocks are directly inlined
into the generated parser's functions. This behavior is not guaranteed and may change in the
future, but if code bloat is a concern then you can delegate executors and suggesters to regular
functions defined outside the module.

# Semantics

This section deals with a discussion of node graphs, including what makes them valid or degenerate.
It may get a little complicated, however the command compiler currently does a decent job at
identifying incorrect semantics and giving useful hints for correcting those semantics. If the
validator fails however, then the error `rustc` generates will almost certainly be cryptic and
difficult to debug. Improving error messages will be a constant battle for this crate, however
they will only improve over time. If you want to contribute to this project to help with this,
we would greatly appreciate it.

## Successors

The first stage of command compilation involves flattening out all the branches into pairs of nodes
and their successors. This process ensures that all successors to a given node are valid. Invalid
successors include:
 - A successor to a `greedy` argument
 - An explicit root successor
 - Duplicate successors

This is also the time at which all `any` nodes are flattened out into their contents, and that
node definitions are ensured to be unique. By the end of this stage, the implied graph from all
the branches the user specified is considered valid.

## Well-defined Arguments

The next stage of command compilation tracks all of the non-default argument nodes to see which
arguments are "well-defined" at each node in the graph. An argument is considered well-defined
at a node if it is guaranteed to have a value at that node. What this translates to internally,
since all argument values are stored as options, is that an `unwrap` on that argument value is
guaranteed to succeed. Because of this guarantee, the parser does this for you.

This step is performed by walking the graph at every allowed entry point, exiting on cycles,
and keeping track of which arguments we have passed along the way. With each pass, the nodes
visited have their defined arguments updated by removing those which are not defined on the
current path.

## Handler bindings

The last step, besides some more duplicate definition checking, is binding handlers to to nodes.
Handlers are simply executors or suggestion generators. The actual check for invalid no-op branches
occurs during code generation. An invalid no-op branch can be identified if a node is found without
an executor and without any successors. If such a node if found, the command compiler will throw
an error.

One should also bind a single handler to multiple nodes with prudence, since by necessity the
well-defined arguments for that handler are limited to the intersection of the sets of
well-defined arguments for all nodes to which it is bound.

## Cycles

The flexibility of command graphs allows for cycles, which is both a curse and a blessing.
By default, the parser will not allow for a node to be visited more than once at runtime. This is
to prevent pointlessly overwriting argument variables with a new value. This feature can be
opted-out of by declaring an argument or literal `transient`. Simply place this keyword in front
of the argument or literal definition as shown in the Command and Branch Definitions sections.

Tracking well-defined arguments in cycles is another difficult problem. To understand how this
works, imagine you are going to ride the hour hand of a clock but can only step on at noon. The
question we're trying to solve, and that the parser tries to solve, is this: when you hop off
the hour hand of the clock and land on a certain number, what numbers *must* you have passed over
to get to your current position. If you hop off at noon, you *could* have gone all the way around
the clock, or you could have just hopped off immediately, hence we can only be sure you passed over
noon. However if you get off at 11, then you must have passed over 12, 1, 2, etc. to get there,
because clocks only spin in one direction. The parser will handle this computation, and also the
more complicated case where there are multiple allowed entry points to the metaphorical clock,
using the method described in subsection on well-defined arguments.

Cycles also make it difficult to check for no-ops. Take the branch `"a" => "b" => "a"`, for example.
This clearly produces a degenerate graph since it will never execute anything, however the command
compiler will not produce an error on such a branch. It theoretically is possible to check for
a cycle where no node has an executor, however that is not required to produce a parser which
compiles and yields technically correct behavior, and we all know technically correct is the best
kind of correct.

[`CommandModule`]: crate::CommandModule
[`FromArgument`]: crate::FromArgument
[`Help`]: crate::Help
*/

mod arg;

pub use arg::*;

/// Parses Quartz's custom command syntax and generates a command module. See the crate-level
/// documentation for an extensive overview of how this works, and see the github repository
/// for examples.
pub use quartz_commands_macros::module;

/// Derives the trait FromArgument for enums whose variants are field-less.
///
/// ```
/// use quartz_commands::{ArgumentTraverser, FromArgument};
///
/// #[derive(FromArgument, PartialEq, Eq, Debug)]
/// enum ValidArgs {
///     Foo,
///     Bar,
///     FizzBuzz
/// }
///
/// // We need this because types are allowed to parse multiple arguments
/// let mut dummy = ArgumentTraverser::new("");
///
/// assert_eq!(ValidArgs::from_arg("foo", &mut dummy, &()), Ok(ValidArgs::Foo));
/// assert_eq!(ValidArgs::from_arg("fizz-buzz", &mut dummy, &()), Ok(ValidArgs::FizzBuzz));
/// ```
pub use quartz_commands_macros::FromArgument;

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
