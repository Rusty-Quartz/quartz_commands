# quartz_commands

Generates a parser at compile-time for handling commands similar in structure to those of Minecraft.
Using a custom-designed syntax, one can efficiently specify the format of a number of commands
within our `module!` macro.

This crate is the standalone command handler for [Quartz](https://github.com/Rusty-Quartz/Quartz),
a Minecraft server implementation in Rust.

For a real-world example, see Quartz's [proxy](https://github.com/Rusty-Quartz/quartz_proxy).

# Usage

Use the most recent version of this crate when adding it to your dependencies as shown below.
```toml
[dependencies]
quartz_commands = "0.1.0"
```

A simple example is shown below:
```rs
use quartz_commands::module;

module! {
    mod simple;
    type Context<'ctx> = &'ctx mut u32;

    command add
    where additional: u32,
    {
        additional executes |ctx| {
            let result = *ctx + additional;

            println!("{} + {} = {}", *ctx, additional, result);
            *ctx = result;

            Ok(())
        };

        additional suggests |_ctx, _arg| ["10", "20"].iter().copied();
    }
}

fn main() {
    use quartz_commands::CommandModule;

    let mut x: u32 = 5;
    assert!((Simple).dispatch("add 10", &mut x).is_ok());
    assert_eq!(x, 15);
}
```

View the documentation [here](https://docs.rs/quartz_commands) for more examples and an in-depth
explanation of the custom command syntax and how the generated parser works.
