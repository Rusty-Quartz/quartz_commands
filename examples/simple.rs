use quartz_commands::module;

module! {
    // The generated struct name will be `Simple`
    mod simple;

    // This field is required. If it has a lifetime, the lifetime must be named 'ctx
    type Context<'ctx> = &'ctx mut u32;

    // Declare a new command named `add`
    command add

    // Define the arguments it takes, and how certain arguments "lead" into other arguments
    where additional: u32,

    // Define the command logic
    {
        additional executes |ctx| {
            // `additional` is in-scope since it is guaranteed to be well defined if we
            // made it to this branch.
            let result = *ctx + additional;

            // All rust code is valid here.
            println!("{} + {} = {}", *ctx, additional, result);

            *ctx = result;

            // Execution blocks return Result<(), quartz_commands::Error>. Currently
            // the error type is just `String`.
            Ok(())
        };

        additional suggests |_ctx, _arg| {
            // Suggestions blocks need to return an iterator over types which dereference to str
            // and which implement Into<String>. The generated parser will filter out suggestions
            // that don't match the current argument in the buffer.
            ["10", "20"].iter().copied()
        };
    }
}

fn main() {
    // Make sure this trait is in scope when invoking the module
    use quartz_commands::CommandModule;

    let mut x: u32 = 5;
    assert!((Simple).dispatch("add 10", &mut x).is_ok());
    assert_eq!(x, 15);
}
