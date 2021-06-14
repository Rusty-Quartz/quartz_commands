use quartz_commands::module;
use quartz_commands::CommandModule;

module! {
    mod test;
    type Context<'ctx> = &'ctx mut u32;

    command simple
    where
        additional: u32 => any[more: u32, "log"],
    {
        additional executes |ctx| {
            *ctx += additional;
            println!("Additional: {}", additional);
            Ok(())
        };

        "log" executes |ctx| {
            *ctx += additional;
            println!("Value: {}", *ctx);
            Ok(())
        };

        more executes |ctx| {
            *ctx += additional + more;
            println!("Additional: {}, More: {}", additional, more);
            Ok(())
        };

        additional suggests |ctx, arg| {
            println!("{}", arg);
            vec!["10".to_owned(), "20".to_owned()]
        };
    }
}

fn main() {
    let mut x: u32 = 5;
    println!("{:?}", (Test).get_suggestions("simple 1", &&mut x));
}