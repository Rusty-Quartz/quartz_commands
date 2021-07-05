use quartz_commands::module;
use quartz_commands::CommandModule;

module! {
    mod test;
    type Context<'ctx> = &'ctx mut u32;

    command simple
    where
        additional: u32,
    {
        additional executes |ctx| {
            *ctx += additional;
            println!("Additional: {}", additional);
            Ok(())
        };

        additional suggests |_ctx, _arg| {
            vec!["10".to_owned(), "20".to_owned()]
        };
    }

    command suggtest
    where
        "apple" => "orange"
    {
        "orange" executes |_ctx| Ok(());
    }
}

fn main() {
    let mut x: u32 = 5;
    println!("{:?}", (Test).get_suggestions("suggtest apple ", &&mut x));
}