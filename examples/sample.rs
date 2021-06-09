use quartz_commands::module;

module! {
    pub mod test;

    command teleport | tp
    where
        x: f64 => y: f64 => z: f64,
        entity: String,
    {
        x suggests |ctx| vec![ctx.player.position.x.to_string()];
        y suggests |ctx| vec![ctx.player.position.y.to_string()];
        z suggests |ctx| vec![ctx.player.position.z.to_string()];
        z executes |ctx| {
            ctx.player.teleport_to(x, y, z);
            Ok(())
        };

        entity suggests |ctx| ctx.server.something_about_listing_entities();
        entity executes |ctx| {
            ctx.player.teleport_to_entity(ctx.server.lookup_entity(&entity));
            Ok(())
        };
    }

    command anytest
    where
        input: String => any[i: i32, f: f32],
        f => "truncated"
    {
        input executes |_ctx| {
            println!("{}", input);
        };

        "truncated" executes |ctx| {
            println!("{:.1}", f);
        }
    }
}

fn main() {}

// struct CommandTestData {
//     num: Option<u32>,
// }

// impl Default for CommandTestData {
//     fn default() -> Self {
//         CommandTestData { num: None }
//     }
// }

// struct TestModule;

// impl TestModule {
//     fn dispatch_test_0<'a>(
//         args: ArgumentTraverser<'a>,
//         _context: (),
//         data: CommandTestData,
//     ) -> Result<(), String> {
//         if args.has_next() {
//             return Err(format!(
//                 "Unexpected additional arguments: \"{}\"",
//                 args.gobble_remaining_truncated(32)
//             ));
//         }

//         // Code from the execute branch
//         println!("Num is {}", data.num.unwrap());
//         Ok(())
//     }

//     fn dispatch_test<'a>(mut args: ArgumentTraverser<'a>, context: ()) -> Result<(), String> {
//         let mut data = CommandTestData::default();

//         match args.next() {
//             Some(arg) if <u32 as FromArgument<'a, ()>>::matches(arg) => {
//                 data.num = Some(FromArgument::<'a, ()>::from_arg(arg, &context)?);
//                 Self::dispatch_test_0(args, context, data)
//             }
//             Some(arg @ _) => Err(format!("Invalid argument \"{}\" expected u32", arg)),
//             None => Err(format!("Expected additional argument: u32")),
//         }
//     }

//     fn get_suggestions_test_0(_context: &(), _arg: &str, _data: &CommandTestData) -> Vec<String> {
//         vec!["5".to_owned(), "10".to_owned(), "15".to_owned()]
//     }

//     fn get_suggestions_test<'a>(mut args: ArgumentTraverser<'a>, context: &'a ()) -> Vec<String> {
//         let mut data = CommandTestData::default();

//         match args.next() {
//             Some(arg) if <u32 as FromArgument<'a, ()>>::partial_matches(arg) => {
//                 if args.has_next() {
//                     data.num = match FromArgument::<'a, ()>::from_arg(arg, context) {
//                         Ok(value) => Some(value),
//                         Err(_) => return Vec::new(),
//                     };

//                     // If there were more arguments we would suggest recursively here
//                     Vec::new()
//                 } else {
//                     Self::get_suggestions_test_0(context, arg, &data)
//                 }
//             }
//             Some(arg @ _) => Self::get_suggestions_test_0(context, arg, &data),
//             None => Vec::new(),
//         }
//     }
// }

// // In reality we should probably construct a hash map with function pointers so this is faster
// impl CommandModule<()> for TestModule {
//     fn dispatch(&self, command: &str, context: ()) -> Result<(), String> {
//         let mut args = ArgumentTraverser::new(command);
//         match args.next() {
//             Some("test") => Self::dispatch_test(args, context),
//             Some(_) => return Err("Invalid command".to_owned()),
//             None => Ok(()),
//         }
//     }

//     fn get_suggestions(&self, command: &str, context: &()) -> Vec<String> {
//         let mut args = ArgumentTraverser::new(command);
//         match args.next() {
//             Some("test") => Self::get_suggestions_test(args, context),
//             // Just list all the command names in the vec
//             _ => vec!["test"]
//                 .into_iter()
//                 .filter(|&cmd| cmd.starts_with(command))
//                 .map(|cmd| cmd.to_owned())
//                 .collect(),
//         }
//     }
// }