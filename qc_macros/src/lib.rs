#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

mod gen;
mod keyword;
mod parse;

use gen::generate_module;
use syn::parse_macro_input;

#[proc_macro]
pub fn module(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module = parse_macro_input!(item as parse::CommandModule);
    generate_module(module).into()
}
