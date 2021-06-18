#![warn(missing_docs)]
#![feature(proc_macro_diagnostic)]

//! This crate contains the function-like procedural macro which parses the custom command syntax
//! and converts it into viable rust code.

extern crate proc_macro;

mod gen;
mod keyword;
mod parse;

use gen::generate_module;
use syn::parse_macro_input;

/// Parses Quartz's custom command syntax and generates a command module.
#[proc_macro]
pub fn module(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module = parse_macro_input!(item as parse::CommandModule);
    generate_module(module).into()
}
