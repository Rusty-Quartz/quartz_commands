extern crate proc_macro;

mod keyword;
mod parse;

use quote::quote;
use syn::parse_macro_input;

#[proc_macro]
pub fn module(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module = parse_macro_input!(item as parse::CommandModule);
    (quote! {}).into()
}
