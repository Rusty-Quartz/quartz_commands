#![warn(missing_docs)]
#![feature(proc_macro_diagnostic)]

//! This crate contains the function-like procedural macro which parses the custom command syntax
//! and converts it into viable rust code.

extern crate proc_macro;

mod gen;
mod keyword;
mod parse;

use proc_macro2::Literal;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Error, Fields, TypePath};

#[allow(missing_docs)]
#[proc_macro]
pub fn module(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module = parse_macro_input!(item as parse::CommandModule);
    match gen::generate_module(module) {
        Some(output) => output.into(),
        None => proc_macro::TokenStream::new(),
    }
}

#[allow(missing_docs)]
#[proc_macro_derive(FromArgument)]
pub fn derive_from_argument(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let data_enum = match input.data {
        Data::Enum(data_enum) => data_enum,
        _ =>
            return Error::new_spanned(
                input,
                "FromArgument can only be derived on enums whose variants are field-less.",
            )
            .to_compile_error()
            .into(),
    };

    let mut arg_literals = Vec::new();
    let mut match_arms = Vec::new();
    for variant in &data_enum.variants {
        if !matches!(&variant.fields, Fields::Unit) {
            return Error::new_spanned(variant, "Variants cannot have fields.")
                .to_compile_error()
                .into();
        }

        arg_literals.push(Literal::string(&pascal_to_kebab(
            &variant.ident.to_string(),
        )));
        let arg_repr = arg_literals.last().unwrap();
        let variant_name = &variant.ident;

        match_arms.push(quote! {
            #arg_repr => ::core::result::Result::Ok(Self::#variant_name)
        });
    }

    let ident = &input.ident;
    let ident_str = Literal::string(&ident.to_string());

    // Since all the variants are field-less, we don't need to worry about copying over generics.
    (quote! {
        impl<C> ::quartz_commands::FromArgument<'_, C> for #ident {
            fn matches(arg: &str) -> bool {
                [#( #arg_literals ),*].iter().any(|&lit| arg == lit)
            }

            fn partial_matches(partial_arg: &str) -> bool {
                [#( #arg_literals ),*].iter().any(|lit| lit.starts_with(partial_arg))
            }

            fn from_arg(arg: &str, _context: &C) -> Result<Self, ::quartz_commands::Error> {
                match arg {
                    #( #match_arms, )*
                    _ => ::core::result::Result::Err(format!("\"{}\" does not match any variant in enum {}", arg, #ident_str))
                }
            }
        }
    }).into()
}

fn path_matches(path: &TypePath, ident: &str) -> bool {
    path.qself.is_none()
        && path.path.leading_colon.is_none()
        && !path.path.segments.is_empty()
        && path.path.segments.last().unwrap().ident == ident
}

fn pascal_to_kebab(pascal: &str) -> String {
    if pascal.is_empty() {
        return String::new();
    }

    let mut result = String::with_capacity(pascal.len());
    let chars = pascal.chars();
    for ch in chars {
        if ch.is_uppercase() && !result.is_empty() {
            result.push('-');
        }

        result.extend(ch.to_lowercase());
    }

    result
}
