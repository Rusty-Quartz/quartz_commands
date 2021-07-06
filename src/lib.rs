#![warn(missing_docs)]

//!

mod arg;
mod module;

/// The crate's error type. This is subject to change in the future.
pub type Error = String;

pub use arg::*;
pub use module::*;
pub use qc_macros::*;
