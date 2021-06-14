#[warn(missing_docs)]
mod arg;
mod module;

pub type ErrorType = String;

pub use qc_macros::module;
pub use module::*;
pub use arg::*;