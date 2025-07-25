mod context;
mod debug_info;
pub mod error;
mod generate;
mod tcb;

#[derive(Clone)]
pub struct Config {
    pub is_debug: bool,
    pub compile_only: bool,
    pub prefix_symbols: bool,
    pub mod_name: String,
    pub filename: std::path::PathBuf,
}

pub use generate::CodeGen;
