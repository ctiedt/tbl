mod context;
mod debug_info;
mod generate;

#[derive(Clone)]
pub struct Config {
    pub is_debug: bool,
    pub filename: std::path::PathBuf,
}

pub use generate::CodeGen;
