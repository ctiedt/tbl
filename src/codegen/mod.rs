mod context;
mod debug_info;
mod generate;

#[derive(Clone)]
pub struct Config {
    pub is_debug: bool,
    pub filename: std::path::PathBuf,
    pub link_target: TargetPlatform,
}

pub use generate::CodeGen;

use crate::TargetPlatform;
