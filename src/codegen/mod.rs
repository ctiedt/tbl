mod context;
mod debug_info;
mod generate;

pub struct Config {
    pub is_debug: bool,
}

pub use generate::CodeGen;
