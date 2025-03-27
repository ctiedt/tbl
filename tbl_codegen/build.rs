use std::{env, path::PathBuf};

fn main() {
    let bindings = bindgen::Builder::default()
        .header(format!(
            "{}/../tcb.h",
            env::var("CARGO_MANIFEST_DIR").unwrap()
        ))
        .generate()
        .unwrap();
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
