//! Embeds the stdlib, since a browser has no directory to import it from.
//! Every `stdlib/*.oxi` is included, so a new module needs no change here.

use std::{env, fs, path::Path};

fn main() {
    let stdlib = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib");
    println!("cargo:rerun-if-changed={}", stdlib.display());

    let mut modules: Vec<_> = fs::read_dir(&stdlib)
        .expect("stdlib directory")
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "oxi"))
        .collect();
    modules.sort();

    let mut table = String::from("&[\n");
    for path in &modules {
        let name = path.file_stem().unwrap().to_string_lossy();
        let path = path.canonicalize().unwrap();
        table.push_str(&format!(
            "    ({name:?}, include_str!({:?})),\n",
            path.display().to_string()
        ));
    }
    table.push(']');
    let out = Path::new(&env::var("OUT_DIR").unwrap()).join("stdlib.rs");
    fs::write(out, table).unwrap();

    // The interpreter recurses natively for callbacks such as `map`; wasm's
    // default 1 MB stack overflowed well before the VM's own frame limit.
    if env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32") {
        println!("cargo:rustc-link-arg-cdylib=-zstack-size=16777216");
    }
}
