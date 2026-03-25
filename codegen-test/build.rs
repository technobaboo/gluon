use gluon_codegen_rust::helpers::gen_multiple_modules;
use gluon_wire::Derives;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    gen_multiple_modules(
        &[
            (
                "types",
                Path::new("./gluon-binder-src/org.stardustxr.gluon.types.gluon"),
            ),
            (
                "test",
                Path::new("./gluon-binder-src/org.stardustxr.gluon.test.gluon"),
            ),
        ],
        &[],
        Derives::CLONE | Derives::PARTIAL_EQ | Derives::EQ | Derives::HASH,
        "protocol",
        "./src/protocol.rs",
    );
}
