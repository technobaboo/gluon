use gluon_codegen_rust::helpers::gen_multiple_modules;
use gluon_wire::Derives;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    gen_multiple_modules(
        &[
            (
                "types",
                Path::new("./gluon-binder-src/org.gluon.Types.gluon"),
            ),
            ("test", Path::new("./gluon-binder-src/org.gluon.Test.gluon")),
        ],
        &[],
        Derives::all(),
        "protocol",
        "./src/protocol",
    );
}
