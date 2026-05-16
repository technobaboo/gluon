use gluon_codegen_rust::helpers::gen_multiple_modules;
use gluon_codegen_rust::{Derives, TypeProxy};
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
        &[
            TypeProxy {
                protocol_type_name: "test::TestEnum".into(),
                rust_type: "crate::MyTestEnum".into(),
                derives: Derives::empty(),
            },
            TypeProxy {
                protocol_type_name: "test::Color".into(),
                rust_type: "crate::MyColor".into(),
                derives: Derives::COPY | Derives::CLONE | Derives::HASH | Derives::PARTIAL_EQ | Derives::EQ,
            },
            TypeProxy {
                protocol_type_name: "types::Vec3".into(),
                rust_type: "crate::MyVec3".into(),
                derives: Derives::COPY | Derives::CLONE | Derives::PARTIAL_EQ,
            },
        ],
        "./src/protocol",
    );
}
