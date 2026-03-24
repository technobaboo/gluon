use gluon_codegen_rust::helpers::gen_single_module;

fn main() {
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    gen_single_module(
        "test",
        "./gluon-binder-src/org.stardustxr.gluon.test.gluon",
        "./src/protocol.rs",
    );
}
