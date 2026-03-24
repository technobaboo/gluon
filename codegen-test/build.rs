use gluon_codegen_rust::gen_module;
use gluon_parser::parse_idl;
use std::fs;

const PROTO: &str = include_str!("./gluon-binder-src/org.stardustxr.gluon.test.gluon");
fn main() {
    println!("cargo:rerun-if-changed=./gluon-binder-src/org.stardustxr.gluon.test.gluon");
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    let proto = parse_idl("Test", PROTO).unwrap();
    let protocol = gen_module(&proto);
    let str = prettyplease::unparse(&syn::parse2(protocol).unwrap());
    fs::write("./src/protocol.rs", str).unwrap();
}
