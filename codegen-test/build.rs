use std::fs;

use gluon_binder::idl_parser::parse_idl;
use gluon_codegen_rust::{gen_enum, gen_interface, gen_module, gen_struct};
use quote::quote;

const PROTO: &str = include_str!("./gluon-binder-src/org.stardustxr.gluon.test.gluon");
fn main() {
    println!(
        "cargo:rerun-if-changed={}",
        "./gluon-binder-src/org.stardustxr.gluon.test.gluon"
    );
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    let proto = parse_idl("Test", PROTO).unwrap();
    let protocol = gen_module(&proto);
    let str = protocol.to_string();
    let str = prettyplease::unparse(&syn::parse_file(&str).unwrap());
    fs::write("./src/protocol.rs", str).unwrap();
}
