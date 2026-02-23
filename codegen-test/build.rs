use std::fs;

use gluon_binder::idl_parser::parse_idl;
use gluon_codegen_rust::{gen_enum, gen_interface, gen_struct};
use quote::quote;

const PROTO: &str = include_str!("./gluon-binder-src/org.stardustxr.gluon.test.gluon");
fn main() {
    println!(
        "cargo:rerun-if-changed={}",
        "./gluon-binder-src/org.stardustxr.gluon.test.gluon"
    );
    println!("cargo:rerun-if-changed=../gluon-codegen-rust/src/lib.rs");
    let proto = parse_idl("Test", PROTO).unwrap();
    let interfaces = proto
        .interfaces
        .iter()
        .map(|(name, interface)| gen_interface(name, interface));
    let structs = proto.structs.iter().map(|(name, def)| gen_struct(def));
    let enums = proto.enums.iter().map(|(name, def)| gen_enum(def));
    let protocol = quote! {
        use gluon_wire::GluonConvertable;
        #(#interfaces)*
        #(#structs)*
        #(#enums)*
    };
    let str = protocol.to_string();
    let str = prettyplease::unparse(&syn::parse_file(&str).unwrap());
    fs::write("./src/protocol.rs", str).unwrap();
}
