use std::{fs, path::Path};

use convert_case::{Case, Casing};
use gluon_parser::parse_idl;
use quote::{format_ident, quote};

use crate::gen_module;

/// Generates all modules into one large file with one inline rust module for each gluon module
pub fn gen_multiple_modules(modules: &[(&'static str, &Path)], output_file_path: impl AsRef<Path>) {
    for (_, path) in modules {
        println!(
            "cargo:rerun-if-changed={}",
            path.to_str().expect("path can't be turned into string")
        );
    }
    let modules = modules.iter().map(|(name, path)| {
        let proto_str = fs::read_to_string(path).unwrap();
        let proto = parse_idl(name, &proto_str).unwrap();
        let module = gen_module(&proto);
        let name = format_ident!("{}", name.to_case(Case::Snake));
        quote! {
            pub mod #name {
                #module
            }
        }
    });

    let str = prettyplease::unparse(&syn::parse2(quote! {#(#modules)*}).unwrap());
    fs::write(output_file_path, str).unwrap();
}
/// Generates a module into a single rust file
pub fn gen_single_module(
    mod_name: &'static str,
    mod_path: impl AsRef<Path>,
    output_file_path: impl AsRef<Path>,
) {
    println!(
        "cargo:rerun-if-changed={}",
        mod_path
            .as_ref()
            .to_str()
            .expect("path can't be turned into string")
    );
    let proto_str = fs::read_to_string(mod_path).unwrap();
    let proto = parse_idl(mod_name, &proto_str).unwrap();
    let module = gen_module(&proto);

    let str = prettyplease::unparse(&syn::parse2(module).unwrap());
    fs::write(output_file_path, str).unwrap();
}
