use crate::{ExternalProtocol, LocalProtocol, gen_module};
use convert_case::{Case, Casing};
use gluon_parser::parse_idl;
use gluon_wire::Derives;
use quote::{format_ident, quote};
use std::{fs, path::Path};

/// Generates all modules into one large file with one inline rust module for each gluon module
pub fn gen_multiple_modules(
    modules: &[(&'static str, &Path)],
    external_protocols: &[&ExternalProtocol],
    requested_derives: Derives,
    output_rust_module: &'static str,
    output_file_path: impl AsRef<Path>,
) {
    for (_, path) in modules {
        println!(
            "cargo:rerun-if-changed={}",
            path.to_str().expect("path can't be turned into string")
        );
    }
    let modules = modules
        .iter()
        .map(|(name, path)| {
            let proto_str = fs::read_to_string(path).unwrap();
            let gluon_filename = path
                .file_name()
                .and_then(|f| f.to_str())
                .expect("gluon module path must have a valid filename");
            (
                *name,
                LocalProtocol {
                    rust_module: format!("{output_rust_module}::{name}"),
                    protocol: parse_idl(gluon_filename, &proto_str).unwrap(),
                },
            )
        })
        .collect::<Vec<_>>();
    let modules = modules.iter().map(|(name, proto)| {
        let other_mods = modules
            .iter()
            .filter(|v| v.0 != *name)
            .map(|v| &v.1)
            .collect::<Vec<_>>();
        let module = gen_module(proto, &other_mods, external_protocols, requested_derives);
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
/// this assumes no imports are use in this module
pub fn gen_single_module(
    mod_name: &'static str,
    mod_path: impl AsRef<Path>,
    requested_derives: Derives,
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
    let module = gen_module(
        &LocalProtocol {
            // this should only matter when getting imported by a local module
            rust_module: String::new(),
            protocol: proto,
        },
        &[],
        &[],
        requested_derives,
    );

    let str = prettyplease::unparse(&syn::parse2(module).unwrap());
    fs::write(output_file_path, str).unwrap();
}
