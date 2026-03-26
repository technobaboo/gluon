use crate::{ExternalProtocol, LocalProtocol, gen_module};
use convert_case::{Case, Casing};
use gluon_parser::parse_idl;
use gluon_wire::Derives;
use std::{fs, path::Path};

/// Generates each module into a separate file within a folder, with a `mod.rs` re-exporting all modules
pub fn gen_multiple_modules(
    modules: &[(&'static str, &Path)],
    external_protocols: &[&ExternalProtocol],
    requested_derives: Derives,
    output_dir: impl AsRef<Path>,
) {
    let output_dir = output_dir.as_ref();
    fs::create_dir_all(output_dir).unwrap();

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
            let mod_name = name.to_case(Case::Snake);
            (
                *name,
                LocalProtocol {
                    rust_module: format!("super::{mod_name}"),
                    protocol: parse_idl(gluon_filename, &proto_str).unwrap(),
                },
                mod_name,
            )
        })
        .collect::<Vec<_>>();

    let mut mod_names = Vec::new();
    for (name, proto, mod_name) in &modules {
        let other_mods = modules
            .iter()
            .filter(|v| &v.0 != name)
            .map(|v| &v.1)
            .collect::<Vec<_>>();
        let module = gen_module(proto, &other_mods, external_protocols, requested_derives);
        let str = prettyplease::unparse(&syn::parse2(module).unwrap());
        fs::write(output_dir.join(format!("{mod_name}.rs")), str).unwrap();
        mod_names.push(mod_name.clone());
    }

    // Generate mod.rs with pub mod declarations
    let mod_decls: String = mod_names
        .iter()
        .map(|name| format!("pub mod {name};\n"))
        .collect();
    fs::write(output_dir.join("mod.rs"), mod_decls).unwrap();
}
/// Generates a module into a single rust file
/// this assumes no imports are use in this module
pub fn gen_single_module(
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
    let proto_str = fs::read_to_string(&mod_path).unwrap();
    let gluon_filename = mod_path
        .as_ref()
        .file_name()
        .and_then(|f| f.to_str())
        .expect("gluon module path must have a valid filename");
    let proto = parse_idl(gluon_filename, &proto_str).unwrap();
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
