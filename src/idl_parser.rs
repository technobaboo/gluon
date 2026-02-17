use chumsky::{prelude::*, text::keyword};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub path: String,
    pub alias: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Protocol {
    pub name: String,
    pub imports: Vec<ImportDecl>,
    pub imported_protocols: HashMap<String, Protocol>,
    pub interfaces: HashMap<String, Interface>,
    pub structs: HashMap<String, StructDef>,
    pub enums: HashMap<String, EnumDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub doc: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: String,
    pub doc: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub doc: Option<String>,
    pub fields: Vec<Field>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub doc: String,
    methods: Vec<Method>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    name: String,
    params: Vec<Field>,
    /// if none, this is a oneway function.
    /// if some but vec is empty, this is a void function.
    returns: Option<Vec<Field>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
    Fd,
    Ref(Option<String>),       // binder object reference
    Named(String),             // reference to a struct or enum by name
    Qualified(String, String), // namespace::TypeName from an import
    Array(Box<Type>, u32),
    Vec(Box<Type>),
    Set(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    Parse(String),
    CyclicImport(PathBuf),
    DuplicateAlias(String),
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Io(e) => write!(f, "IO error: {e}"),
            LoadError::Parse(e) => write!(f, "Parse error: {e}"),
            LoadError::CyclicImport(p) => write!(f, "Cyclic import: {}", p.display()),
            LoadError::DuplicateAlias(a) => write!(f, "Duplicate import alias: {a}"),
        }
    }
}

impl std::error::Error for LoadError {}

/// Derive the default alias from an import path.
/// Takes the filename, strips `.gluon`, splits by `.`, and takes the last segment.
pub fn default_alias(path: &str) -> String {
    let filename = path.rsplit('/').next().unwrap_or(path);
    let stem = filename.strip_suffix(".gluon").unwrap_or(filename);
    stem.rsplit('.').next().unwrap_or(stem).to_string()
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Protocol, extra::Err<Rich<'src, char>>> {
    // --- Import ---
    // Syntax:  import "path.gluon" [as alias]
    // Output:  ImportDecl { path, alias }
    let import_path = none_of("\"")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|s: &str| s.to_string())
        .delimited_by(just('"'), just('"'));

    let import_stmt = keyword("import")
        .padded()
        .ignore_then(import_path)
        .then(
            keyword("as")
                .padded()
                .ignore_then(text::ident().map(str::to_string))
                .or_not(),
        )
        .map(|(path, alias_opt): (String, Option<String>)| {
            let alias = alias_opt.unwrap_or_else(|| default_alias(&path));
            ImportDecl { path, alias }
        });

    // Syntax:  (import ...)*
    // Output:  Vec<ImportDecl>
    let imports = import_stmt.padded().repeated().collect::<Vec<ImportDecl>>();

    // --- Doc comment ---
    // Syntax:  /// text\n
    // Output:  String (trimmed line content)
    let doc_line = one_of(" \t")
        .repeated()
        .ignore_then(just("///"))
        .ignore_then(none_of("\n\r").repeated().to_slice().map(|s: &str| {
            let s = s.strip_prefix(' ').unwrap_or(s);
            s.trim_end().to_string()
        }))
        .then_ignore(text::newline());

    // Syntax:  (/// text\n)+
    // Output:  String (lines joined by \n, required)
    let req_doc_block = doc_line
        .repeated()
        .at_least(1)
        .collect::<Vec<std::string::String>>()
        .map(|lines| lines.join("\n"))
        .labelled("doc comment (/// ...)");

    // Syntax:  (/// text\n)*
    // Output:  Option<String> (lines joined by \n, or None if empty)
    let opt_doc_block = doc_line
        .repeated()
        .collect::<Vec<std::string::String>>()
        .map(|lines| {
            if lines.is_empty() {
                None
            } else {
                Some(lines.join("\n"))
            }
        });

    // Syntax:  // comment (not ///)
    // Output:  consumed and discarded
    let comment = just("//")
        .then(none_of("/\n\r").then(none_of("\n\r").repeated()).or_not())
        .or_not();

    // --- Type ---
    // Syntax:  bool | u8 | u16 | ... | String | Fd
    //          | Vec<T> | Set<T> | Map<K, V> | [T; N]
    //          | Ref[<name>] | Name | ns::Name
    // Output:  Type
    let method_name = text::ident().map(str::to_string).labelled("method");
    let type_parser = recursive(|p| {
        choice((
            just("bool").to(Type::Bool),
            just("u8").to(Type::U8),
            just("u16").to(Type::U16),
            just("u32").to(Type::U32),
            just("u64").to(Type::U64),
            just("i8").to(Type::I8),
            just("i16").to(Type::I16),
            just("i32").to(Type::I32),
            just("i64").to(Type::I64),
            just("f32").to(Type::F32),
            just("f64").to(Type::F64),
            just("String").to(Type::String),
            just("Fd").to(Type::Fd),
            just("Vec").ignore_then(
                p.clone()
                    .map(|t: Type| Type::Vec(Box::new(t)))
                    .delimited_by(just('<'), just('>')),
            ),
            just("Set").ignore_then(
                p.clone()
                    .map(|t: Type| Type::Set(Box::new(t)))
                    .delimited_by(just('<'), just('>')),
            ),
            just("Map").ignore_then(
                p.clone()
                    .then_ignore(just(',').padded())
                    .then(p.clone())
                    .map(|(k, v): (Type, Type)| Type::Map(Box::new(k), Box::new(v)))
                    .delimited_by(just('<'), just('>')),
            ),
            p.clone()
                .then_ignore(just(';').padded())
                .then(text::int(10).map(|s: &str| s.parse::<u32>().unwrap()))
                .map(|(t, n): (Type, u32)| Type::Array(Box::new(t), n))
                .delimited_by(just('['), just(']')),
            just("Ref")
                .then(
                    text::ident()
                        .map(str::to_string)
                        .then(
                            just("::")
                                .ignore_then(text::ident().map(str::to_string))
                                .or_not(),
                        )
                        .map(|(first, second)| match second {
                            Some(name) => Some(format!("{first}::{name}")),
                            None => Some(first),
                        })
                        .delimited_by(just('<').padded(), just('>').padded())
                        .or_not()
                        .map(|opt| opt.flatten()),
                )
                .map(|(_, name)| Type::Ref(name)),
            text::ident()
                .map(str::to_string)
                .then(
                    just("::")
                        .ignore_then(text::ident().map(str::to_string))
                        .or_not(),
                )
                .map(|(first, second)| match second {
                    Some(name) => Type::Qualified(first, name),
                    None => Type::Named(first),
                }),
        ))
        .padded()
        .labelled("type")
    });

    // --- Params ---
    // Syntax:  name: Type
    // Output:  Field { name, ty, doc: None }
    let param = text::ident()
        .map(str::to_string)
        .then_ignore(just(':').padded())
        .then(type_parser.clone())
        .map(|(name, ty)| Field {
            name,
            ty,
            doc: None,
        });

    // Syntax:  (param, param, ...)
    // Output:  Vec<Field>
    let params = param
        .padded()
        .separated_by(just(','))
        .collect::<Vec<Field>>()
        .delimited_by(just('('), just(')'))
        .padded();

    // Syntax:  -> (param, param, ...)
    // Output:  Option<Vec<Field>> (None = oneway, Some(vec![]) = void)
    let returns = just("->").padded().ignore_then(params.clone()).or_not();

    // --- Method ---
    // Syntax:  name(params) [-> (returns)] [// comment]
    // Output:  Method { name, params, returns }
    let method = method_name
        .then(params.clone())
        .then(returns)
        .map(|((name, params), returns)| Method {
            name,
            params,
            returns,
        })
        .then_ignore(comment);
    let methods = method.padded().repeated().collect::<Vec<Method>>();

    // --- Interface ---
    // Syntax:  /// doc\n  interface Name { method* }
    // Output:  (String, Interface)  — name and interface definition
    let interface = req_doc_block
        .then(
            one_of(" \t")
                .repeated()
                .ignore_then(keyword("interface"))
                .padded()
                .ignore_then(text::ident().map(str::to_string).padded())
                .then(methods.delimited_by(just('{'), just('}')).padded()),
        )
        .map(|(doc, (name, methods))| (name, Interface { doc, methods }));

    // --- Struct ---
    // Syntax:  [/// doc\n]  name: Type
    // Output:  Field { name, ty, doc }
    let struct_field = opt_doc_block
        .then(
            text::ident()
                .map(str::to_string)
                .padded()
                .then_ignore(just(':').padded())
                .then(type_parser.clone()),
        )
        .map(|(doc, (name, ty))| Field { name, ty, doc });
    let struct_fields = struct_field
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<Field>>();

    // Syntax:  /// doc\n  struct Name { field, field, ... }
    // Output:  StructDef { name, doc, fields }
    let struct_def = req_doc_block
        .then(
            one_of(" \t")
                .repeated()
                .ignore_then(keyword("struct"))
                .padded()
                .ignore_then(text::ident().map(str::to_string).padded())
                .then(
                    struct_fields
                        .clone()
                        .delimited_by(just('{').padded(), just('}').padded()),
                ),
        )
        .map(|(doc, (name, fields))| StructDef { name, doc, fields });

    // --- Enum ---
    // Syntax:  [/// doc\n]  Name [{ field, ... }]
    // Output:  EnumVariant { name, doc, fields }
    let variant_fields = struct_fields.delimited_by(just('{').padded(), just('}').padded());
    let enum_variant = opt_doc_block
        .then(
            text::ident()
                .map(str::to_string)
                .padded()
                .then(variant_fields.or_not()),
        )
        .map(|(doc, (name, fields))| EnumVariant {
            name,
            doc,
            fields: fields.unwrap_or_default(),
        });
    let enum_variants = enum_variant
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<EnumVariant>>();

    // Syntax:  /// doc\n  enum Name { variant, variant, ... }
    // Output:  EnumDef { name, doc, variants }
    let enum_def = req_doc_block
        .then(
            one_of(" \t")
                .repeated()
                .ignore_then(keyword("enum"))
                .padded()
                .ignore_then(text::ident().map(str::to_string).padded())
                .then(enum_variants.delimited_by(just('{').padded(), just('}').padded())),
        )
        .map(|(doc, (name, variants))| EnumDef {
            name,
            doc,
            variants,
        });

    // --- Top-level ---
    // Syntax:  import* (interface | struct | enum)*
    // Output:  Protocol { name, imports, interfaces, structs, enums }
    enum TopLevel {
        Interface(String, Interface),
        Struct(StructDef),
        Enum(EnumDef),
    }

    let top_level_item = choice((
        interface.map(|(name, iface)| TopLevel::Interface(name, iface)),
        struct_def.map(TopLevel::Struct),
        enum_def.map(TopLevel::Enum),
    ));

    imports
        .then(
            top_level_item
                .padded()
                .repeated()
                .collect::<Vec<TopLevel>>(),
        )
        .then_ignore(end())
        .map(|(imports, items)| {
            let mut interfaces = HashMap::new();
            let mut structs = HashMap::new();
            let mut enums = HashMap::new();
            for item in items {
                match item {
                    TopLevel::Interface(name, iface) => {
                        interfaces.insert(name, iface);
                    }
                    TopLevel::Struct(s) => {
                        structs.insert(s.name.clone(), s);
                    }
                    TopLevel::Enum(e) => {
                        enums.insert(e.name.clone(), e);
                    }
                }
            }
            Protocol {
                name: "".to_string(),
                imports,
                imported_protocols: HashMap::new(),
                interfaces,
                structs,
                enums,
            }
        })
}

pub fn parse_idl<'src>(name: &str, input: &'src str) -> Result<Protocol, Vec<Rich<'src, char>>> {
    let mut protocol = parser().parse(input).into_result()?;
    protocol.name = name.to_string();
    Ok(protocol)
}

#[test]
fn test_parse_idl() {
    let input = r#"
        /// A test interface
        interface Test {
            quit() // oneway transaction
            ping() -> () // transaction with empty reply, ensures it got it
            echo(input: String) -> (output: String) // just echo it back
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    assert_eq!(
        protocol,
        Protocol {
            name: "Test".to_string(),
            imports: vec![],
            imported_protocols: HashMap::new(),
            interfaces: HashMap::from([(
                "Test".to_string(),
                Interface {
                    doc: "A test interface".to_string(),
                    methods: vec![
                        Method {
                            name: "quit".to_string(),
                            params: vec![],
                            returns: None,
                        },
                        Method {
                            name: "ping".to_string(),
                            params: vec![],
                            returns: Some(vec![]),
                        },
                        Method {
                            name: "echo".to_string(),
                            params: vec![Field {
                                name: "input".to_string(),
                                ty: Type::String,
                                doc: None
                            }],
                            returns: Some(vec![Field {
                                name: "output".to_string(),
                                ty: Type::String,
                                doc: None
                            }]),
                        },
                    ],
                }
            ),]),
            structs: HashMap::new(),
            enums: HashMap::new(),
        }
    );
}

#[test]
fn test_parse_types() {
    let input = r#"
        /// File storage interface
        interface FileStore {
            upload(data: Vec<u8>, name: String, executable: bool) -> (handle: Fd)
            read(handle: Fd, offset: u64, length: u32) -> (data: Vec<u8>, bytes_read: u32)
            hash(handle: Fd) -> (sha256: [u8; 32])
            stat(handle: Fd) -> (size: u64, mode: u16, uid: u32, gid: u32)
            set_tags(handle: Fd, tags: Set<String>) -> ()
            get_metadata(handle: Fd) -> (metadata: Map<String, String>)
            measure(handle: Fd) -> (temperature: f32, pressure: f64)
            adjust(handle: Fd, delta_x: i8, delta_y: i16, delta_z: i32, delta_w: i64) -> ()
        }
    "#;
    let protocol = parse_idl("FileStore", input).unwrap();
    let iface = &protocol.interfaces["FileStore"];
    assert_eq!(iface.methods.len(), 8);

    assert_eq!(
        iface.methods[0].params,
        vec![
            Field {
                name: "data".to_string(),
                ty: Type::Vec(Box::new(Type::U8)),
                doc: None
            },
            Field {
                name: "name".to_string(),
                ty: Type::String,
                doc: None
            },
            Field {
                name: "executable".to_string(),
                ty: Type::Bool,
                doc: None
            },
        ]
    );
    assert_eq!(
        iface.methods[0].returns,
        Some(vec![Field {
            name: "handle".to_string(),
            ty: Type::Fd,
            doc: None
        }])
    );

    assert_eq!(
        iface.methods[1].params,
        vec![
            Field {
                name: "handle".to_string(),
                ty: Type::Fd,
                doc: None
            },
            Field {
                name: "offset".to_string(),
                ty: Type::U64,
                doc: None
            },
            Field {
                name: "length".to_string(),
                ty: Type::U32,
                doc: None
            },
        ]
    );
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![
            Field {
                name: "data".to_string(),
                ty: Type::Vec(Box::new(Type::U8)),
                doc: None
            },
            Field {
                name: "bytes_read".to_string(),
                ty: Type::U32,
                doc: None
            },
        ])
    );

    assert_eq!(
        iface.methods[2].returns,
        Some(vec![Field {
            name: "sha256".to_string(),
            ty: Type::Array(Box::new(Type::U8), 32),
            doc: None
        }])
    );

    assert_eq!(
        iface.methods[3].returns,
        Some(vec![
            Field {
                name: "size".to_string(),
                ty: Type::U64,
                doc: None
            },
            Field {
                name: "mode".to_string(),
                ty: Type::U16,
                doc: None
            },
            Field {
                name: "uid".to_string(),
                ty: Type::U32,
                doc: None
            },
            Field {
                name: "gid".to_string(),
                ty: Type::U32,
                doc: None
            },
        ])
    );

    assert_eq!(
        iface.methods[4].params[1],
        Field {
            name: "tags".to_string(),
            ty: Type::Set(Box::new(Type::String)),
            doc: None
        }
    );
    assert_eq!(iface.methods[4].returns, Some(vec![]));

    assert_eq!(
        iface.methods[5].returns,
        Some(vec![Field {
            name: "metadata".to_string(),
            ty: Type::Map(Box::new(Type::String), Box::new(Type::String)),
            doc: None,
        }])
    );

    assert_eq!(
        iface.methods[6].returns,
        Some(vec![
            Field {
                name: "temperature".to_string(),
                ty: Type::F32,
                doc: None
            },
            Field {
                name: "pressure".to_string(),
                ty: Type::F64,
                doc: None
            },
        ])
    );

    assert_eq!(
        iface.methods[7].params,
        vec![
            Field {
                name: "handle".to_string(),
                ty: Type::Fd,
                doc: None
            },
            Field {
                name: "delta_x".to_string(),
                ty: Type::I8,
                doc: None
            },
            Field {
                name: "delta_y".to_string(),
                ty: Type::I16,
                doc: None
            },
            Field {
                name: "delta_z".to_string(),
                ty: Type::I32,
                doc: None
            },
            Field {
                name: "delta_w".to_string(),
                ty: Type::I64,
                doc: None
            },
        ]
    );
    assert_eq!(iface.methods[7].returns, Some(vec![]));
}

#[test]
fn test_parse_ref_types() {
    let input = r#"
        /// Reference type tests
        interface RefTest {
            get_any() -> (obj: Ref)
            get_display(name: String) -> (display: Ref<Display>)
            bind(obj: Ref, iface: Ref<Compositor>) -> (bound: Ref)
        }
    "#;
    let protocol = parse_idl("RefTest", input).unwrap();
    let iface = &protocol.interfaces["RefTest"];

    assert_eq!(
        iface.methods[0].returns,
        Some(vec![Field {
            name: "obj".to_string(),
            ty: Type::Ref(None),
            doc: None
        }])
    );
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![Field {
            name: "display".to_string(),
            ty: Type::Ref(Some("Display".into())),
            doc: None
        }])
    );
    assert_eq!(
        iface.methods[2].params,
        vec![
            Field {
                name: "obj".to_string(),
                ty: Type::Ref(None),
                doc: None
            },
            Field {
                name: "iface".to_string(),
                ty: Type::Ref(Some("Compositor".into())),
                doc: None
            },
        ]
    );
    assert_eq!(
        iface.methods[2].returns,
        Some(vec![Field {
            name: "bound".to_string(),
            ty: Type::Ref(None),
            doc: None
        }])
    );
}

#[test]
fn test_parse_struct() {
    let input = r#"
        /// A 3D vector
        struct Vec3 {
            x: f32,
            y: f32,
            z: f32,
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let s = &protocol.structs["Vec3"];
    assert_eq!(s.name, "Vec3");
    assert_eq!(s.doc, "A 3D vector");
    assert_eq!(
        s.fields,
        vec![
            Field {
                name: "x".to_string(),
                ty: Type::F32,
                doc: None
            },
            Field {
                name: "y".to_string(),
                ty: Type::F32,
                doc: None
            },
            Field {
                name: "z".to_string(),
                ty: Type::F32,
                doc: None
            },
        ]
    );
}

#[test]
fn test_parse_enum() {
    let input = r#"
        /// Interaction methods
        enum Interaction {
            Pointer {
                origin: f32,
                direction: f32,
            },
            Hand {
                joints: u32,
            },
            Touch {
                point: f32,
            },
            None,
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let e = &protocol.enums["Interaction"];
    assert_eq!(e.name, "Interaction");
    assert_eq!(e.doc, "Interaction methods");
    assert_eq!(e.variants.len(), 4);

    assert_eq!(e.variants[0].name, "Pointer");
    assert_eq!(
        e.variants[0].fields,
        vec![
            Field {
                name: "origin".to_string(),
                ty: Type::F32,
                doc: None
            },
            Field {
                name: "direction".to_string(),
                ty: Type::F32,
                doc: None
            },
        ]
    );
    assert_eq!(e.variants[1].name, "Hand");
    assert_eq!(
        e.variants[1].fields,
        vec![Field {
            name: "joints".to_string(),
            ty: Type::U32,
            doc: None
        }]
    );
    assert_eq!(e.variants[2].name, "Touch");
    assert_eq!(
        e.variants[2].fields,
        vec![Field {
            name: "point".to_string(),
            ty: Type::F32,
            doc: None
        }]
    );
    assert_eq!(e.variants[3].name, "None");
    assert!(e.variants[3].fields.is_empty());
}

#[test]
fn test_parse_mixed() {
    let input = r#"
        /// A 3D vector
        struct Vec3 {
            x: f32,
            y: f32,
            z: f32,
        }

        /// Hit test result
        enum HitResult {
            Miss,
            Surface {
                point: Vec3,
                normal: Vec3,
                distance: f32,
            },
        }

        /// Spatial node interface
        interface Spatial {
            set_position(position: Vec3)
            interact(ray_origin: Vec3, ray_dir: Vec3) -> (hit: HitResult)
            get_child(name: String) -> (child: Ref<Spatial>)
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();

    assert!(protocol.structs.contains_key("Vec3"));
    assert!(protocol.enums.contains_key("HitResult"));
    assert!(protocol.interfaces.contains_key("Spatial"));

    let iface = &protocol.interfaces["Spatial"];
    assert_eq!(iface.methods.len(), 3);
    assert_eq!(
        iface.methods[0].params,
        vec![Field {
            name: "position".to_string(),
            ty: Type::Named("Vec3".into()),
            doc: None
        }]
    );
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![Field {
            name: "hit".to_string(),
            ty: Type::Named("HitResult".into()),
            doc: None
        }])
    );
    assert_eq!(
        iface.methods[2].returns,
        Some(vec![Field {
            name: "child".to_string(),
            ty: Type::Ref(Some("Spatial".into())),
            doc: None
        }])
    );

    let hit = &protocol.enums["HitResult"];
    assert_eq!(
        hit.variants[1].fields[0],
        Field {
            name: "point".to_string(),
            ty: Type::Named("Vec3".into()),
            doc: None
        }
    );
    assert_eq!(
        hit.variants[1].fields[1],
        Field {
            name: "normal".to_string(),
            ty: Type::Named("Vec3".into()),
            doc: None
        }
    );
}

#[test]
fn test_doc_comments() {
    let input = r#"
        /// Position in 3D space
        /// with x, y, z components
        struct Vec3 {
            x: f32,
            y: f32,
            z: f32,
        }

        /// Input interaction type
        enum InputType {
            None,
            Pointer {
                origin: Vec3,
                direction: Vec3,
            },
            Touch,
        }

        /// Spatial node
        interface Node {
            set_position(pos: Vec3)
            get_name() -> (name: String)
        }
    "#;
    let protocol = parse_idl("DocTest", input).unwrap();

    // Multi-line struct doc
    let s = &protocol.structs["Vec3"];
    assert_eq!(s.doc, "Position in 3D space\nwith x, y, z components");

    // Enum doc
    let e = &protocol.enums["InputType"];
    assert_eq!(e.doc, "Input interaction type");

    // Interface doc
    let iface = &protocol.interfaces["Node"];
    assert_eq!(iface.doc, "Spatial node");
}

#[test]
fn test_field_and_variant_docs() {
    let input = r#"
        /// A color
        struct Color {
            /// Red channel
            r: f32,
            /// Green channel
            g: f32,
            /// Blue channel
            b: f32,
            a: f32,
        }

        /// Paint types
        enum Paint {
            /// No paint
            None,
            /// Solid color fill
            Solid {
                /// The fill color
                color: Color,
            },
            Gradient {
                start: Color,
                end: Color,
            },
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();

    let s = &protocol.structs["Color"];
    assert_eq!(
        s.fields[0],
        Field {
            name: "r".to_string(),
            ty: Type::F32,
            doc: Some("Red channel".to_string())
        }
    );
    assert_eq!(
        s.fields[1],
        Field {
            name: "g".to_string(),
            ty: Type::F32,
            doc: Some("Green channel".to_string())
        }
    );
    assert_eq!(
        s.fields[2],
        Field {
            name: "b".to_string(),
            ty: Type::F32,
            doc: Some("Blue channel".to_string())
        }
    );
    assert_eq!(
        s.fields[3],
        Field {
            name: "a".to_string(),
            ty: Type::F32,
            doc: None
        }
    ); // no doc

    let e = &protocol.enums["Paint"];
    assert_eq!(e.variants[0].name, "None");
    assert_eq!(e.variants[0].doc, Some("No paint".to_string()));
    assert_eq!(e.variants[1].name, "Solid");
    assert_eq!(e.variants[1].doc, Some("Solid color fill".to_string()));
    assert_eq!(
        e.variants[1].fields[0],
        Field {
            name: "color".to_string(),
            ty: Type::Named("Color".into()),
            doc: Some("The fill color".to_string())
        }
    );
    assert_eq!(e.variants[2].name, "Gradient");
    assert_eq!(e.variants[2].doc, None); // no doc
    assert_eq!(
        e.variants[2].fields[0],
        Field {
            name: "start".to_string(),
            ty: Type::Named("Color".into()),
            doc: None
        }
    );
}

#[test]
fn test_missing_required_doc() {
    assert!(parse_idl("Test", "struct Foo { x: u32 }").is_err());
    assert!(parse_idl("Test", "enum Bar { A, B }").is_err());
    assert!(parse_idl("Test", "interface Baz { ping() -> () }").is_err());
}

/// Load a `.gluon` protocol file from disk, recursively resolving imports.
pub fn load_protocol(path: &Path) -> Result<Protocol, LoadError> {
    let mut seen = HashSet::new();
    let mut cache = HashMap::new();
    load_protocol_inner(path, &mut seen, &mut cache)
}

fn load_protocol_inner(
    path: &Path,
    seen: &mut HashSet<PathBuf>,
    cache: &mut HashMap<PathBuf, Protocol>,
) -> Result<Protocol, LoadError> {
    let canonical = path.canonicalize().map_err(LoadError::Io)?;

    if let Some(cached) = cache.get(&canonical) {
        return Ok(cached.clone());
    }

    if !seen.insert(canonical.clone()) {
        return Err(LoadError::CyclicImport(canonical));
    }

    let source = std::fs::read_to_string(&canonical).map_err(LoadError::Io)?;
    let name = default_alias(canonical.to_str().unwrap_or(""));
    let mut protocol = parse_idl(&name, &source).map_err(|errs| {
        let msgs: Vec<String> = errs.iter().map(|e| format!("{e}")).collect();
        LoadError::Parse(msgs.join("; "))
    })?;

    // Check for duplicate aliases
    let mut alias_set = HashSet::new();
    for import in &protocol.imports {
        if !alias_set.insert(import.alias.clone()) {
            return Err(LoadError::DuplicateAlias(import.alias.clone()));
        }
    }

    // Resolve imports
    let parent_dir = canonical.parent().unwrap_or(Path::new("."));
    for import in &protocol.imports {
        let import_path = parent_dir.join(&import.path);
        let imported = load_protocol_inner(&import_path, seen, cache)?;
        protocol
            .imported_protocols
            .insert(import.alias.clone(), imported);
    }

    seen.remove(&canonical);
    cache.insert(canonical, protocol.clone());
    Ok(protocol)
}

// --- Import system tests ---

#[test]
fn test_default_alias() {
    assert_eq!(default_alias("spatial.gluon"), "spatial");
    assert_eq!(default_alias("org.stardustxr.spatial.gluon"), "spatial");
    assert_eq!(default_alias("relative/path/types.gluon"), "types");
    assert_eq!(default_alias("a/b/org.stardustxr.spatial.gluon"), "spatial");
}

#[test]
fn test_parse_imports() {
    let input = r#"
        import "spatial.gluon"
        import "other.gluon" as myalias

        /// Test interface
        interface Foo {
            do_thing() -> ()
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    assert_eq!(protocol.imports.len(), 2);
    assert_eq!(protocol.imports[0].path, "spatial.gluon");
    assert_eq!(protocol.imports[0].alias, "spatial");
    assert_eq!(protocol.imports[1].path, "other.gluon");
    assert_eq!(protocol.imports[1].alias, "myalias");
}

#[test]
fn test_parse_qualified_type_in_params() {
    let input = r#"
        import "spatial.gluon"

        /// Test interface
        interface Foo {
            do_thing(pos: spatial::Vec3) -> (result: spatial::HitResult)
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let iface = &protocol.interfaces["Foo"];
    assert_eq!(
        iface.methods[0].params[0].ty,
        Type::Qualified("spatial".into(), "Vec3".into())
    );
    assert_eq!(
        iface.methods[0].returns.as_ref().unwrap()[0].ty,
        Type::Qualified("spatial".into(), "HitResult".into())
    );
}

#[test]
fn test_parse_qualified_type_in_containers() {
    let input = r#"
        import "spatial.gluon"

        /// Test interface
        interface Foo {
            get_all() -> (items: Vec<spatial::Vec3>)
            get_map() -> (m: Map<String, spatial::Vec3>)
            get_arr() -> (a: [spatial::Vec3; 4])
            get_ref() -> (r: Ref<spatial::Node>)
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let iface = &protocol.interfaces["Foo"];

    assert_eq!(
        iface.methods[0].returns.as_ref().unwrap()[0].ty,
        Type::Vec(Box::new(Type::Qualified("spatial".into(), "Vec3".into())))
    );
    assert_eq!(
        iface.methods[1].returns.as_ref().unwrap()[0].ty,
        Type::Map(
            Box::new(Type::String),
            Box::new(Type::Qualified("spatial".into(), "Vec3".into()))
        )
    );
    assert_eq!(
        iface.methods[2].returns.as_ref().unwrap()[0].ty,
        Type::Array(
            Box::new(Type::Qualified("spatial".into(), "Vec3".into())),
            4
        )
    );
    assert_eq!(
        iface.methods[3].returns.as_ref().unwrap()[0].ty,
        Type::Ref(Some("spatial::Node".into()))
    );
}

#[test]
fn test_parse_qualified_type_in_struct() {
    let input = r#"
        import "spatial.gluon"

        /// A transform
        struct Transform {
            position: spatial::Vec3,
            rotation: spatial::Quat,
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let s = &protocol.structs["Transform"];
    assert_eq!(
        s.fields[0].ty,
        Type::Qualified("spatial".into(), "Vec3".into())
    );
    assert_eq!(
        s.fields[1].ty,
        Type::Qualified("spatial".into(), "Quat".into())
    );
}

#[test]
fn test_parse_qualified_type_in_enum() {
    let input = r#"
        import "spatial.gluon"

        /// Hit result
        enum HitResult {
            Miss,
            Hit {
                point: spatial::Vec3,
            },
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    let e = &protocol.enums["HitResult"];
    assert_eq!(
        e.variants[1].fields[0].ty,
        Type::Qualified("spatial".into(), "Vec3".into())
    );
}

#[test]
fn test_load_protocol_basic() {
    use std::io::Write;
    let dir = tempfile::tempdir().unwrap();

    // Write spatial.gluon
    let spatial_path = dir.path().join("spatial.gluon");
    let mut f = std::fs::File::create(&spatial_path).unwrap();
    write!(
        f,
        "/// A 3D vector\nstruct Vec3 {{\n    x: f32,\n    y: f32,\n    z: f32,\n}}\n"
    )
    .unwrap();

    // Write main.gluon that imports spatial
    let main_path = dir.path().join("main.gluon");
    let mut f = std::fs::File::create(&main_path).unwrap();
    write!(
        f,
        "import \"spatial.gluon\"\n\n/// Main interface\ninterface Main {{\n    move_to(pos: spatial::Vec3)\n}}\n"
    )
    .unwrap();

    let protocol = load_protocol(&main_path).unwrap();
    assert_eq!(protocol.imports.len(), 1);
    assert_eq!(protocol.imports[0].alias, "spatial");
    assert!(protocol.imported_protocols.contains_key("spatial"));

    let spatial = &protocol.imported_protocols["spatial"];
    assert!(spatial.structs.contains_key("Vec3"));

    let iface = &protocol.interfaces["Main"];
    assert_eq!(
        iface.methods[0].params[0].ty,
        Type::Qualified("spatial".into(), "Vec3".into())
    );
}

#[test]
fn test_load_protocol_cyclic_import() {
    use std::io::Write;
    let dir = tempfile::tempdir().unwrap();

    let a_path = dir.path().join("a.gluon");
    let b_path = dir.path().join("b.gluon");

    let mut f = std::fs::File::create(&a_path).unwrap();
    write!(
        f,
        "import \"b.gluon\"\n\n/// A\ninterface A {{\n    ping() -> ()\n}}\n"
    )
    .unwrap();

    let mut f = std::fs::File::create(&b_path).unwrap();
    write!(
        f,
        "import \"a.gluon\"\n\n/// B\ninterface B {{\n    pong() -> ()\n}}\n"
    )
    .unwrap();

    let result = load_protocol(&a_path);
    assert!(matches!(result, Err(LoadError::CyclicImport(_))));
}

#[test]
fn test_load_protocol_file_not_found() {
    let result = load_protocol(Path::new("/nonexistent/path/test.gluon"));
    assert!(matches!(result, Err(LoadError::Io(_))));
}

#[test]
fn test_load_protocol_duplicate_alias() {
    use std::io::Write;
    let dir = tempfile::tempdir().unwrap();

    let types_path = dir.path().join("types.gluon");
    let mut f = std::fs::File::create(&types_path).unwrap();
    write!(f, "/// A type\nstruct Foo {{\n    x: u32,\n}}\n").unwrap();

    let other_dir = dir.path().join("sub");
    std::fs::create_dir(&other_dir).unwrap();
    let other_types_path = other_dir.join("types.gluon");
    let mut f = std::fs::File::create(&other_types_path).unwrap();
    write!(f, "/// Another type\nstruct Bar {{\n    y: u32,\n}}\n").unwrap();

    let main_path = dir.path().join("main.gluon");
    let mut f = std::fs::File::create(&main_path).unwrap();
    write!(
        f,
        "import \"types.gluon\"\nimport \"sub/types.gluon\"\n\n/// Main\ninterface Main {{\n    ping() -> ()\n}}\n"
    )
    .unwrap();

    let result = load_protocol(&main_path);
    assert!(matches!(result, Err(LoadError::DuplicateAlias(_))));
}

#[test]
fn test_load_protocol_diamond_import() {
    use std::io::Write;
    let dir = tempfile::tempdir().unwrap();

    // shared.gluon
    let shared_path = dir.path().join("shared.gluon");
    let mut f = std::fs::File::create(&shared_path).unwrap();
    write!(
        f,
        "/// Shared type\nstruct Vec3 {{\n    x: f32,\n    y: f32,\n    z: f32,\n}}\n"
    )
    .unwrap();

    // a.gluon imports shared
    let a_path = dir.path().join("a.gluon");
    let mut f = std::fs::File::create(&a_path).unwrap();
    write!(f, "import \"shared.gluon\"\n\n/// A\ninterface A {{\n    get_pos() -> (pos: shared::Vec3)\n}}\n").unwrap();

    // b.gluon imports shared
    let b_path = dir.path().join("b.gluon");
    let mut f = std::fs::File::create(&b_path).unwrap();
    write!(
        f,
        "import \"shared.gluon\"\n\n/// B\ninterface B {{\n    set_pos(pos: shared::Vec3)\n}}\n"
    )
    .unwrap();

    // main.gluon imports both a and b
    let main_path = dir.path().join("main.gluon");
    let mut f = std::fs::File::create(&main_path).unwrap();
    write!(f, "import \"a.gluon\"\nimport \"b.gluon\"\n\n/// Main\ninterface Main {{\n    ping() -> ()\n}}\n").unwrap();

    let protocol = load_protocol(&main_path).unwrap();
    assert!(protocol.imported_protocols.contains_key("a"));
    assert!(protocol.imported_protocols.contains_key("b"));

    // Both a and b should have resolved their shared import
    let a = &protocol.imported_protocols["a"];
    assert!(a.imported_protocols.contains_key("shared"));
    let b = &protocol.imported_protocols["b"];
    assert!(b.imported_protocols.contains_key("shared"));
}

#[test]
fn test_parse_no_imports() {
    let input = r#"
        /// Test interface
        interface Foo {
            ping() -> ()
        }
    "#;
    let protocol = parse_idl("Test", input).unwrap();
    assert!(protocol.imports.is_empty());
    assert!(protocol.imported_protocols.is_empty());
}

#[test]
fn test_parse_real_interface() {
    let input = include_str!("../gluon/org.stardustxr.gluon.test.gluon");
    let protocol = parse_idl("test", input).unwrap();
    assert!(protocol.imports.is_empty());
    assert!(protocol.imported_protocols.is_empty());
}
