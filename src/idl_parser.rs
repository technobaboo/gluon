use chumsky::{prelude::*, text::keyword};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Protocol {
    name: String,
    interfaces: HashMap<String, Interface>,
    structs: HashMap<String, StructDef>,
    enums: HashMap<String, EnumDef>,
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
    Ref(Option<String>),    // binder object reference
    Named(String),          // reference to a struct or enum by name
    Array(Box<Type>, u32),
    Vec(Box<Type>),
    Set(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Protocol, extra::Err<Rich<'src, char>>> {
    // --- Doc comment parser ---
    // A single `/// text` line, returns the trimmed text content
    let doc_line = one_of(" \t")
        .repeated()
        .ignore_then(just("///"))
        .ignore_then(
            none_of("\n\r")
                .repeated()
                .to_slice()
                .map(|s: &str| {
                    let s = s.strip_prefix(' ').unwrap_or(s);
                    s.trim_end().to_string()
                }),
        )
        .then_ignore(text::newline());

    // Required doc block: one or more `///` lines, joined by newline
    let req_doc_block = doc_line
        .clone()
        .repeated()
        .at_least(1)
        .collect::<Vec<std::string::String>>()
        .map(|lines| lines.join("\n"))
        .labelled("doc comment (/// ...)");

    // Optional doc block: zero or more `///` lines
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

    // Regular comment: `//` NOT followed by `/`
    let comment = just("//")
        .then(
            none_of("/\n\r")
                .then(none_of("\n\r").repeated())
                .or_not(),
        )
        .or_not();

    // --- Type parser ---
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
                        .map(|s: &str| Some(s.to_string()))
                        .delimited_by(just('<').padded(), just('>').padded())
                        .or_not()
                        .map(|opt| opt.flatten()),
                )
                .map(|(_, name)| Type::Ref(name)),
            text::ident().map(|s: &str| Type::Named(s.to_string())),
        ))
        .padded()
        .labelled("type")
    });

    // --- Params ---
    let param = text::ident()
        .map(str::to_string)
        .then_ignore(just(':').padded())
        .then(type_parser.clone())
        .map(|(name, ty)| Field { name, ty, doc: None });
    let params = param
        .padded()
        .separated_by(just(','))
        .collect::<Vec<Field>>()
        .delimited_by(just('('), just(')'))
        .padded();
    let returns = just("->").padded().ignore_then(params.clone()).or_not();

    // --- Method ---
    let method = method_name
        .then(params.clone())
        .then(returns)
        .map(|((name, params), returns)| Method {
            name,
            params,
            returns,
        })
        .then_ignore(comment);
    let methods = method
        .padded()
        .repeated()
        .collect::<Vec<Method>>();

    // --- Interface ---
    let interface = req_doc_block
        .clone()
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
    let struct_field = opt_doc_block
        .clone()
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
    let struct_def = req_doc_block
        .clone()
        .then(
            one_of(" \t")
                .repeated()
                .ignore_then(keyword("struct"))
                .padded()
                .ignore_then(text::ident().map(str::to_string).padded())
                .then(struct_fields.clone().delimited_by(just('{').padded(), just('}').padded())),
        )
        .map(|(doc, (name, fields))| StructDef { name, doc, fields });

    // --- Enum ---
    let variant_fields = struct_fields
        .delimited_by(just('{').padded(), just('}').padded());
    let enum_variant = opt_doc_block
        .clone()
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
    let enum_def = req_doc_block
        .clone()
        .then(
            one_of(" \t")
                .repeated()
                .ignore_then(keyword("enum"))
                .padded()
                .ignore_then(text::ident().map(str::to_string).padded())
                .then(enum_variants.delimited_by(just('{').padded(), just('}').padded())),
        )
        .map(|(doc, (name, variants))| EnumDef { name, doc, variants });

    // --- Top-level ---
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

    top_level_item
        .padded()
        .repeated()
        .collect::<Vec<TopLevel>>()
        .then_ignore(end())
        .map(|items| {
            let mut interfaces = HashMap::new();
            let mut structs = HashMap::new();
            let mut enums = HashMap::new();
            for item in items {
                match item {
                    TopLevel::Interface(name, iface) => { interfaces.insert(name, iface); }
                    TopLevel::Struct(s) => { structs.insert(s.name.clone(), s); }
                    TopLevel::Enum(e) => { enums.insert(e.name.clone(), e); }
                }
            }
            Protocol {
                name: "".to_string(),
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

fn f(name: &str, ty: Type) -> Field {
    Field { name: name.to_string(), ty, doc: None }
}

fn fd(name: &str, ty: Type, doc: &str) -> Field {
    Field { name: name.to_string(), ty, doc: Some(doc.to_string()) }
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
                            params: vec![f("input", Type::String)],
                            returns: Some(vec![f("output", Type::String)]),
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
            f("data", Type::Vec(Box::new(Type::U8))),
            f("name", Type::String),
            f("executable", Type::Bool),
        ]
    );
    assert_eq!(iface.methods[0].returns, Some(vec![f("handle", Type::Fd)]));

    assert_eq!(
        iface.methods[1].params,
        vec![
            f("handle", Type::Fd),
            f("offset", Type::U64),
            f("length", Type::U32),
        ]
    );
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![
            f("data", Type::Vec(Box::new(Type::U8))),
            f("bytes_read", Type::U32),
        ])
    );

    assert_eq!(
        iface.methods[2].returns,
        Some(vec![f("sha256", Type::Array(Box::new(Type::U8), 32))])
    );

    assert_eq!(
        iface.methods[3].returns,
        Some(vec![
            f("size", Type::U64),
            f("mode", Type::U16),
            f("uid", Type::U32),
            f("gid", Type::U32),
        ])
    );

    assert_eq!(iface.methods[4].params[1], f("tags", Type::Set(Box::new(Type::String))));
    assert_eq!(iface.methods[4].returns, Some(vec![]));

    assert_eq!(
        iface.methods[5].returns,
        Some(vec![f("metadata", Type::Map(Box::new(Type::String), Box::new(Type::String)))])
    );

    assert_eq!(
        iface.methods[6].returns,
        Some(vec![f("temperature", Type::F32), f("pressure", Type::F64)])
    );

    assert_eq!(
        iface.methods[7].params,
        vec![
            f("handle", Type::Fd),
            f("delta_x", Type::I8),
            f("delta_y", Type::I16),
            f("delta_z", Type::I32),
            f("delta_w", Type::I64),
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

    assert_eq!(iface.methods[0].returns, Some(vec![f("obj", Type::Ref(None))]));
    assert_eq!(iface.methods[1].returns, Some(vec![f("display", Type::Ref(Some("Display".into())))]));
    assert_eq!(
        iface.methods[2].params,
        vec![f("obj", Type::Ref(None)), f("iface", Type::Ref(Some("Compositor".into())))]
    );
    assert_eq!(iface.methods[2].returns, Some(vec![f("bound", Type::Ref(None))]));
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
    assert_eq!(s.fields, vec![f("x", Type::F32), f("y", Type::F32), f("z", Type::F32)]);
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
    assert_eq!(e.variants[0].fields, vec![f("origin", Type::F32), f("direction", Type::F32)]);
    assert_eq!(e.variants[1].name, "Hand");
    assert_eq!(e.variants[1].fields, vec![f("joints", Type::U32)]);
    assert_eq!(e.variants[2].name, "Touch");
    assert_eq!(e.variants[2].fields, vec![f("point", Type::F32)]);
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
    assert_eq!(iface.methods[0].params, vec![f("position", Type::Named("Vec3".into()))]);
    assert_eq!(iface.methods[1].returns, Some(vec![f("hit", Type::Named("HitResult".into()))]));
    assert_eq!(iface.methods[2].returns, Some(vec![f("child", Type::Ref(Some("Spatial".into())))]));

    let hit = &protocol.enums["HitResult"];
    assert_eq!(hit.variants[1].fields[0], f("point", Type::Named("Vec3".into())));
    assert_eq!(hit.variants[1].fields[1], f("normal", Type::Named("Vec3".into())));
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
    assert_eq!(s.fields[0], fd("r", Type::F32, "Red channel"));
    assert_eq!(s.fields[1], fd("g", Type::F32, "Green channel"));
    assert_eq!(s.fields[2], fd("b", Type::F32, "Blue channel"));
    assert_eq!(s.fields[3], f("a", Type::F32)); // no doc

    let e = &protocol.enums["Paint"];
    assert_eq!(e.variants[0].name, "None");
    assert_eq!(e.variants[0].doc, Some("No paint".to_string()));
    assert_eq!(e.variants[1].name, "Solid");
    assert_eq!(e.variants[1].doc, Some("Solid color fill".to_string()));
    assert_eq!(e.variants[1].fields[0], fd("color", Type::Named("Color".into()), "The fill color"));
    assert_eq!(e.variants[2].name, "Gradient");
    assert_eq!(e.variants[2].doc, None); // no doc
    assert_eq!(e.variants[2].fields[0], f("start", Type::Named("Color".into())));
}

#[test]
fn test_missing_required_doc() {
    assert!(parse_idl("Test", "struct Foo { x: u32 }").is_err());
    assert!(parse_idl("Test", "enum Bar { A, B }").is_err());
    assert!(parse_idl("Test", "interface Baz { ping() -> () }").is_err());
}
