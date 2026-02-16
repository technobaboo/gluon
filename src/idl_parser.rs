use chumsky::{prelude::*, text::keyword};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Protocol {
    name: String,
    interfaces: HashMap<String, Interface>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    methods: Vec<Method>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    name: String,
    params: Vec<(String, Type)>,
    /// if none, this is a oneway function.
    /// if some but vec is empty, this is a void function.
    returns: Option<Vec<(String, Type)>>,
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
    Object(Option<String>), // optional interface name
    Array(Box<Type>, u32),
    Vec(Box<Type>),
    Set(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Protocol, extra::Err<Rich<'src, char>>> {
    let interface_name = text::ident()
        .map(str::to_string)
        .padded()
        .labelled("interface");
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
            just("Object").to(Type::Object(None)),
            text::ident().map(|s: &str| Type::Object(Some(s.to_string()))),
        ))
        .padded()
        .labelled("type")
    });
    let param = text::ident()
        .map(str::to_string)
        .then_ignore(just(':').padded())
        .then(type_parser.clone());
    let params = param
        .padded()
        .separated_by(just(','))
        .collect::<Vec<(String, Type)>>()
        .delimited_by(just('('), just(')'))
        .padded();
    let returns = just("->").padded().ignore_then(params.clone()).or_not();
    let comment = just("//").then(none_of("\n\r").repeated()).or_not();
    let method = method_name
        .then(params)
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
        .collect::<Vec<Method>>()
        .map(|methods| Interface { methods });
    let interface_inner = methods.delimited_by(just('{'), just('}')).padded();
    let interface = keyword("interface")
        .padded()
        .ignore_then(interface_name)
        .then(interface_inner);

    interface
        .padded()
        .repeated()
        .collect::<HashMap<String, Interface>>()
        .then_ignore(end())
        .map(|interfaces| Protocol {
            name: "".to_string(),
            interfaces,
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
                            params: vec![("input".to_string(), Type::String)],
                            returns: Some(vec![("output".to_string(), Type::String)]),
                        },
                    ],
                }
            ),]),
        }
    );
}

#[test]
fn test_parse_types() {
    let input = r#"
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

    // upload: Vec<u8>, String, bool -> Fd
    assert_eq!(
        iface.methods[0].params,
        vec![
            ("data".into(), Type::Vec(Box::new(Type::U8))),
            ("name".into(), Type::String),
            ("executable".into(), Type::Bool),
        ]
    );
    assert_eq!(
        iface.methods[0].returns,
        Some(vec![("handle".into(), Type::Fd)])
    );

    // read: Fd, u64, u32 -> Vec<u8>, u32
    assert_eq!(
        iface.methods[1].params,
        vec![
            ("handle".into(), Type::Fd),
            ("offset".into(), Type::U64),
            ("length".into(), Type::U32),
        ]
    );
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![
            ("data".into(), Type::Vec(Box::new(Type::U8))),
            ("bytes_read".into(), Type::U32),
        ])
    );

    // hash: Fd -> [u8; 32]
    assert_eq!(
        iface.methods[2].returns,
        Some(vec![("sha256".into(), Type::Array(Box::new(Type::U8), 32)),])
    );

    // stat: Fd -> u64, u16, u32, u32
    assert_eq!(
        iface.methods[3].returns,
        Some(vec![
            ("size".into(), Type::U64),
            ("mode".into(), Type::U16),
            ("uid".into(), Type::U32),
            ("gid".into(), Type::U32),
        ])
    );

    // set_tags: Fd, Set<String> -> ()
    assert_eq!(
        iface.methods[4].params[1],
        ("tags".into(), Type::Set(Box::new(Type::String)))
    );
    assert_eq!(iface.methods[4].returns, Some(vec![]));

    // get_metadata: Fd -> Map<String, String>
    assert_eq!(
        iface.methods[5].returns,
        Some(vec![(
            "metadata".into(),
            Type::Map(Box::new(Type::String), Box::new(Type::String))
        ),])
    );

    // measure: Fd -> f32, f64
    assert_eq!(
        iface.methods[6].returns,
        Some(vec![
            ("temperature".into(), Type::F32),
            ("pressure".into(), Type::F64),
        ])
    );

    // adjust: i8, i16, i32, i64 -> ()
    assert_eq!(
        iface.methods[7].params,
        vec![
            ("handle".into(), Type::Fd),
            ("delta_x".into(), Type::I8),
            ("delta_y".into(), Type::I16),
            ("delta_z".into(), Type::I32),
            ("delta_w".into(), Type::I64),
        ]
    );
    assert_eq!(iface.methods[7].returns, Some(vec![]));
}

#[test]
fn test_parse_object_types() {
    let input = r#"
        interface ObjectTest {
            get_any() -> (obj: Object)
            get_display(name: String) -> (display: Display)
            bind(obj: Object, iface: Compositor) -> (bound: Object)
        }
    "#;
    let protocol = parse_idl("ObjectTest", input).unwrap();
    let iface = &protocol.interfaces["ObjectTest"];

    // get_any: -> Object (untyped)
    assert_eq!(
        iface.methods[0].returns,
        Some(vec![("obj".into(), Type::Object(None))])
    );

    // get_display: String -> Display (named object)
    assert_eq!(
        iface.methods[1].returns,
        Some(vec![("display".into(), Type::Object(Some("Display".into())))])
    );

    // bind: Object, Compositor -> Object
    assert_eq!(
        iface.methods[2].params,
        vec![
            ("obj".into(), Type::Object(None)),
            ("iface".into(), Type::Object(Some("Compositor".into()))),
        ]
    );
    assert_eq!(
        iface.methods[2].returns,
        Some(vec![("bound".into(), Type::Object(None))])
    );
}
