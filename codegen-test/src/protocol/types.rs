#![allow(unused, clippy::all, private_bounds, private_interfaces)]
use gluon::Convertable;
pub const EXTERNAL_PROTOCOL: gluon::ExternalProtocol = gluon::ExternalProtocol {
    protocol_name: "org.gluon.Types",
    types: &[
        gluon::ExternalGluonType {
            name: "Vec3",
            supported_derives: gluon::Derives::from_bits_truncate(171u32),
            proxy: Some("proxies::MyVec3"),
        },
    ],
};
pub mod proxies {
    use super::*;
    pub use crate::MyVec3;
}
pub mod proxied {
    use super::*;
    ///3D vector
    #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Default)]
    pub struct Vec3 {
        pub x: f32,
        pub y: f32,
        pub z: f32,
    }
    impl gluon::Convertable for Vec3 {
        fn write<'a, 'b: 'a>(
            &'b self,
            gluon_data: &mut gluon::DataBuilder<'a>,
        ) -> Result<(), gluon::WriteError> {
            self.x.write(gluon_data)?;
            self.y.write(gluon_data)?;
            self.z.write(gluon_data)?;
            Ok(())
        }
        fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
            let x = gluon::Convertable::read(gluon_data)?;
            let y = gluon::Convertable::read(gluon_data)?;
            let z = gluon::Convertable::read(gluon_data)?;
            Ok(Vec3 { x, y, z })
        }
        fn write_owned(
            self,
            gluon_data: &mut gluon::DataBuilder<'_>,
        ) -> Result<(), gluon::WriteError> {
            self.x.write_owned(gluon_data)?;
            self.y.write_owned(gluon_data)?;
            self.z.write_owned(gluon_data)?;
            Ok(())
        }
    }
}
