use std::os::fd::{AsFd, OwnedFd};

use binderbinder::binder_object::BinderObjectOrRef;

use crate::{GluonConvertable, GluonReadError, GluonWriteError};

impl<T: GluonConvertable> GluonConvertable for Vec<T> {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_u32(
            self.len()
                .try_into()
                .map_err(|_| GluonWriteError::ListToLong)?,
        )?;
        for v in self.iter() {
            v.write(data)?;
        }
        Ok(())
    }

    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_u32(
            self.len()
                .try_into()
                .map_err(|_| GluonWriteError::ListToLong)?,
        )?;
        for v in self.into_iter() {
            v.write_owned(data)?;
        }
        Ok(())
    }

    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        let len = data.read_u32()?;
        let mut out = Vec::with_capacity(len as usize);
        for _ in 0..len {
            out.push(T::read(data)?);
        }
        Ok(out)
    }
}
impl<T: GluonConvertable> GluonConvertable for Option<T> {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_bool(self.is_some())?;
        if let Some(v) = self {
            v.write(data)?;
        }
        Ok(())
    }

    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_bool(self.is_some())?;
        if let Some(v) = self {
            v.write_owned(data)?;
        }
        Ok(())
    }

    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_bool()?.then(|| T::read(data)).transpose()
    }
}

impl GluonConvertable for BinderObjectOrRef {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_binder(self)
    }

    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_binder()
    }

    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_binder(&self)
    }
}
impl GluonConvertable for OwnedFd {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_fd(self.as_fd())
    }

    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_fd()
    }

    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_owned_fd(self)
    }
}
impl GluonConvertable for String {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_str(self)
    }

    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_string()
    }

    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_str(&self)
    }
}

impl GluonConvertable for bool {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_bool(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_bool()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_bool(self)
    }
}
impl GluonConvertable for u64 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_u64(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_u64()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_u64(self)
    }
}
impl GluonConvertable for i64 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_i64(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_i64()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_i64(self)
    }
}
impl GluonConvertable for f64 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_f64(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_f64()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_f64(self)
    }
}
impl GluonConvertable for u32 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_u32(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_u32()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_u32(self)
    }
}
impl GluonConvertable for i32 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_i32(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_i32()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_i32(self)
    }
}
impl GluonConvertable for f32 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_f32(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_f32()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_f32(self)
    }
}
impl GluonConvertable for u16 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_u16(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_u16()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_u16(self)
    }
}
impl GluonConvertable for i16 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_i16(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_i16()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_i16(self)
    }
}
impl GluonConvertable for u8 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_u8(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_u8()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_u8(self)
    }
}
impl GluonConvertable for i8 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut crate::GluonDataBuilder<'a>,
    ) -> Result<(), GluonWriteError> {
        data.write_i8(*self)
    }
    fn read(data: &mut crate::GluonDataReader) -> Result<Self, GluonReadError> {
        data.read_i8()
    }
    fn write_owned(self, data: &mut crate::GluonDataBuilder<'_>) -> Result<(), GluonWriteError> {
        data.write_i8(self)
    }
}
