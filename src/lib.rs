use std::ops::{BitAnd, BitOr, BitXor, Not};
use std::cmp::{PartialEq, Eq};

/// Integer byte order
pub trait ByteOrder {}

/// Big‐endian byte order
///
/// Most significant byte first
pub struct BigEndian;
impl ByteOrder for BigEndian {}

/// Little‐endian byte order
///
/// Least significant byte first
pub struct LittleEndian;
impl ByteOrder for LittleEndian {}

/// Byte order‐limited integer
#[derive(Copy, Clone)]
pub struct Endian<T: BitAnd + BitOr + BitXor + Not + Eq, O: ByteOrder>(T, O);

macro_rules! endian {
	($int:ty, $order:ty) => (
		impl BitAnd for Endian<$int, $order> {
			type Output = Endian<<$int as BitAnd>::Output, $order>;

			fn bitand(self, rhs: Self) -> Self::Output {
				let Endian(lhs_value, lhs_order) = self;
				let Endian(rhs_value, _) = rhs;
				Endian(lhs_value & rhs_value, lhs_order)
			}
		}

		impl BitOr for Endian<$int, $order> {
			type Output = Endian<<$int as BitOr>::Output, $order>;

			fn bitor(self, rhs: Self) -> Self::Output {
				let Endian(lhs_value, lhs_order) = self;
				let Endian(rhs_value, _) = rhs;
				Endian(lhs_value | rhs_value, lhs_order)
			}
		}

		impl BitXor for Endian<$int, $order> {
			type Output = Endian<<$int as BitXor>::Output, $order>;

			fn bitxor(self, rhs: Self) -> Self::Output {
				let Endian(lhs_value, lhs_order) = self;
				let Endian(rhs_value, _) = rhs;
				Endian(lhs_value ^ rhs_value, lhs_order)
			}
		}

		impl Not for Endian<$int, $order> {
			type Output = Endian<<$int as Not>::Output, $order>;

			fn not(self) -> Self::Output {
				let Endian(lhs_value, lhs_order) = self;
				Endian(!lhs_value, lhs_order)
			}
		}

		impl PartialEq for Endian<$int, $order> {
			fn eq(&self, rhs: &Self) -> bool {
				let Endian(lhs_value, _) = *self;
				let Endian(rhs_value, _) = *rhs;
				lhs_value == rhs_value
			}

			fn ne(&self, rhs: &Self) -> bool {
				let Endian(lhs_value, _) = *self;
				let Endian(rhs_value, _) = *rhs;
				lhs_value != rhs_value
			}
		}

		impl Eq for Endian<$int, $order> {}
	);
}

macro_rules! endian_be {
	($int:ident) => (
		impl Endian<$int, BigEndian> {
			/// Constructs new byte order‐limited integer
			pub fn new(value: $int) -> Self {
				Endian(value.to_be(), BigEndian)
			}

			/// Converts byte‐order limited integer to native integer
			pub fn to_native(self) -> $int {
				let Endian(value, _) = self;
				$int::from_be(value)
			}
		}

		endian!($int, BigEndian);
	);
}

macro_rules! endian_le {
	($int:ident) => (
		impl Endian<$int, LittleEndian> {
			/// Constructs new byte order‐limited integer
			pub fn new(value: $int) -> Self {
				Endian(value.to_le(), LittleEndian)
			}

			/// Converts byte‐order limited integer to native integer
			pub fn to_native(self) -> $int {
				let Endian(value, _) = self;
				$int::from_le(value)
			}
		}

		endian!($int, LittleEndian);
	);
}

endian_be!(u16);
endian_be!(u32);
endian_be!(u64);

endian_le!(u16);
endian_le!(u32);
endian_le!(u64);

/// Short type aliases
pub mod types {
	use {Endian, BigEndian, LittleEndian};

	#[allow(non_camel_case_types)]
	/// Big‐endian unsigned 16 bit integer
	pub type be16 = Endian<u16, BigEndian>;

	#[allow(non_camel_case_types)]
	/// Big‐endian unsigned 32 bit integer
	pub type be32 = Endian<u32, BigEndian>;

	#[allow(non_camel_case_types)]
	/// Big‐endian unsigned 64 bit integer
	pub type be64 = Endian<u64, BigEndian>;

	#[allow(non_camel_case_types)]
	/// Little‐endian unsigned 16 bit integer
	pub type le16 = Endian<u16, LittleEndian>;

	#[allow(non_camel_case_types)]
	/// Little‐endian unsigned 32 bit integer
	pub type le32 = Endian<u32, LittleEndian>;

	#[allow(non_camel_case_types)]
	/// Little‐endian unsigned 64 bit integer
	pub type le64 = Endian<u64, LittleEndian>;
}

#[cfg(test)]
mod tests {
	use std::mem::transmute;
	use types::*;

	#[test]
	fn be16_invert_static() {
		let int = 0xc0de_u16;
		let be = be16::new(int);
		assert_eq!(int, be.to_native());
	}

	#[test]
	fn be32_invert_static() {
		let int = 0xdeadbeef_u32;
		let be = be32::new(int);
		assert_eq!(int, be.to_native());
	}

	#[test]
	fn be64_invert_static() {
		let int = 0xcafec0dedeadbeef_u64;
		let be = be64::new(int);
		assert_eq!(int, be.to_native());
	}

	#[test]
	fn le16_invert_static() {
		let int = 0xc0de_u16;
		let le = le16::new(int);
		assert_eq!(int, le.to_native());
	}

	#[test]
	fn le32_invert_static() {
		let int = 0xdeadbeef_u32;
		let le = le32::new(int);
		assert_eq!(int, le.to_native());
	}

	#[test]
	fn le64_invert_static() {
		let int = 0xcafec0dedeadbeef_u64;
		let le = le64::new(int);
		assert_eq!(int, le.to_native());
	}

	#[test]
	fn be16_inspect_static() {
		let int = 0xc0de_u16;
		let be = be16::new(int);
		let b: &[u8; 2] = unsafe { transmute(&be) };
		assert_eq!(b[0], 0xc0_u8);
		assert_eq!(b[1], 0xde_u8);
	}

	#[test]
	fn be32_inspect_static() {
		let int = 0xdeadbeef_u32;
		let be = be32::new(int);
		let b: &[u8; 4] = unsafe { transmute(&be) };
		assert_eq!(b[0], 0xde_u8);
		assert_eq!(b[1], 0xad_u8);
		assert_eq!(b[2], 0xbe_u8);
		assert_eq!(b[3], 0xef_u8);
	}

	#[test]
	fn be64_inspect_static() {
		let int = 0xcafec0dedeadbeef_u64;
		let be = be64::new(int);
		let b: &[u8; 8] = unsafe { transmute(&be) };
		assert_eq!(b[0], 0xca_u8);
		assert_eq!(b[1], 0xfe_u8);
		assert_eq!(b[2], 0xc0_u8);
		assert_eq!(b[3], 0xde_u8);
		assert_eq!(b[4], 0xde_u8);
		assert_eq!(b[5], 0xad_u8);
		assert_eq!(b[6], 0xbe_u8);
		assert_eq!(b[7], 0xef_u8);
	}

	#[test]
	fn le16_inspect_static() {
		let int = 0xc0de_u16;
		let le = le16::new(int);
		let b: &[u8; 2] = unsafe { transmute(&le) };
		assert_eq!(b[0], 0xde_u8);
		assert_eq!(b[1], 0xc0_u8);
	}

	#[test]
	fn le32_inspect_static() {
		let int = 0xdeadbeef_u32;
		let le = le32::new(int);
		let b: &[u8; 4] = unsafe { transmute(&le) };
		assert_eq!(b[0], 0xef_u8);
		assert_eq!(b[1], 0xbe_u8);
		assert_eq!(b[2], 0xad_u8);
		assert_eq!(b[3], 0xde_u8);
	}

	#[test]
	fn le64_inspect_static() {
		let int = 0xcafec0dedeadbeef_u64;
		let le = le64::new(int);
		let b: &[u8; 8] = unsafe { transmute(&le) };
		assert_eq!(b[0], 0xef_u8);
		assert_eq!(b[1], 0xbe_u8);
		assert_eq!(b[2], 0xad_u8);
		assert_eq!(b[3], 0xde_u8);
		assert_eq!(b[4], 0xde_u8);
		assert_eq!(b[5], 0xc0_u8);
		assert_eq!(b[6], 0xfe_u8);
		assert_eq!(b[7], 0xca_u8);
	}
}
